//! Net effects — minimal HTTP server for the Loon dev server.
//!
//! Provides: Net.http-serve, Net.respond, Net.sse-open, Net.sse-send

use super::builtins::{create_shared_channel, shared_send};
use super::value::Value;
use super::{err, InterpError, PerformedEffect};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Mutex, OnceLock};
use std::sync::atomic::{AtomicU32, Ordering};

type IResult = Result<Value, InterpError>;

// --- Global connection storage ---

/// Active TCP connections indexed by request id.
fn connections() -> &'static Mutex<HashMap<u32, TcpStream>> {
    static INSTANCE: OnceLock<Mutex<HashMap<u32, TcpStream>>> = OnceLock::new();
    INSTANCE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn next_conn_id() -> &'static AtomicU32 {
    static INSTANCE: OnceLock<AtomicU32> = OnceLock::new();
    INSTANCE.get_or_init(|| AtomicU32::new(1))
}

/// Active SSE streams indexed by stream id.
fn sse_streams() -> &'static Mutex<HashMap<u32, TcpStream>> {
    static INSTANCE: OnceLock<Mutex<HashMap<u32, TcpStream>>> = OnceLock::new();
    INSTANCE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn next_sse_id() -> &'static AtomicU32 {
    static INSTANCE: OnceLock<AtomicU32> = OnceLock::new();
    INSTANCE.get_or_init(|| AtomicU32::new(1))
}

/// Try to handle a Net.* effect.
pub fn try_net_handler(performed: &PerformedEffect) -> Option<IResult> {
    if performed.effect != "Net" {
        return None;
    }
    match performed.operation.as_str() {
        "http-serve" => Some(http_serve(&performed.args)),
        "respond" => Some(http_respond(&performed.args)),
        "serve-file" => Some(serve_file(&performed.args)),
        "sse-open" => Some(sse_open(&performed.args)),
        "sse-send" => Some(sse_send(&performed.args)),
        "sse-broadcast" => Some(sse_broadcast(&performed.args)),
        _ => None,
    }
}

/// `Net.http-serve port` → `(tx, rx)` where requests arrive as maps on the channel.
///
/// Each request is a map: `{:id Int :method Str :path Str :headers Map}`
/// The request id is used with `Net.respond` to send a response.
fn http_serve(args: &[Value]) -> IResult {
    let port = match args.first() {
        Some(Value::Int(p)) => *p as u16,
        _ => return Err(err("Net.http-serve requires an integer port")),
    };

    let (tx_id, rx_id) = create_shared_channel();

    let listener = TcpListener::bind(format!("0.0.0.0:{port}"))
        .map_err(|e| err(format!("Net.http-serve: {e}")))?;

    // Spawn acceptor thread
    std::thread::spawn(move || {
        for stream in listener.incoming() {
            let stream = match stream {
                Ok(s) => s,
                Err(_) => continue,
            };

            match parse_request(&stream) {
                Ok(req) => {
                    // Store the connection for later response
                    let conn_id = next_conn_id().fetch_add(1, Ordering::Relaxed);
                    connections().lock().unwrap().insert(conn_id, stream);

                    // Build request map
                    let req_map = Value::Map(vec![
                        (Value::Keyword("id".to_string()), Value::Int(conn_id as i64)),
                        (Value::Keyword("method".to_string()), Value::Str(req.method)),
                        (Value::Keyword("path".to_string()), Value::Str(req.path)),
                        (Value::Keyword("headers".to_string()), Value::Map(
                            req.headers.into_iter()
                                .map(|(k, v)| (Value::Str(k), Value::Str(v)))
                                .collect()
                        )),
                    ]);

                    // Send to channel (ignore errors — channel may be gone)
                    let _ = shared_send(tx_id, req_map);
                }
                Err(_) => {
                    // Malformed request — just drop the connection
                    drop(stream);
                }
            }
        }
    });

    Ok(Value::Tuple(vec![
        Value::ChannelTx(tx_id),
        Value::ChannelRx(rx_id),
    ]))
}

/// `Net.respond id response-map` — send an HTTP response and close the connection.
///
/// response-map: `{:status Int :headers Map :body Str}`
fn http_respond(args: &[Value]) -> IResult {
    let conn_id = match args.first() {
        Some(Value::Int(id)) => *id as u32,
        _ => return Err(err("Net.respond requires a connection id")),
    };
    let resp = match args.get(1) {
        Some(Value::Map(m)) => m,
        _ => return Err(err("Net.respond requires a response map")),
    };

    let status = get_map_int(resp, "status").unwrap_or(200);
    let body = get_map_str(resp, "body").unwrap_or_default();
    let headers = get_map_map(resp, "headers").unwrap_or_default();
    let content_type = get_str_from_pairs(&headers, "content-type")
        .unwrap_or_else(|| "text/html; charset=utf-8".to_string());

    let mut stream = connections()
        .lock()
        .unwrap()
        .remove(&conn_id)
        .ok_or_else(|| err(format!("Net.respond: connection {conn_id} not found")))?;

    let status_text = match status {
        200 => "OK",
        304 => "Not Modified",
        404 => "Not Found",
        500 => "Internal Server Error",
        _ => "OK",
    };

    let mut response = format!(
        "HTTP/1.1 {status} {status_text}\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nConnection: close\r\n",
        body.len()
    );

    // Add custom headers
    for (k, v) in &headers {
        let key = match k {
            Value::Str(s) => s.clone(),
            Value::Keyword(s) => s.clone(),
            _ => continue,
        };
        if key == "content-type" {
            continue; // already set
        }
        if let Value::Str(val) = v {
            response.push_str(&format!("{key}: {val}\r\n"));
        }
    }

    response.push_str("\r\n");
    response.push_str(&body);

    let _ = stream.write_all(response.as_bytes());
    let _ = stream.flush();

    Ok(Value::Unit)
}

/// `Net.serve-file conn-id file-path content-type` — read a file (binary-safe) and
/// send it as an HTTP response. Handles WASM, images, etc. that `IO.read-file` can't.
fn serve_file(args: &[Value]) -> IResult {
    let conn_id = match args.first() {
        Some(Value::Int(id)) => *id as u32,
        _ => return Err(err("Net.serve-file requires a connection id")),
    };
    let file_path = match args.get(1) {
        Some(Value::Str(s)) => s.clone(),
        _ => return Err(err("Net.serve-file requires a file path")),
    };
    let content_type = match args.get(2) {
        Some(Value::Str(s)) => s.clone(),
        _ => "application/octet-stream".to_string(),
    };

    let body = match std::fs::read(&file_path) {
        Ok(bytes) => bytes,
        Err(_) => {
            // File not found — send 404
            let mut stream = connections()
                .lock()
                .unwrap()
                .remove(&conn_id)
                .ok_or_else(|| err(format!("Net.serve-file: connection {conn_id} not found")))?;
            let resp = "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\nConnection: close\r\n\r\nnot found";
            let _ = stream.write_all(resp.as_bytes());
            let _ = stream.flush();
            return Ok(Value::Unit);
        }
    };

    let mut stream = connections()
        .lock()
        .unwrap()
        .remove(&conn_id)
        .ok_or_else(|| err(format!("Net.serve-file: connection {conn_id} not found")))?;

    let header = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: {content_type}\r\nContent-Length: {}\r\nCache-Control: no-cache\r\nConnection: close\r\n\r\n",
        body.len()
    );
    let _ = stream.write_all(header.as_bytes());
    let _ = stream.write_all(&body);
    let _ = stream.flush();

    Ok(Value::Unit)
}

/// `Net.sse-open conn-id` — send SSE headers, keep the stream open, return stream-id.
fn sse_open(args: &[Value]) -> IResult {
    let conn_id = match args.first() {
        Some(Value::Int(id)) => *id as u32,
        _ => return Err(err("Net.sse-open requires a connection id")),
    };

    let mut stream = connections()
        .lock()
        .unwrap()
        .remove(&conn_id)
        .ok_or_else(|| err(format!("Net.sse-open: connection {conn_id} not found")))?;

    let headers = "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\nAccess-Control-Allow-Origin: *\r\n\r\n";
    let _ = stream.write_all(headers.as_bytes());
    let _ = stream.flush();

    let sse_id = next_sse_id().fetch_add(1, Ordering::Relaxed);
    sse_streams().lock().unwrap().insert(sse_id, stream);

    Ok(Value::Int(sse_id as i64))
}

/// `Net.sse-send stream-id event-type data` — write an SSE event to an open stream.
fn sse_send(args: &[Value]) -> IResult {
    let sse_id = match args.first() {
        Some(Value::Int(id)) => *id as u32,
        _ => return Err(err("Net.sse-send requires a stream id")),
    };
    let event_type = match args.get(1) {
        Some(Value::Str(s)) => s.clone(),
        _ => return Err(err("Net.sse-send requires an event type string")),
    };
    let data = match args.get(2) {
        Some(Value::Str(s)) => s.clone(),
        _ => return Err(err("Net.sse-send requires a data string")),
    };

    let mut streams = sse_streams().lock().unwrap();
    if let Some(stream) = streams.get_mut(&sse_id) {
        let msg = format!("event: {event_type}\ndata: {data}\n\n");
        if stream.write_all(msg.as_bytes()).is_err() || stream.flush().is_err() {
            // Client disconnected — remove the stream
            streams.remove(&sse_id);
        }
    }
    // Silently ignore sends to closed streams

    Ok(Value::Unit)
}

/// `Net.sse-broadcast event-type data` — send an SSE event to ALL open streams.
/// Dead connections are automatically cleaned up. Thread-safe.
fn sse_broadcast(args: &[Value]) -> IResult {
    let event_type = match args.first() {
        Some(Value::Str(s)) => s.clone(),
        _ => return Err(err("Net.sse-broadcast requires an event type string")),
    };
    let data = match args.get(1) {
        Some(Value::Str(s)) => s.clone(),
        _ => return Err(err("Net.sse-broadcast requires a data string")),
    };

    let msg = format!("event: {event_type}\ndata: {data}\n\n");
    let mut streams = sse_streams().lock().unwrap();
    let mut dead = Vec::new();
    for (&id, stream) in streams.iter_mut() {
        if stream.write_all(msg.as_bytes()).is_err() || stream.flush().is_err() {
            dead.push(id);
        }
    }
    for id in dead {
        streams.remove(&id);
    }

    Ok(Value::Unit)
}

// --- Minimal HTTP request parser ---

struct HttpRequest {
    method: String,
    path: String,
    headers: Vec<(String, String)>,
}

fn parse_request(stream: &TcpStream) -> Result<HttpRequest, ()> {
    let mut reader = BufReader::new(stream);
    let mut request_line = String::new();
    reader.read_line(&mut request_line).map_err(|_| ())?;

    let parts: Vec<&str> = request_line.trim().splitn(3, ' ').collect();
    if parts.len() < 2 {
        return Err(());
    }

    let method = parts[0].to_string();
    let raw_path = parts[1];
    let path = raw_path.split('?').next().unwrap_or(raw_path).to_string();

    let mut headers = Vec::new();
    loop {
        let mut line = String::new();
        reader.read_line(&mut line).map_err(|_| ())?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            break;
        }
        if let Some((key, val)) = trimmed.split_once(':') {
            headers.push((key.trim().to_lowercase(), val.trim().to_string()));
        }
    }

    Ok(HttpRequest { method, path, headers })
}

// --- Map helpers ---

fn get_map_int(pairs: &[(Value, Value)], key: &str) -> Option<u16> {
    pairs.iter().find_map(|(k, v)| {
        if matches!(k, Value::Keyword(s) if s == key) {
            if let Value::Int(n) = v { Some(*n as u16) } else { None }
        } else {
            None
        }
    })
}

fn get_map_str(pairs: &[(Value, Value)], key: &str) -> Option<String> {
    pairs.iter().find_map(|(k, v)| {
        if matches!(k, Value::Keyword(s) if s == key) {
            if let Value::Str(s) = v { Some(s.clone()) } else { None }
        } else {
            None
        }
    })
}

fn get_map_map(pairs: &[(Value, Value)], key: &str) -> Option<Vec<(Value, Value)>> {
    pairs.iter().find_map(|(k, v)| {
        if matches!(k, Value::Keyword(s) if s == key) {
            if let Value::Map(m) = v { Some(m.clone()) } else { None }
        } else {
            None
        }
    })
}

fn get_str_from_pairs(pairs: &[(Value, Value)], key: &str) -> Option<String> {
    pairs.iter().find_map(|(k, v)| {
        let k_str = match k {
            Value::Str(s) => s.as_str(),
            Value::Keyword(s) => s.as_str(),
            _ => return None,
        };
        if k_str == key {
            if let Value::Str(s) = v { Some(s.clone()) } else { None }
        } else {
            None
        }
    })
}
