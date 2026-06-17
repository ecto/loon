//! Real TCP/HTTP networking for the EIR VM.
//!
//! A deliberately small, blocking server model that fits the single-threaded
//! VM: `listen` binds a port, `accept` blocks for one connection and parses the
//! request, `send` writes the response to the connection accepted most recently
//! and closes it. One request is served at a time — which is exactly the shape a
//! cooperative handler tower drives (accept → dispatch → respond → repeat).
//!
//! These are exposed to Loon as unhandled `Net.*` effects (see
//! `Vm::builtin_effect`); a program performs them, and a handler tower decides
//! whether they hit real sockets (prod) or synthetic data (test).

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Mutex, OnceLock};

fn listeners() -> &'static Mutex<HashMap<u16, TcpListener>> {
    static I: OnceLock<Mutex<HashMap<u16, TcpListener>>> = OnceLock::new();
    I.get_or_init(|| Mutex::new(HashMap::new()))
}

/// The connection currently being served (one at a time).
fn current_conn() -> &'static Mutex<Option<TcpStream>> {
    static I: OnceLock<Mutex<Option<TcpStream>>> = OnceLock::new();
    I.get_or_init(|| Mutex::new(None))
}

/// Bind `port` (idempotent). Returns true on success.
pub fn listen(port: u16) -> bool {
    let mut ls = listeners().lock().unwrap();
    if ls.contains_key(&port) {
        return true;
    }
    match TcpListener::bind(("0.0.0.0", port)) {
        Ok(l) => {
            ls.insert(port, l);
            true
        }
        Err(_) => false,
    }
}

/// Block for one connection on `port`, parse the request line + body, store the
/// stream as the current connection, and return `(method, path, body)`.
pub fn accept(port: u16) -> Option<(String, String, String)> {
    let stream = {
        let ls = listeners().lock().unwrap();
        let l = ls.get(&port)?;
        l.accept().ok()?.0
    };
    let mut reader = BufReader::new(stream.try_clone().ok()?);

    // Request line: METHOD PATH HTTP/x.y
    let mut request_line = String::new();
    reader.read_line(&mut request_line).ok()?;
    let mut parts = request_line.split_whitespace();
    let method = parts.next().unwrap_or("GET").to_string();
    let path = parts.next().unwrap_or("/").to_string();

    // Headers — find Content-Length, stop at the blank line.
    let mut content_length = 0usize;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            break;
        }
        let trimmed = line.trim_end();
        if trimmed.is_empty() {
            break;
        }
        if let Some(v) = trimmed.to_ascii_lowercase().strip_prefix("content-length:") {
            content_length = v.trim().parse().unwrap_or(0);
        }
    }

    // Body
    let mut body = String::new();
    if content_length > 0 {
        let mut buf = vec![0u8; content_length];
        reader.read_exact(&mut buf).ok()?;
        body = String::from_utf8_lossy(&buf).into_owned();
    }

    *current_conn().lock().unwrap() = Some(stream);
    Some((method, path, body))
}

/// Write an HTTP response to the current connection and close it.
pub fn send(status: i64, body: &str) -> bool {
    let mut slot = current_conn().lock().unwrap();
    let Some(mut stream) = slot.take() else {
        return false;
    };
    let reason = match status {
        200 => "OK",
        404 => "Not Found",
        500 => "Internal Server Error",
        _ => "OK",
    };
    let response = format!(
        "HTTP/1.1 {status} {reason}\r\nContent-Length: {}\r\nContent-Type: text/plain; charset=utf-8\r\nConnection: close\r\n\r\n{body}",
        body.len()
    );
    stream.write_all(response.as_bytes()).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Read, Write};
    use std::net::TcpStream;

    #[test]
    fn real_http_round_trip() {
        let port = 47835u16;
        assert!(listen(port), "bind {port}");
        let client = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(150));
            let mut s = TcpStream::connect(("127.0.0.1", port)).unwrap();
            s.write_all(b"POST /hi HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello")
                .unwrap();
            let mut resp = String::new();
            s.read_to_string(&mut resp).unwrap();
            resp
        });
        let (method, path, body) = accept(port).expect("accept");
        assert_eq!(method, "POST");
        assert_eq!(path, "/hi");
        assert_eq!(body, "hello");
        assert!(send(200, "world"));
        let resp = client.join().unwrap();
        assert!(resp.contains("200 OK"), "status line in: {resp}");
        assert!(resp.ends_with("world"), "body in: {resp}");
    }
}
