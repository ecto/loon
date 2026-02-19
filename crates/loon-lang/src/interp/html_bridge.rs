//! HTML Bridge — a virtual DOM that captures `dom/*` bridge operations
//! and serializes to an HTML string. Used for build-time SSR (static site generation).

use super::dom_builtins::set_dom_bridge;
use super::value::Value;
use super::{err, InterpError};
use std::sync::{Arc, Mutex};

type IResult = Result<Value, InterpError>;

enum NodeKind {
    Element(String),
    Text(String),
}

struct HtmlNode {
    kind: NodeKind,
    attrs: Vec<(String, String)>,
    styles: Vec<(String, String)>,
    children: Vec<u32>,
}

struct HtmlBridgeInner {
    nodes: Vec<HtmlNode>,
    location: String,
    title: Option<String>,
}

pub struct HtmlBridge {
    inner: Arc<Mutex<HtmlBridgeInner>>,
}

impl HtmlBridge {
    pub fn new(location: String) -> Self {
        // Handle 0 = root <div id="app"> equivalent
        let root = HtmlNode {
            kind: NodeKind::Element("div".to_string()),
            attrs: vec![("id".to_string(), "app".to_string())],
            styles: Vec::new(),
            children: Vec::new(),
        };
        Self {
            inner: Arc::new(Mutex::new(HtmlBridgeInner {
                nodes: vec![root],
                location,
                title: None,
            })),
        }
    }

    /// Install this bridge as the active DOM bridge for the interpreter.
    pub fn install(&self) {
        let inner = self.inner.clone();

        let bridge_fn = move |op: &str, args: &[Value]| -> IResult {
            let mut guard = inner.lock().unwrap();
            html_bridge_op(&mut guard, op, args)
        };

        set_dom_bridge(Arc::new(bridge_fn));
    }

    /// Serialize the root node's children to an HTML string.
    pub fn to_html(&self) -> String {
        let inner = self.inner.lock().unwrap();
        let mut out = String::new();
        let root = &inner.nodes[0];
        for &child_handle in &root.children {
            serialize_node(&inner.nodes, child_handle as usize, &mut out);
        }
        out
    }

    /// Return the captured title, if any.
    pub fn title(&self) -> Option<String> {
        self.inner.lock().unwrap().title.clone()
    }
}

fn html_bridge_op(inner: &mut HtmlBridgeInner, op: &str, args: &[Value]) -> IResult {
    match op {
        "createElement" => {
            let tag = arg_str(args, 0, "createElement")?;
            let handle = inner.nodes.len() as u32;
            inner.nodes.push(HtmlNode {
                kind: NodeKind::Element(tag),
                attrs: Vec::new(),
                styles: Vec::new(),
                children: Vec::new(),
            });
            Ok(Value::DomNode(handle))
        }
        "createText" => {
            let text = arg_str(args, 0, "createText")?;
            let handle = inner.nodes.len() as u32;
            inner.nodes.push(HtmlNode {
                kind: NodeKind::Text(text),
                attrs: Vec::new(),
                styles: Vec::new(),
                children: Vec::new(),
            });
            Ok(Value::DomNode(handle))
        }
        "setAttribute" => {
            let handle = arg_handle(args, 0, "setAttribute")?;
            let key = arg_str(args, 1, "setAttribute")?;
            let val = arg_str(args, 2, "setAttribute")?;
            if let Some(node) = inner.nodes.get_mut(handle) {
                // Update existing attr or add new one
                if let Some(existing) = node.attrs.iter_mut().find(|(k, _)| *k == key) {
                    existing.1 = val;
                } else {
                    node.attrs.push((key, val));
                }
            }
            Ok(Value::Unit)
        }
        "setStyle" => {
            let handle = arg_handle(args, 0, "setStyle")?;
            let key = arg_str(args, 1, "setStyle")?;
            let val = arg_str(args, 2, "setStyle")?;
            if let Some(node) = inner.nodes.get_mut(handle) {
                if let Some(existing) = node.styles.iter_mut().find(|(k, _)| *k == key) {
                    existing.1 = val;
                } else {
                    node.styles.push((key, val));
                }
            }
            Ok(Value::Unit)
        }
        "appendChild" => {
            let parent = arg_handle(args, 0, "appendChild")?;
            let child = arg_handle(args, 1, "appendChild")?;
            if let Some(node) = inner.nodes.get_mut(parent) {
                node.children.push(child as u32);
            }
            Ok(Value::Unit)
        }
        "removeChild" => {
            let parent = arg_handle(args, 0, "removeChild")?;
            let child = arg_handle(args, 1, "removeChild")?;
            if let Some(node) = inner.nodes.get_mut(parent) {
                node.children.retain(|&c| c != child as u32);
            }
            Ok(Value::Unit)
        }
        "replaceChild" => {
            let parent = arg_handle(args, 0, "replaceChild")?;
            let new_child = arg_handle(args, 1, "replaceChild")?;
            let old_child = arg_handle(args, 2, "replaceChild")?;
            if let Some(node) = inner.nodes.get_mut(parent) {
                if let Some(pos) = node.children.iter().position(|&c| c == old_child as u32) {
                    node.children[pos] = new_child as u32;
                }
            }
            Ok(Value::Unit)
        }
        "setText" => {
            let handle = arg_handle(args, 0, "setText")?;
            if let Some(node) = inner.nodes.get_mut(handle) {
                node.kind = NodeKind::Text(arg_str(args, 1, "setText")?);
            }
            Ok(Value::Unit)
        }
        "setInnerHTML" => {
            let handle = arg_handle(args, 0, "setInnerHTML")?;
            let html = arg_str(args, 1, "setInnerHTML")?;
            if html.is_empty() {
                if let Some(node) = inner.nodes.get_mut(handle) {
                    node.children.clear();
                }
            } else {
                // For non-empty innerHTML, create a raw HTML text node
                let raw_handle = inner.nodes.len() as u32;
                inner.nodes.push(HtmlNode {
                    kind: NodeKind::Text(html),
                    attrs: Vec::new(),
                    styles: Vec::new(),
                    children: Vec::new(),
                });
                if let Some(node) = inner.nodes.get_mut(handle) {
                    node.children.clear();
                    node.children.push(raw_handle);
                }
            }
            Ok(Value::Unit)
        }
        "querySelector" => {
            let selector = arg_str(args, 0, "querySelector")?;
            if selector == "#app" {
                Ok(Value::DomNode(0)) // Root node
            } else {
                // For other selectors, return a dummy node (no-op)
                Ok(Value::DomNode(0))
            }
        }
        "addListener" => {
            // No-op for SSR — skip event handlers
            // But we still need to store the callback to get a valid ID
            if args.len() >= 3 {
                if let Value::Int(cb_id) = &args[2] {
                    return Ok(Value::Int(*cb_id));
                }
            }
            Ok(Value::Int(0))
        }
        "removeListener" => Ok(Value::Unit),
        "getValue" => Ok(Value::Str(String::new())),
        "setValue" => Ok(Value::Unit),
        "evalLoon" => Ok(Value::Str(String::new())),
        "setTitle" => {
            let title = arg_str(args, 0, "setTitle")?;
            inner.title = Some(title);
            Ok(Value::Unit)
        }
        "pushState" => Ok(Value::Unit),
        "location" => Ok(Value::Str(inner.location.clone())),
        "requestAnimationFrame" => Ok(Value::Unit),
        "setTimeout" => Ok(Value::Unit),
        _ => {
            // Unknown ops are no-ops in SSR
            Ok(Value::Unit)
        }
    }
}

fn arg_str(args: &[Value], idx: usize, op: &str) -> Result<String, InterpError> {
    match args.get(idx) {
        Some(Value::Str(s)) => Ok(s.clone()),
        Some(Value::Keyword(k)) => Ok(k.clone()),
        Some(v) => Ok(v.display_str()),
        None => Err(err(format!("{op}: missing argument {idx}"))),
    }
}

fn arg_handle(args: &[Value], idx: usize, op: &str) -> Result<usize, InterpError> {
    match args.get(idx) {
        Some(Value::DomNode(h)) => Ok(*h as usize),
        Some(Value::Int(n)) => Ok(*n as usize),
        _ => Err(err(format!("{op}: expected DOM handle at arg {idx}"))),
    }
}

// --- HTML Serialization ---

const VOID_ELEMENTS: &[&str] = &[
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "link", "meta", "param", "source", "track", "wbr",
];

fn serialize_node(nodes: &[HtmlNode], idx: usize, out: &mut String) {
    let Some(node) = nodes.get(idx) else { return };

    match &node.kind {
        NodeKind::Text(text) => {
            out.push_str(text);
        }
        NodeKind::Element(tag) => {
            out.push('<');
            out.push_str(tag);

            // Attributes
            for (key, val) in &node.attrs {
                out.push(' ');
                out.push_str(key);
                out.push_str("=\"");
                escape_attr(val, out);
                out.push('"');
            }

            // Inline styles
            if !node.styles.is_empty() {
                out.push_str(" style=\"");
                for (i, (key, val)) in node.styles.iter().enumerate() {
                    if i > 0 {
                        out.push_str("; ");
                    }
                    out.push_str(key);
                    out.push_str(": ");
                    out.push_str(val);
                }
                out.push('"');
            }

            out.push('>');

            // Void elements don't get closing tags
            if VOID_ELEMENTS.contains(&tag.as_str()) {
                return;
            }

            // Children
            for &child_handle in &node.children {
                serialize_node(nodes, child_handle as usize, out);
            }

            out.push_str("</");
            out.push_str(tag);
            out.push('>');
        }
    }
}

fn escape_attr(s: &str, out: &mut String) {
    for c in s.chars() {
        match c {
            '"' => out.push_str("&quot;"),
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            _ => out.push(c),
        }
    }
}
