/// Built-in type definitions loaded at startup.
pub const PRELUDE: &str = r#"
[type Option T [Some T] None]
[type Result T E [Ok T] [Err E]]
"#;
