use loon_lang::syntax::Span;
use ropey::Rope;
use tower_lsp::lsp_types::{Position, Range};

/// Convert a byte offset into an LSP Position (0-based line and character).
pub fn offset_to_position(rope: &Rope, offset: usize) -> Position {
    let offset = offset.min(rope.len_bytes());
    let char_offset = rope.byte_to_char(offset);
    let line = rope.char_to_line(char_offset);
    let line_start_char = rope.line_to_char(line);
    let col = char_offset - line_start_char;
    Position::new(line as u32, col as u32)
}

/// Convert an LSP Position to a byte offset.
pub fn position_to_offset(rope: &Rope, pos: Position) -> usize {
    let line = pos.line as usize;
    if line >= rope.len_lines() {
        return rope.len_bytes();
    }
    let line_start_char = rope.line_to_char(line);
    let char_offset = line_start_char + pos.character as usize;
    let char_offset = char_offset.min(rope.len_chars());
    rope.char_to_byte(char_offset)
}

/// Convert a Loon Span to an LSP Range.
pub fn span_to_range(rope: &Rope, span: Span) -> Range {
    Range::new(
        offset_to_position(rope, span.start),
        offset_to_position(rope, span.end),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset_to_position_basic() {
        let rope = Rope::from_str("hello\nworld\n");
        // Start of first line
        assert_eq!(offset_to_position(&rope, 0), Position::new(0, 0));
        // 'w' at start of second line
        assert_eq!(offset_to_position(&rope, 6), Position::new(1, 0));
        // 'o' in "world"
        assert_eq!(offset_to_position(&rope, 7), Position::new(1, 1));
    }

    #[test]
    fn position_to_offset_basic() {
        let rope = Rope::from_str("hello\nworld\n");
        assert_eq!(position_to_offset(&rope, Position::new(0, 0)), 0);
        assert_eq!(position_to_offset(&rope, Position::new(1, 0)), 6);
        assert_eq!(position_to_offset(&rope, Position::new(1, 1)), 7);
    }

    #[test]
    fn roundtrip() {
        let rope = Rope::from_str("[defn add [x y]\n  [+ x y]]\n");
        for offset in [0, 1, 5, 10, 16, 20, 25] {
            let pos = offset_to_position(&rope, offset);
            let back = position_to_offset(&rope, pos);
            assert_eq!(back, offset, "roundtrip failed for offset {offset}");
        }
    }

    #[test]
    fn span_to_range_basic() {
        let rope = Rope::from_str("hello\nworld\n");
        let span = Span::new(6, 11); // "world"
        let range = span_to_range(&rope, span);
        assert_eq!(range.start, Position::new(1, 0));
        assert_eq!(range.end, Position::new(1, 5));
    }
}
