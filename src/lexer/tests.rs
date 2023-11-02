use super::{Op, StrStream, TokenType};
use crate::{lexer::Lexer, Stream};

#[test]
fn test_stream() {
    let line = "abc";
    let mut stream = StrStream::new(line);

    assert_eq!(stream.peek_char(), b'a');
    assert_eq!(stream.next_char(), b'a');
    assert_eq!(stream.peek_char(), b'b');
    assert_eq!(stream.next_char(), b'b');
    assert_eq!(stream.peek_char(), b'c');
    assert_eq!(stream.next_char(), b'c');
    assert_eq!(stream.peek_char(), b'\0');
    assert_eq!(stream.next_char(), b'\0');
}

fn test_match(string: &str, tokens: impl IntoIterator<Item = TokenType>) {
    let mut lexer = Lexer::from_string(string);
    for tok in tokens {
        assert_eq!(lexer.next_tok(), tok);
    }
    assert_eq!(lexer.next_tok(), TokenType::End);
}

#[test]
fn test_operators() {
    let string = "+ - * / not and or == != < > <= >= % | ^ & >> <<";
    let tokens = [
        Op::Add,
        Op::Sub,
        Op::Mul,
        Op::Div,
        Op::Not,
        Op::And,
        Op::Or,
        Op::Eq,
        Op::Neq,
        Op::Lt,
        Op::Gt,
        Op::Lte,
        Op::Gte,
        Op::Rem,
        Op::BitOr,
        Op::BitXor,
        Op::BitAnd,
        Op::Shr,
        Op::Shl,
    ]
    .into_iter()
    .map(TokenType::Operator);

    test_match(string, tokens);
}

#[test]
fn test_tokens() {
    let string = "( ) [ ] , = => : ?";
    let tokens = [
        TokenType::LeftParen,
        TokenType::RightParen,
        TokenType::LeftBracket,
        TokenType::RightBracket,
        TokenType::Comma,
        TokenType::Assign,
        TokenType::Arrow,
        TokenType::Colon,
        TokenType::Unknown(b'?'),
    ];

    test_match(string, tokens);
}

#[test]
fn test_idents() {
    let string = "true false if then else or and not foo";
    let tokens = [
        TokenType::True,
        TokenType::False,
        TokenType::If,
        TokenType::Then,
        TokenType::Else,
        TokenType::Operator(Op::Or),
        TokenType::Operator(Op::And),
        TokenType::Operator(Op::Not),
        TokenType::Ident("foo".into()),
    ];

    test_match(string, tokens);
}

#[test]
fn test_numbers() {
    let string = "1 .2 3. 4.5 0x1A 0b1010 0o12 1e3 -2.5 6.022e23";
    let tokens = [
        TokenType::Number("1".into()),
        TokenType::Number(".2".into()),
        TokenType::Number("3.".into()),
        TokenType::Number("4.5".into()),
        TokenType::Number("0x1A".into()),
        TokenType::Number("0b1010".into()),
        TokenType::Number("0o12".into()),
        TokenType::Number("1e3".into()),
        TokenType::Operator(Op::Sub),
        TokenType::Number("2.5".into()),
        TokenType::Number("6.022e23".into()),
    ];
    test_match(string, tokens);
}

#[test]
fn test_basic() {
    let string = "compare(a, ~)";
    let tokens = [
        TokenType::Ident("compare".into()),
        TokenType::LeftParen,
        TokenType::Ident("a".into()),
        TokenType::Comma,
        TokenType::Unknown(b'~'),
        TokenType::RightParen,
    ];

    test_match(string, tokens);
}

#[test]
fn test_prev_peek_next() {
    let string = "a b c";
    let mut lexer = Lexer::from_string(string);

    let a = TokenType::Ident("a".into());
    let b = TokenType::Ident("b".into());
    let c = TokenType::Ident("c".into());

    assert_eq!(lexer.peek_tok(), a);
    assert_eq!(lexer.next_tok(), a);
    assert_eq!(lexer.peek_tok(), b);
    lexer.prev_tok();
    assert_eq!(lexer.peek_tok(), a);
    assert_eq!(lexer.next_tok(), a);
    assert_eq!(lexer.next_tok(), b);
    assert_eq!(lexer.next_tok(), c);
    lexer.prev_tok();
    assert_eq!(lexer.next_tok(), c);
    assert_eq!(lexer.next_tok(), TokenType::End);
}
