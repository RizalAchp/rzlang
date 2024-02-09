use rzcalc_core::{CharStream, Lexer, Op, TokenType};

#[test]
fn test_stream() {
    let line = "abc";
    let mut stream = CharStream::new(line);

    assert_eq!(stream.peek(), Some(&'a'));
    assert_eq!(stream.next(), Some('a'));
    assert_eq!(stream.peek(), Some(&'b'));
    assert_eq!(stream.next(), Some('b'));
    assert_eq!(stream.peek(), Some(&'c'));
    assert_eq!(stream.next(), Some('c'));
    assert_eq!(stream.peek(), None);
    assert_eq!(stream.next(), None);
}

fn test_match<'s>(string: &'s str, tokens: impl IntoIterator<Item = TokenType<'s>>) {
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
    .map(TokenType::Op);

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
        TokenType::Unknown('?'),
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
        TokenType::Op(Op::Or),
        TokenType::Op(Op::And),
        TokenType::Op(Op::Not),
        TokenType::Ident("foo"),
    ];

    test_match(string, tokens);
}

#[test]
fn test_numbers() {
    let string = "1 .2 3. 4.5 0x1A 0b1010 0o12 1e3 -2.5 6.022e23";
    let tokens = [
        TokenType::Number("1"),
        TokenType::Number(".2"),
        TokenType::Number("3."),
        TokenType::Number("4.5"),
        TokenType::Number("0x1A"),
        TokenType::Number("0b1010"),
        TokenType::Number("0o12"),
        TokenType::Number("1e3"),
        TokenType::Op(Op::Sub),
        TokenType::Number("2.5"),
        TokenType::Number("6.022e23"),
    ];
    test_match(string, tokens);
}

#[test]
fn test_basic() {
    let string = "compare(a, ~)";
    let tokens = [
        TokenType::Ident("compare"),
        TokenType::LeftParen,
        TokenType::Ident("a"),
        TokenType::Comma,
        TokenType::Unknown('~'),
        TokenType::RightParen,
    ];

    test_match(string, tokens);
}

#[test]
fn test_prev_peek_next() {
    let string = "a b c";
    let mut lexer = Lexer::from_string(string);

    let a = TokenType::Ident("a");
    let b = TokenType::Ident("b");
    let c = TokenType::Ident("c");

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
