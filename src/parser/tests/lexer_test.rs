use nsh_parser::highlight::*;
use nsh_parser::lexer::*;

fn string(s: &str) -> String {
    s.to_owned()
}

fn plain_span(s: &str) -> Span {
    Span::Plain(string(s))
}

fn word(spans: Vec<Span>) -> Token {
    Token::Word(Word::new(spans))
}

fn single_plain_word(s: &str) -> Token {
    word(vec![plain_span(s)])
}

fn lex(input: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(input.chars());
    let mut tokens = Vec::new();
    loop {
        match lexer.next() {
            Some(Ok(token)) => tokens.push(token),
            Some(Err(err)) => return Err(err),
            None => break,
        }
    }
    Ok(tokens)
}

fn highlight(input: &str) -> Vec<HighlightSpan> {
    let mut lexer = Lexer::new(input.chars());
    while lexer.next().is_some() {}

    let mut spans = lexer.highlight_spans().to_vec();
    spans.sort_by_key(|span| span.char_range.start);
    spans
}

#[test]
fn simple_command() {
    let input = "echo hello";
    assert_eq!(
        lex(input),
        Ok(vec![single_plain_word("echo"), single_plain_word("hello")])
    );

    let input = "123";
    assert_eq!(lex(input), Ok(vec![single_plain_word("123")]));

    let input = "echo | cat|grep foo";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            Token::Or,
            single_plain_word("cat"),
            Token::Or,
            single_plain_word("grep"),
            single_plain_word("foo")
        ])
    );
}

#[test]
fn command_substituion() {
    let input = "echo $(ls /)";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            word(vec![Span::Command(vec![
                single_plain_word("ls"),
                single_plain_word("/")
            ])])
        ])
    );

    let input = "echo $(grep $(ls /foo*) bar)";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            word(vec![Span::Command(vec![
                single_plain_word("grep"),
                word(vec![Span::Command(vec![
                    single_plain_word("ls"),
                    single_plain_word("/foo*"),
                ])]),
                single_plain_word("bar"),
            ])])
        ])
    );
}

#[test]
fn variable_expansion() {
    let input = "echo a${b}c";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            word(vec![
                plain_span("a"),
                Span::Variable { name: string("b") },
                plain_span("c"),
            ])
        ])
    );
}

#[test]
fn double_quotes() {
    let input = "echo \"a b c\"";
    assert_eq!(
        lex(input),
        Ok(vec![single_plain_word("echo"), single_plain_word("a b c"),])
    );

    let input = "echo \"a\\\"b\"";
    assert_eq!(
        lex(input),
        Ok(vec![single_plain_word("echo"), single_plain_word("a\"b"),])
    );

    let input = "echo X\"a b c\"X";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            single_plain_word("Xa b cX"),
        ])
    );

    let input = "echo \"a $b c\"";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            word(vec![
                plain_span("a "),
                Span::Variable { name: string("b") },
                plain_span(" c"),
            ])
        ])
    );

    let input = "echo \"a b\" c \"d e\"";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            single_plain_word("a b"),
            single_plain_word("c"),
            single_plain_word("d e"),
        ])
    );
}

#[test]
fn single_quotes() {
    let input = "echo 'a b c'";
    assert_eq!(
        lex(input),
        Ok(vec![single_plain_word("echo"), single_plain_word("a b c"),])
    );

    let input = "echo 'a\\'b'";
    assert_eq!(
        lex(input),
        Ok(vec![single_plain_word("echo"), single_plain_word("a'b"),])
    );

    let input = "echo X'a b c'X";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            single_plain_word("Xa b cX"),
        ])
    );

    let input = "echo 'a $b c'";
    assert_eq!(
        lex(input),
        Ok(vec![single_plain_word("echo"), single_plain_word("a $b c"),])
    );

    let input = "echo 'a b' c 'd e'";
    assert_eq!(
        lex(input),
        Ok(vec![
            single_plain_word("echo"),
            single_plain_word("a b"),
            single_plain_word("c"),
            single_plain_word("d e"),
        ])
    );
}

#[test]
fn simple_highlighting() {
    assert_eq!(
        highlight("$foo 123 ${bar}"),
        vec![
            HighlightSpan {
                char_range: 0..4,
                kind: HighlightKind::Variable {
                    name: string("foo"),
                },
            },
            HighlightSpan {
                char_range: 9..15,
                kind: HighlightKind::Variable {
                    name: string("bar"),
                },
            },
        ]
    );

    assert_eq!(
        highlight("echo \"abc\"def"),
        vec![HighlightSpan {
            char_range: 5..10,
            kind: HighlightKind::QuotedString,
        },]
    );

    assert_eq!(
        // echo "a\"b"c
        highlight("echo \"a\\\"b\"c"),
        vec![
            HighlightSpan {
                char_range: 5..11,
                kind: HighlightKind::QuotedString,
            },
            HighlightSpan {
                char_range: 7..9,
                kind: HighlightKind::EscSeq,
            },
        ]
    );
}

#[test]
fn nested_highlighting() {
    assert_eq!(
        highlight("grep \"$(echo \"foo\" bar)\""),
        vec![
            HighlightSpan {
                kind: HighlightKind::QuotedString,
                char_range: 5..24
            },
            HighlightSpan {
                kind: HighlightKind::CommandSymbol,
                char_range: 7..8
            },
            HighlightSpan {
                kind: HighlightKind::Plain,
                char_range: 8..22
            },
            HighlightSpan {
                kind: HighlightKind::QuotedString,
                char_range: 13..18
            },
            HighlightSpan {
                kind: HighlightKind::CommandSymbol,
                char_range: 22..23
            },
        ]
    );
}
