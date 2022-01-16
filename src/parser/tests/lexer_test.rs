use std::str::Chars;

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

fn do_lex<F>(input: &str, before_tokenize: F) -> Result<Vec<Token>, LexerError>
where
    F: FnOnce(&mut Lexer<Chars>),
{
    let mut lexer = Lexer::new(input.chars());
    before_tokenize(&mut lexer);
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

fn lex(input: &str) -> Result<Vec<Token>, LexerError> {
    do_lex(input, |_| {})
}

fn lex_with_heredocs(input: &str) -> Result<(Vec<Token>, Vec<HereDoc>), LexerError> {
    let mut lexer = Lexer::new(input.chars());
    let mut tokens = Vec::new();
    loop {
        match lexer.next() {
            Some(Ok(token)) => tokens.push(token),
            Some(Err(err)) => return Err(err),
            None => break,
        }
    }

    let heredocs = lexer.heredocs().to_vec();
    Ok((tokens, heredocs))
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
fn command_substituion_1() {
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
fn command_substituion_2() {
    let input = "echo `ls /`";
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
}

#[test]
fn process_substituion() {
    let input = "echo <(ls /) >(grep usr)";
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

#[test]
fn normal_heredocs() {
    assert_eq!(
        lex_with_heredocs(concat!(
            "cat <<EOF xyz\n",
            "foo\n",
            "bar\n",
            "baz\n",
            "EOF\n",
        )),
        Ok((
            (vec![
                single_plain_word("cat"),
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::HereDoc(0),
                    fd: 0
                }),
                single_plain_word("xyz"),
                Token::Newline,
            ]),
            (vec![HereDoc::new(vec![
                vec![plain_span("foo")],
                vec![plain_span("bar")],
                vec![plain_span("baz")]
            ])])
        ))
    );

    assert_eq!(
        lex_with_heredocs(concat!(
            "cat <<EOF1 << EOF2\n",
            "foo\n",
            "$bar\n",
            "EOF1\n",
            "baz\n",
            "EOF2\n",
        )),
        Ok((
            (vec![
                single_plain_word("cat"),
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::HereDoc(0),
                    fd: 0
                }),
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::HereDoc(1),
                    fd: 0
                }),
                Token::Newline,
            ]),
            (vec![
                HereDoc::new(vec![
                    vec![plain_span("foo")],
                    vec![Span::Variable {
                        name: string("bar"),
                    }]
                ]),
                HereDoc::new(vec![vec![plain_span("baz")]]),
            ])
        ))
    );
}

#[test]
fn quoted_heredocs() {
    assert_eq!(
        lex_with_heredocs(concat!(
            "cat <<'EOF' xyz\n",
            "foo\n",
            "bar\n",
            "baz\n",
            "EOF\n",
        )),
        Ok((
            (vec![
                single_plain_word("cat"),
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::HereDoc(0),
                    fd: 0
                }),
                single_plain_word("xyz"),
                Token::Newline,
            ]),
            (vec![HereDoc::new(vec![
                vec![plain_span("foo")],
                vec![plain_span("bar")],
                vec![plain_span("baz")]
            ])])
        ))
    );

    assert_eq!(
        lex_with_heredocs(concat!(
            "cat <<'EOF1' << \"EOF2\"\n",
            "foo\n",
            "$bar\n",
            "EOF1\n",
            "baz\n",
            "EOF2\n",
        )),
        Ok((
            (vec![
                single_plain_word("cat"),
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::HereDoc(0),
                    fd: 0
                }),
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::HereDoc(1),
                    fd: 0
                }),
                Token::Newline,
            ]),
            (vec![
                HereDoc::new(vec![vec![plain_span("foo")], vec![plain_span("$bar")],]),
                HereDoc::new(vec![vec![plain_span("baz")]]),
            ])
        ))
    );
}

#[test]
fn assignment() {
    assert_eq!(
        do_lex("FOO=123", |l| l.set_argv0_mode(true)),
        Ok(vec![Token::Assignment {
            name: string("FOO"),
            value: Word::new(vec![plain_span("123")]),
        }])
    );

    assert_eq!(
        do_lex("FOO=123", |l| l.set_argv0_mode(false)),
        Ok(vec![single_plain_word("FOO=123")])
    );

    assert_eq!(
        do_lex("FOO\\=123", |l| l.set_argv0_mode(true)),
        Ok(vec![Token::Argv0(Word::new(vec![plain_span("FOO=123")]))])
    );
}

#[test]
fn tilde() {
    assert_eq!(
        lex("~"),
        Ok(vec![Token::Word(Word::new(
            vec![Span::Tilde(Tilde::Home),]
        ))])
    );

    assert_eq!(lex("\\~"), Ok(vec![single_plain_word("~")]));

    assert_eq!(
        lex("~ a ~"),
        Ok(vec![
            Token::Word(Word::new(vec![Span::Tilde(Tilde::Home),])),
            single_plain_word("a"),
            Token::Word(Word::new(vec![Span::Tilde(Tilde::Home),]))
        ])
    );

    assert_eq!(
        lex("echo ~/foo/bar/baz"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![
                Span::Tilde(Tilde::Home),
                plain_span("/foo/bar/baz")
            ]))
        ])
    );

    assert_eq!(
        lex("~seiya"),
        Ok(vec![Token::Word(Word::new(vec![Span::Tilde(
            Tilde::HomeOf(string("seiya"))
        ),]))])
    );

    assert_eq!(lex("\\~seiya"), Ok(vec![single_plain_word("~seiya")]));

    assert_eq!(
        lex("~seiya a ~seiya"),
        Ok(vec![
            Token::Word(Word::new(
                vec![Span::Tilde(Tilde::HomeOf(string("seiya"))),]
            )),
            single_plain_word("a"),
            Token::Word(Word::new(
                vec![Span::Tilde(Tilde::HomeOf(string("seiya"))),]
            ))
        ])
    );

    assert_eq!(
        lex("echo ~seiya/foo/bar/baz"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![
                Span::Tilde(Tilde::HomeOf(string("seiya"))),
                plain_span("/foo/bar/baz")
            ]))
        ])
    );

    assert_eq!(
        do_lex("FOO=~/foo/bar/baz", |l| l.set_argv0_mode(true)),
        Ok(vec![Token::Assignment {
            name: string("FOO"),
            value: Word::new(vec![Span::Tilde(Tilde::Home), plain_span("/foo/bar/baz")]),
        }])
    );
}

#[test]
fn brace_expansion() {
    assert_eq!(
        lex("echo {a,b}"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Word(Word::new(vec![plain_span("a")])),
                BraceExpansion::Word(Word::new(vec![plain_span("b")])),
            ])),]))
        ])
    );

    assert_eq!(
        lex("echo x{a,b}y"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![
                plain_span("x"),
                Span::Brace(BraceExpansion::List(vec![
                    BraceExpansion::Word(Word::new(vec![plain_span("a")])),
                    BraceExpansion::Word(Word::new(vec![plain_span("b")])),
                ])),
                plain_span("y"),
            ]))
        ])
    );
}

#[test]
fn sequence_brace_expansion() {
    assert_eq!(
        lex("echo {0..10}"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Sequence(Sequence::Integer {
                    start: 0,
                    end: 10,
                    num_digits: 1
                })
            ])),]))
        ])
    );

    assert_eq!(
        lex("echo {001..100}"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Sequence(Sequence::Integer {
                    start: 1,
                    end: 100,
                    num_digits: 3
                })
            ])),]))
        ])
    );
}

#[test]
fn not_sequence_brace_expansion() {
    assert_eq!(
        lex("echo {123}"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Word(Word::new(vec![plain_span("123")]))
            ])),]))
        ])
    );

    assert_eq!(
        lex("echo {123..}"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Word(Word::new(vec![plain_span("123..")]))
            ])),]))
        ])
    );

    assert_eq!(
        lex("echo {0..x}"),
        Ok(vec![
            single_plain_word("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Word(Word::new(vec![plain_span("0..x")]))
            ])),]))
        ])
    );
}

#[test]
fn not_brace_expansion() {
    assert_eq!(
        lex("echo a,b"),
        Ok(vec![single_plain_word("echo"), single_plain_word("a,b")])
    );
}
