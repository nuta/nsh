use std::str::Chars;

use nsh_parser::highlight::*;
use nsh_parser::lexer::*;

fn string(s: &str) -> String {
    s.to_owned()
}

fn plain_span(s: &str) -> Span {
    Span::Plain(string(s))
}

fn word(s: &str) -> Word {
    Word::new(vec![Span::Plain(string(s))])
}

fn word_token_from_spans(spans: Vec<Span>) -> Token {
    Token::Word(Word::new(spans))
}

fn word_token(s: &str) -> Token {
    word_token_from_spans(vec![plain_span(s)])
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
        Ok(vec![word_token("echo"), word_token("hello")])
    );

    let input = "123";
    assert_eq!(lex(input), Ok(vec![word_token("123")]));

    let input = "echo | cat|grep foo";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            Token::Or,
            word_token("cat"),
            Token::Or,
            word_token("grep"),
            word_token("foo")
        ])
    );
}

#[test]
fn redirections() {
    assert_eq!(
        lex("echo > foo.txt"),
        Ok(vec![
            word_token("echo"),
            Token::Redirection(Redirection {
                op: RedirOp::Output(1),
                rhs: RedirRhs::File(word("foo.txt")),
            })
        ])
    );

    assert_eq!(
        lex("echo 2>foo.txt"),
        Ok(vec![
            word_token("echo"),
            Token::Redirection(Redirection {
                op: RedirOp::Output(2),
                rhs: RedirRhs::File(word("foo.txt")),
            })
        ])
    );
}

#[test]
fn redirection_markers_at_eof() {
    assert_eq!(
        lex("echo >"),
        Ok(vec![word_token("echo"), word_token(">"),])
    );

    assert_eq!(
        lex("echo > "),
        Ok(vec![word_token("echo"), word_token(">"),])
    );

    assert_eq!(
        lex("echo <"),
        Ok(vec![word_token("echo"), word_token("<"),])
    );

    assert_eq!(
        lex("echo < "),
        Ok(vec![word_token("echo"), word_token("<"),])
    );

    assert_eq!(
        lex("echo >>"),
        Ok(vec![word_token("echo"), word_token(">>"),])
    );

    assert_eq!(
        lex("echo >> "),
        Ok(vec![word_token("echo"), word_token(">>"),])
    );

    assert_eq!(lex("echo <<"), Err(LexerError::ExpectedHereDocMarker));
}

#[test]
fn command_substituion_1() {
    let input = "echo $(ls /)";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token_from_spans(vec![Span::Command(vec![word_token("ls"), word_token("/")])])
        ])
    );

    let input = "echo $(grep $(ls /foo*) bar)";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token_from_spans(vec![Span::Command(vec![
                word_token("grep"),
                word_token_from_spans(vec![Span::Command(vec![
                    word_token("ls"),
                    word_token_from_spans(vec![plain_span("/foo"), Span::AnyString,]),
                ])]),
                word_token("bar"),
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
            word_token("echo"),
            word_token_from_spans(vec![Span::Command(vec![word_token("ls"), word_token("/")])])
        ])
    );
}

#[test]
fn process_substituion() {
    let input = "echo <(ls /) >(grep usr)";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token_from_spans(vec![Span::ProcessReadable(vec![
                word_token("ls"),
                word_token("/")
            ]),]),
            word_token_from_spans(vec![Span::ProcessWritable(vec![
                word_token("grep"),
                word_token("usr")
            ]),])
        ])
    );
}

#[test]
fn variable_expansion() {
    let input = "echo a${b}c";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token_from_spans(vec![
                plain_span("a"),
                Span::Variable {
                    name: string("b"),
                    expansion: VarExpansion::GetOrEmpty,
                    quoted: false
                },
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
        Ok(vec![word_token("echo"), word_token("a b c"),])
    );

    let input = "echo \"a\\\"b\"";
    assert_eq!(
        lex(input),
        Ok(vec![word_token("echo"), word_token("a\"b"),])
    );

    let input = "echo X\"a b c\"X";
    assert_eq!(
        lex(input),
        Ok(vec![word_token("echo"), word_token("Xa b cX"),])
    );

    let input = "echo \"a $b c\"";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token_from_spans(vec![
                plain_span("a "),
                Span::Variable {
                    name: string("b"),
                    expansion: VarExpansion::GetOrEmpty,
                    quoted: true
                },
                plain_span(" c"),
            ])
        ])
    );

    let input = "echo \"a b\" c \"d e\"";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token("a b"),
            word_token("c"),
            word_token("d e"),
        ])
    );
}

#[test]
fn single_quotes() {
    let input = "echo 'a b c'";
    assert_eq!(
        lex(input),
        Ok(vec![word_token("echo"), word_token("a b c"),])
    );

    let input = "echo 'a\\'b'";
    assert_eq!(lex(input), Ok(vec![word_token("echo"), word_token("a'b"),]));

    let input = "echo X'a b c'X";
    assert_eq!(
        lex(input),
        Ok(vec![word_token("echo"), word_token("Xa b cX"),])
    );

    let input = "echo 'a $b c'";
    assert_eq!(
        lex(input),
        Ok(vec![word_token("echo"), word_token("a $b c"),])
    );

    let input = "echo 'a b' c 'd e'";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token("a b"),
            word_token("c"),
            word_token("d e"),
        ])
    );
}

#[test]
fn wildcard() {
    let input = "echo *?[a-z\\]0-9]";
    assert_eq!(
        lex(input),
        Ok(vec![
            word_token("echo"),
            word_token_from_spans(vec![
                Span::AnyString,
                Span::AnyChar,
                Span::AnyCharIn(string("a-z]0-9")),
            ]),
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
                word_token("cat"),
                Token::Redirection(Redirection {
                    op: RedirOp::Input(0),
                    rhs: RedirRhs::HereDoc(0),
                }),
                word_token("xyz"),
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
                word_token("cat"),
                Token::Redirection(Redirection {
                    op: RedirOp::Input(0),
                    rhs: RedirRhs::HereDoc(0),
                }),
                Token::Redirection(Redirection {
                    op: RedirOp::Input(0),
                    rhs: RedirRhs::HereDoc(1),
                }),
                Token::Newline,
            ]),
            (vec![
                HereDoc::new(vec![
                    vec![plain_span("foo")],
                    vec![Span::Variable {
                        name: string("bar"),
                        expansion: VarExpansion::GetOrEmpty,
                        quoted: false
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
                word_token("cat"),
                Token::Redirection(Redirection {
                    op: RedirOp::Input(0),
                    rhs: RedirRhs::HereDoc(0),
                }),
                word_token("xyz"),
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
            "cat <<'EOF1' 3<< \"EOF2\"\n",
            "foo\n",
            "$bar\n",
            "EOF1\n",
            "baz\n",
            "EOF2\n",
        )),
        Ok((
            (vec![
                word_token("cat"),
                Token::Redirection(Redirection {
                    op: RedirOp::Input(0),
                    rhs: RedirRhs::HereDoc(0),
                }),
                Token::Redirection(Redirection {
                    op: RedirOp::Input(3),
                    rhs: RedirRhs::HereDoc(1),
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
        Ok(vec![word_token("FOO=123")])
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

    assert_eq!(lex("\\~"), Ok(vec![word_token("~")]));

    assert_eq!(
        lex("~ a ~"),
        Ok(vec![
            Token::Word(Word::new(vec![Span::Tilde(Tilde::Home),])),
            word_token("a"),
            Token::Word(Word::new(vec![Span::Tilde(Tilde::Home),]))
        ])
    );

    assert_eq!(
        lex("echo ~/foo/bar/baz"),
        Ok(vec![
            word_token("echo"),
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

    assert_eq!(lex("\\~seiya"), Ok(vec![word_token("~seiya")]));

    assert_eq!(
        lex("~seiya a ~seiya"),
        Ok(vec![
            Token::Word(Word::new(
                vec![Span::Tilde(Tilde::HomeOf(string("seiya"))),]
            )),
            word_token("a"),
            Token::Word(Word::new(
                vec![Span::Tilde(Tilde::HomeOf(string("seiya"))),]
            ))
        ])
    );

    assert_eq!(
        lex("echo ~seiya/foo/bar/baz"),
        Ok(vec![
            word_token("echo"),
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
            word_token("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Word(Word::new(vec![plain_span("a")])),
                BraceExpansion::Word(Word::new(vec![plain_span("b")])),
            ])),]))
        ])
    );

    assert_eq!(
        lex("echo x{a,b}y"),
        Ok(vec![
            word_token("echo"),
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
            word_token("echo"),
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
            word_token("echo"),
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
            word_token("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Word(Word::new(vec![plain_span("123")]))
            ])),]))
        ])
    );

    assert_eq!(
        lex("echo {123..}"),
        Ok(vec![
            word_token("echo"),
            Token::Word(Word::new(vec![Span::Brace(BraceExpansion::List(vec![
                BraceExpansion::Word(Word::new(vec![plain_span("123..")]))
            ])),]))
        ])
    );

    assert_eq!(
        lex("echo {0..x}"),
        Ok(vec![
            word_token("echo"),
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
        Ok(vec![word_token("echo"), word_token("a,b")])
    );
}

#[test]
fn arithmetic_expansion() {
    assert_eq!(
        lex("echo $((1 + 2 - 3 * 4 / 5 % 6))"),
        Ok(vec![
            word_token("echo"),
            Token::Word(Word::new(vec![Span::Arith(Word::new(vec![
                plain_span("1"),
                plain_span("+"),
                plain_span("2"),
                plain_span("-"),
                plain_span("3"),
                plain_span("*"),
                plain_span("4"),
                plain_span("/"),
                plain_span("5"),
                plain_span("%"),
                plain_span("6"),
            ]))]))
        ])
    );

    assert_eq!(
        lex("echo $(((1 + (n + ( $(echo) + 4 ))  )))"),
        Ok(vec![
            word_token("echo"),
            Token::Word(Word::new(vec![Span::Arith(Word::new(vec![
                plain_span("(1"),
                plain_span("+"),
                plain_span("(n"),
                plain_span("+"),
                plain_span("("),
                Span::Command(vec![word_token("echo")]),
                plain_span("+"),
                plain_span("4"),
                plain_span("))"),
                plain_span(")"),
            ]))]))
        ])
    );
}

#[test]
fn unclosed_braces_and_parens() {
    assert_eq!(lex("echo $(ls "), Err(LexerError::NoMatchingRightParen));
    assert_eq!(lex("echo {"), Err(LexerError::UnclosedBraceExp));
    assert_eq!(lex("echo ${"), Err(LexerError::UnclosedParamExp));
    assert_eq!(lex("echo $(("), Err(LexerError::NoMatchingRightParen));
    assert_eq!(lex("echo $((("), Err(LexerError::NoMatchingRightParen));
}
