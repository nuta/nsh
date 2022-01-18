use pretty_assertions::assert_eq;

use nsh_parser::ast::*;
use nsh_parser::lexer::*;
use nsh_parser::parser::*;

fn string(s: &str) -> String {
    s.to_owned()
}

fn word(s: &str) -> Word {
    Word::new(vec![Span::Plain(string(s))])
}

fn parse(text: &'static str) -> Result<Ast, ParseError> {
    let lexer = Lexer::new(text.chars());
    let parser = Parser::new(lexer);
    let ast = parser.parse()?;
    Ok(ast)
}

#[test]
fn simple_command() {
    assert_eq!(
        parse("echo hello"),
        Ok(Ast {
            terms: vec![Term {
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        assignments: vec![],
                        argv: vec![word("echo"), word("hello")],
                        redirections: vec![]
                    }]
                }]
            }]
        })
    );

    assert_eq!(
        parse("FOO=123 BAR=456 echo world >/dev/null 2>&1"),
        Ok(Ast {
            terms: vec![Term {
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        assignments: vec![
                            Assignment {
                                name: string("FOO"),
                                initializer: Initializer::String(word("123")),
                            },
                            Assignment {
                                name: string("BAR"),
                                initializer: Initializer::String(word("456")),
                            },
                        ],
                        argv: vec![word("echo"), word("world")],
                        redirections: vec![
                            Redirection {
                                op: RedirOp::Output(1),
                                rhs: RedirRhs::File(word("/dev/null")),
                            },
                            Redirection {
                                op: RedirOp::Output(2),
                                rhs: RedirRhs::Fd(1),
                            },
                        ]
                    }]
                }]
            }]
        })
    );

    assert_eq!(
        parse("cat foo.log | grep trace && ls /var"),
        Ok(Ast {
            terms: vec![Term {
                background: false,
                pipelines: vec![
                    Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![
                            Command::SimpleCommand {
                                assignments: vec![],
                                argv: vec![word("cat"), word("foo.log")],
                                redirections: vec![]
                            },
                            Command::SimpleCommand {
                                assignments: vec![],
                                argv: vec![word("grep"), word("trace")],
                                redirections: vec![]
                            },
                        ],
                    },
                    Pipeline {
                        run_if: RunIf::Success,
                        commands: vec![Command::SimpleCommand {
                            assignments: vec![],
                            argv: vec![word("ls"), word("/var")],
                            redirections: vec![]
                        }],
                    }
                ]
            }]
        })
    );
}

#[test]
fn simple_command_with_newlines() {
    assert_eq!(parse("\n"), Ok(Ast { terms: vec![] }));
    assert_eq!(parse("\n\n\n"), Ok(Ast { terms: vec![] }));

    assert_eq!(
        parse("echo hello\n\necho world"),
        Ok(Ast {
            terms: vec![
                Term {
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            assignments: vec![],
                            argv: vec![word("echo"), word("hello")],
                            redirections: vec![]
                        }]
                    }]
                },
                Term {
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            assignments: vec![],
                            argv: vec![word("echo"), word("world")],
                            redirections: vec![]
                        }]
                    }]
                },
            ]
        })
    );

    assert_eq!(
        parse("echo hello\n\n\necho world"),
        Ok(Ast {
            terms: vec![
                Term {
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            assignments: vec![],
                            argv: vec![word("echo"), word("hello")],
                            redirections: vec![]
                        }]
                    }]
                },
                Term {
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            assignments: vec![],
                            argv: vec![word("echo"), word("world")],
                            redirections: vec![]
                        }]
                    }]
                },
            ]
        })
    );

    assert_eq!(
        parse("\necho world"),
        Ok(Ast {
            terms: vec![Term {
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        assignments: vec![],
                        argv: vec![word("echo"), word("world")],
                        redirections: vec![]
                    }]
                }]
            },]
        })
    );
}
