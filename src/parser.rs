 use nom::types::CompleteStr as Input;
//type Input<'a> = &'a str;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectionDirection {
    Input,  // cat < foo.txt
    Output, // cat > foo.txt
    Append, // cat >> foo.txt
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectionType {
    File(Word),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Redirection {
    fd: usize,
    direction: RedirectionDirection,
    target: RedirectionType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RunIf {
    Always,
    /// Run the command if the previous command returned 0.
    Success,
    /// Run the command if the previous command returned non-zero value.
    Failure,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Command {
    SimpleCommand {
        argv: Vec<Word>,
        redirects: Vec<Redirection>,
        /// Assignment prefixes. (e.g. "RAILS_ENV=production rails server")
        assignments: Vec<(String, Word)>,
    },
    If {
        condition: Vec<Term>,
        then_part: Vec<Term>,
        else_part: Vec<Term>,
        redirects: Vec<Redirection>,
    },
    For {
        var_name: String,
        words: Vec<Word>,
        body: Vec<Term>,
    },
    FunctionDef {
        name: String,
        body: Vec<Term>,
    },
    Group {
        terms: Vec<Term>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pipeline {
    pub run_if: RunIf,
    pub commands: Vec<Command>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Term {
    pub pipelines: Vec<Pipeline>, // Separated by `&&', `||', or `;'.
    pub async: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ast {
    pub terms: Vec<Term>, // Separated by `&&', `||', or `;'.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SyntaxError {
    Fatal(String),
    Empty,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpansionOp {
    Length, // ${#parameter}
    GetOrEmpty,                            // $parameter and ${parameter}
    GetOrDefault(Word),                   // ${parameter:-word}
    GetNullableOrDefault(Word),           // ${parameter-word}
    GetOrDefaultAndAssign(Word),          // ${parameter:-word}
    GetNullableOrDefaultAndAssign(Word),  // ${parameter-word}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Fragment {
    Literal(String),
    // $foo, ${foo}, ${foo:-default}, ...
    Parameter {
        name: String,
        op: ExpansionOp,
    },
    // $(echo hello && echo world)
    Command {
        body: Vec<Term>,
    },
    // $((1 + 2 * 3))
    ArithExpr {
        /// Arguments for expr(1).
        /// TODO: Parse and compute by ourselves.
        body: Vec<Word>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WordOrRedirection {
    Word(Word),
    Redirection(Redirection),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Word (Vec<Fragment>);

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_valid_word_char(ch: char) -> bool {
    match ch {
        '&' | '|' | ';' | '(' | ')' | '`' => false,
        _ if is_whitespace(ch) => false,
        _ => true,
    }
}

fn is_valid_var_name_char(ch: char) -> bool {
    match ch {
        '_' => true,
        _ if ch.is_ascii_alphanumeric() => true,
        _ => false,
    }
}

fn is_whitespace(ch: char) -> bool {
    // TODO: $IFS
    " \t\n".to_owned().contains(ch)
}

fn is_whitespace_or_semicolon(ch: char) -> bool {
    is_whitespace(ch) || ch == ';'
}

named!(escape_sequence<Input, Fragment>,
    do_parse!(tag!("\\") >> s: recognize!(one_of!("\\n")) >> ( Fragment::Literal(s.to_string()) ))
);

named!(parameter_wo_braces<Input, Fragment>,
    map!(recognize!(call!(var_name)),
        |name| {
            Fragment::Parameter {
                name: name.to_string(),
                op: ExpansionOp::GetOrEmpty
            }
        })
);

named!(parameter_w_braces<Input, Fragment>,
    do_parse!(
        tag!("{") >>
        length_op: opt!(tag!("#")) >>
        name: call!(var_name) >>
        modifier: opt!(alt!(
            tag!("-") | tag!(":-") | tag!("=") | tag!(":=")
        )) >>
        default: opt!(alt!(
            // FIXME: accept Word instead
            map!(call!(expansion), |frag| Word(vec![frag])) |
            map!(recognize!(take_while1!(|c| is_valid_word_char(c) && c != '}')), parse_word)
         )) >>
        tag!("}") >>
        ({
            let default_word = default.unwrap_or(Word(vec![]));
            let op = match (length_op, modifier) {
                (Some(_), _) => ExpansionOp::Length,
                (None, Some(Input("-")))  => ExpansionOp::GetNullableOrDefault(default_word),
                (None, Some(Input(":-"))) => ExpansionOp::GetOrDefault(default_word),
                (None, Some(Input("=")))  => ExpansionOp::GetNullableOrDefaultAndAssign(default_word),
                (None, Some(Input(":="))) => ExpansionOp::GetOrDefaultAndAssign(default_word),
                (None, None) => ExpansionOp::GetOrEmpty,
                _ => unreachable!(),
            };

            Fragment::Parameter {
                name: name.to_string(),
                op,
            }
        })
    )
);

named!(parameter_expansion<Input, Fragment>,
    alt!(call!(parameter_w_braces) | call!(parameter_wo_braces))
);

named!(command_expansion<Input, Fragment>,
    do_parse!(
        // Use tag! here instead of keyword because `(' must comes
        // right after `$'.
        tag!("(") >>
        body: call!(compound_list) >>
        call!(keyword, ")") >>
        ( Fragment::Command { body } )
    )
);

named!(backquoted_command_expansion<Input, Fragment>,
    do_parse!(
        tag!("`") >>
        body: call!(compound_list) >>
        call!(keyword, "`") >>
        ( Fragment::Command { body } )
    )
);

named!(expansion<Input, Fragment>,
    do_parse!(
        tag!("$") >>
        frag: alt!(
            call!(command_expansion) | call!(parameter_expansion)
        ) >>
        ( frag )
    )
);

named!(arith_expr<Input, Fragment>,
    do_parse!(
        tag!("$((") >>
        body: many0!(call!(word)) >>
        call!(keyword, "))") >>
        ( Fragment::ArithExpr { body } )
    )
);

fn parse_word(_buf: Input) -> Word {
    let mut buf = _buf;
    let mut fragments = Vec::new();
    let mut literal = String::new();
    info!("parse_word: {:?}", buf);
    loop {
        buf = match recognize!(buf, take_while!(|c| c != '\\' && c != '$')) {
            Ok((rest, head)) => {
                trace!("frag: rest='{}', head='{}'", rest, head);
                match alt!(rest, expansion | escape_sequence) {
                    Ok((rest, frag)) => {
                        literal += &head;
                        match frag {
                            Fragment::Literal(s) => literal += s.as_str(),
                            _ => {
                                if literal.len() > 0 {
                                    fragments.push(Fragment::Literal(literal));
                                    literal = String::new();
                                }
                                fragments.push(frag)
                            },
                        }
                        rest
                    },
                    Err(_) => break,
                }
            },
            Err(_) => break,
        };
    }

    literal += buf.to_string().as_str();
    if literal.len() > 0 {
        fragments.push(Fragment::Literal(literal));
    }

    Word(fragments)
}

named!(whitespaces<Input, Input>,
    take_while!(is_whitespace)
);

named!(string_literal<Input, Word>,
    map!(
        delimited!(
           tag!("\""),
           recognize!(take_until!("\"")),
           tag!("\"")
        ),
        parse_word
    )
);

named!(unquoted_word<Input, Word>,
    map!(
        recognize!(take_while1!(is_valid_word_char)),
        parse_word
    )
);

named!(var_name<Input, String>,
    map!(recognize!(take_while1!(is_valid_var_name_char)), |name| name.to_string())
);

named!(word<Input, Word>,
    do_parse!(
        call!(whitespaces) >>
        word: alt!(
            // Try to parse $((...)) here since it may span multiple words.
            do_parse!(peek!(tag!("$((")) >> frag: call!(arith_expr) >> (Word(vec![frag]))) |
            // Try to parse $(...) here since it may span multiple words.
            do_parse!(peek!(tag!("$(")) >> tag!("$") >> frag: call!(command_expansion) >> (Word(vec![frag]))) |
            do_parse!(peek!(tag!("`")) >> frag: call!(backquoted_command_expansion) >> (Word(vec![frag]))) |
            call!(string_literal) |
            call!(unquoted_word)
        ) >>
        ( word )
    )
);

named!(nonreserved_word<Input, Word>,
    do_parse!(
        call!(whitespaces) >>
        not!(peek!(tag!("if"))) >>
        not!(peek!(tag!("then"))) >>
        not!(peek!(tag!("fi"))) >>
        not!(peek!(tag!("for"))) >>
        not!(peek!(tag!("in"))) >>
        not!(peek!(tag!("do"))) >>
        not!(peek!(tag!("done"))) >>
        not!(peek!(tag!("{"))) >>
        not!(peek!(tag!("}"))) >>
        not!(peek!(tag!("("))) >>
        not!(peek!(tag!(")"))) >>
        not!(peek!(tag!("`"))) >>
        word: call!(word) >>
        ( word )
    )
);

named!(redirection<Input, Redirection>,
    do_parse!(
        peek!(do_parse!(call!(whitespaces) >> opt!(take_while1!(is_digit)) >> call!(whitespaces) >> one_of!("<>") >> (()))) >>
        call!(whitespaces) >>
        fd_string: opt!(recognize!(take_while1!(is_digit))) >>
        call!(whitespaces) >>
        op: recognize!(alt!(tag!("<") | tag!(">") | tag!(">>"))) >>
        call!(whitespaces) >>
        target: call!(word) >>
        ({
            let target = RedirectionType::File(target);
            let (fd, direction) = match (fd_string, op) {
                (Some(s), Input("<"))  => (s.parse().unwrap(), RedirectionDirection::Input),
                (None,    Input("<"))  => (0, RedirectionDirection::Input),
                (Some(s), Input(">"))  => (s.parse().unwrap(), RedirectionDirection::Output),
                (None,    Input(">"))  => (1, RedirectionDirection::Output),
                (Some(s), Input(">>")) => (s.parse().unwrap(), RedirectionDirection::Append),
                (None,    Input(">>")) => (1, RedirectionDirection::Append),
                _ => unreachable!(),
            };

            Redirection {
                fd,
                direction,
                target,
            }
        })
    )
);

named!(assignment<Input, (String, Word)>,
    do_parse!(
        peek!(do_parse!(call!(var_name) >> tag!("=") >> (()))) >>
        name: call!(var_name) >>
        tag!("=") >>
        value: call!(word) >>
        call!(whitespaces) >>
        ( (name, value) )
    )
);

named!(assignments<Input, Vec<(String, Word)>>,
    do_parse!(
        assignments: many0!(call!(assignment)) >>
        ( assignments )
    )
);

named!(simple_command<Input, Command>,
    do_parse!(
        assignments: call!(assignments) >>
        head: call!(nonreserved_word) >>
        words: many0!(alt!(
            map!(call!(redirection), |r| WordOrRedirection::Redirection(r)) |
            map!(call!(word), |w| WordOrRedirection::Word(w))
        )) >>
        ({
            let mut argv = Vec::new();
            let mut redirects = Vec::new();
            argv.push(head);
            for word in words {
                match word {
                    WordOrRedirection::Word(arg) => argv.push(arg),
                    WordOrRedirection::Redirection(redirect) => redirects.push(redirect),
                }
            }

            Command::SimpleCommand {
                argv,
                redirects,
                assignments,
            }
        })
    )
);

named!(next_word<Input, Input>,
    do_parse!(
        take_while!(is_whitespace_or_semicolon) >>
        word: take_while!(|c| !is_whitespace_or_semicolon(c)) >>
        take_while!(is_whitespace_or_semicolon) >>
        ( word )
    )
);

named_args!(keyword<'a>(keyword: &'static str)<Input<'a>, ()>,
    do_parse!(
        take_while!(is_whitespace_or_semicolon) >>
        tag!(keyword) >>
        take_while!(is_whitespace_or_semicolon) >>
        ( () )
    )
);

named_args!(operator<'a>(keyword: &'static str)<Input<'a>, Input<'a>>,
    do_parse!(
        take_while!(is_whitespace) >>
        keyword: recognize!(tag!(keyword)) >>
        take_while!(is_whitespace) >>
        ( keyword )
    )
);

named!(if_command<Input, Command>,
    do_parse!(
        call!(keyword, "if") >>
        condition: call!(compound_list) >>
        call!(keyword, "then") >>
        then_part: call!(compound_list) >>
        call!(keyword, "fi") >>
        ({
            Command::If {
                condition,
                then_part,
                else_part: vec![], // TODO:
                redirects: vec![], // TODO:
            }
        })
    )
);

named!(for_command<Input, Command>,
    do_parse!(
        call!(keyword, "for") >>
        var_name: call!(var_name) >>
        words: opt!(do_parse!(
            call!(keyword, "in") >>
            words: many0!(call!(nonreserved_word))  >>
            ( words )
        )) >>
        call!(keyword, "do") >>
        body: call!(compound_list) >>
        call!(keyword, "done") >>
        ({
            Command:: For {
                var_name,
                words: words.unwrap_or(Vec::new()),
                body,
            }
        })
    )
);

named!(group<Input, Command>,
    do_parse!(
        call!(whitespaces) >>
        call!(keyword, "{") >>
        terms: call!(compound_list) >>
        call!(keyword, "}") >>
        ( Command::Group { terms } )
    )
);

named!(func_def<Input, Command>,
    do_parse!(
        peek!(do_parse!(
            call!(whitespaces) >>
            call!(var_name) >>
            call!(keyword, "(") >>
            call!(keyword, ")") >>
            ( () )
        )) >>
        call!(whitespaces) >>
        name: call!(var_name) >>
        call!(keyword, "(") >>
        call!(keyword, ")") >>
        body: call!(compound_list) >>
        ( Command::FunctionDef { name, body } )
    )
);

named!(command<Input, Command>,
    alt!(
        call!(if_command) |
        call!(for_command) |
        call!(group) |
        call!(func_def) |
        call!(simple_command)
    )
);

named!(pipeline<Input, Pipeline>,
    alt!(
        do_parse!(
            head: call!(command) >>
            rest: opt!(do_parse!(
                call!(operator, "|") >>
                rest: call!(pipeline) >>
                (rest)
            )) >>
            ({
                let mut commands = vec![head];
                if let Some(rest_pipeline) = rest {
                    commands.extend(rest_pipeline.commands);
                }

                trace!("command: {:?}", commands);
                Pipeline {
                    commands,
                    run_if: RunIf::Always,
                }
            })
        )
    )
);

named!(and_or_list<Input, Vec<Pipeline>>,
    alt!(
        do_parse!(
            sep: opt!(alt!(call!(operator, "&&") | call!(operator, "||"))) >>
            head: call!(pipeline) >>
            rest: opt!(do_parse!(
                rest: call!(and_or_list) >>
                (rest)
            )) >>
            ({
                let mut new_head = head.clone();
                new_head.run_if = match sep {
                    Some(Input("&&")) => RunIf::Success,
                    Some(Input("||")) => RunIf::Failure,
                    None => RunIf::Always,
                    _ => unreachable!(),
                };

                let mut pipelines = vec![new_head];
                if let Some(rest_pipelines) = rest {
                    pipelines.extend(rest_pipelines);
                }
                pipelines
            })
        )
    )
);

named!(compound_list<Input, Vec<Term>>,
    do_parse!(
        head: call!(and_or_list) >>
        sep: opt!(alt!(call!(operator, ";") | call!(operator, "&"))) >>
        rest: opt!(do_parse!(
            rest: call!(compound_list) >>
            (rest)
        )) >>
        ({
            let async = match sep {
                None | Some(Input(";")) => false,
                Some(Input("&")) => true,
                _ => unreachable!(),
            };

            let mut terms = Vec::new();
            terms.push(Term {
                pipelines: head,
                async,
            });

            if let Some(rest_terms) = rest {
                terms.extend(rest_terms);
            }

            terms
        })
    )
);

named!(parse_script<Input, Ast>,
    do_parse!(
        call!(whitespaces) >>
        terms: opt!(call!(compound_list)) >>
        call!(whitespaces) >>
        eof!() >>
        (Ast { terms: terms.unwrap_or(Vec::new()) })
    )
);

pub fn parse_line(line: &str) -> Result<Ast, SyntaxError> {
    match parse_script(Input(line)) {
        Ok((_, tree)) => {
            if tree.terms.len() == 0 {
                Err(SyntaxError::Empty)
            } else {
                Ok(tree)
            }
        }
        Err(err) => {
            trace!("parse error: '{}'", &err);
            Err(SyntaxError::Fatal(err.to_string()))
        }
    }
}

#[allow(unused)]
macro_rules! literal_word_vec {
    ($($x:expr), *) => {
        vec![$( Word(vec![Fragment::Literal($x.to_string())]), )*]
    };
}

#[allow(unused)]
macro_rules! single_word {
    ($x:expr) => {
        Word(vec![Fragment::Literal($x.to_string())])
    };
}

#[test]
pub fn test_simple_commands() {
    assert_eq!(
        parse_line("ls -G /tmp\n"),
        Ok(Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: literal_word_vec!["ls", "-G", "/tmp"],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse_line("echo hello | hexdump -C | date"),
        Ok(Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![
                        Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "hello"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                        Command::SimpleCommand {
                            argv: literal_word_vec!["hexdump", "-C"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                        Command::SimpleCommand {
                            argv: literal_word_vec!["date"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                    ],
                }],
            }],
        })
    );

    assert_eq!(
        parse_line("false || false && echo unreachable; echo reachable"),
        Ok(Ast {
            terms: vec![
                Term {
                    async: false,
                    pipelines: vec![
                        Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![Command::SimpleCommand {
                                argv: literal_word_vec!["false"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                        Pipeline {
                            run_if: RunIf::Failure,
                            commands: vec![Command::SimpleCommand {
                                argv: literal_word_vec!["false"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                        Pipeline {
                            run_if: RunIf::Success,
                            commands: vec![Command::SimpleCommand {
                                argv: literal_word_vec!["echo", "unreachable"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                    ],
                },
                Term {
                    async: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "reachable"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(
        parse_line("echo -n \"Hello world\" from; echo nsh").unwrap(),
        Ast {
            terms: vec![
                Term {
                    async: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "-n", "Hello world", "from"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    async: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "nsh"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        }
    );

    assert_eq!(
        parse_line("echo foo & sleep 1 &\n echo bar; echo baz & echo foo2 &").unwrap(),
        Ast {
            terms: vec![
                Term {
                    async: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "foo"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    async: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["sleep", "1"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    async: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "bar"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    async: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "baz"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    async: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "foo2"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        },
    );

    assert_eq!(
        parse_line("PORT=1234 RAILS_ENV=production rails s").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: literal_word_vec!["rails", "s"],
                        redirects: vec![],
                        assignments: vec![
                            ("PORT".into(), Word(vec![Fragment::Literal("1234".into())])),
                            ("RAILS_ENV".into(), Word(vec![Fragment::Literal("production".into())])),
                        ],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("ls -G <foo.txt 2> bar.txt").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: literal_word_vec!["ls", "-G"],
                        redirects: vec![
                            Redirection {
                                direction: RedirectionDirection::Input,
                                fd: 0,
                                target: RedirectionType::File(single_word!("foo.txt")),
                            },
                            Redirection {
                                direction: RedirectionDirection::Output,
                                fd: 2,
                                target: RedirectionType::File(single_word!("bar.txt")),
                            },
                        ],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );
}

#[test]
pub fn test_compound_commands() {
    assert_eq!(
        parse_line("if true; then echo it works; fi").unwrap(),
        Ast {
            terms: vec![Term {
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            async: false,
                        }],
                        then_part: vec![Term {
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["echo", "it", "works"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            async: false,
                        }],
                        else_part: vec![],
                        redirects: vec![],
                    }],
                }],
                async: false,
            }],
        }
    );

    assert_eq!(
        parse_line("if [ foo = \"foo\" ];then echo hello; echo world; fi").unwrap(),
        Ast {
            terms: vec![Term {
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["[", "foo", "=", "foo", "]"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            async: false,
                        }],
                        then_part: vec![
                            Term {
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: literal_word_vec!["echo", "hello"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                async: false,
                            },
                            Term {
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: literal_word_vec!["echo", "world"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                async: false,
                            },
                        ],
                        else_part: vec![],
                        redirects: vec![],
                    }],
                }],
                async: false,
            }],
        }
    );

    assert_eq!(
        parse_line("for arg in hello world do echo ---------; cowsay $arg; done").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::For {
                        var_name: "arg".into(),
                        words: literal_word_vec!["hello", "world"],
                        body: vec![
                            Term {
                                async: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: literal_word_vec!["echo", "---------"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                            Term {
                                async: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: vec![
                                            Word(vec![Fragment::Literal("cowsay".into())]),
                                            Word(vec![Fragment::Parameter {
                                                name: "arg".into(),
                                                op: ExpansionOp::GetOrEmpty,
                                            }])
                                        ],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                        ],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("{ echo hello; echo world; }").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Group {
                        terms: vec![
                            Term {
                                async: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: literal_word_vec!["echo", "hello"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                            Term {
                                async: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: literal_word_vec!["echo", "world"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                        ],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("func1() { echo hello; echo world; }").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::FunctionDef {
                        name: "func1".into(),
                        body: vec![Term {
                            async: false,
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::Group {
                                    terms: vec![
                                        Term {
                                            async: false,
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    argv: literal_word_vec!["echo", "hello"],
                                                    redirects: vec![],
                                                    assignments: vec![],
                                                }],
                                            }],
                                        },
                                        Term {
                                            async: false,
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    argv: literal_word_vec!["echo", "world"],
                                                    redirects: vec![],
                                                    assignments: vec![],
                                                }],
                                            }],
                                        },
                                    ],
                                }],
                            }],
                        }],
                    }],
                }],
            }],
        }
    );
}

#[test]
pub fn test_dollars() {
    assert_eq!(
        parse_line("echo ${undefined:-Current} ${undefined:=TERM} is $TERM len=${#TERM}").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![
                    Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![
                            Command::SimpleCommand {
                                argv: vec![
                                    Word(vec![Fragment::Literal("echo".into())]),
                                    Word(vec![Fragment::Parameter {
                                        name: "undefined".into(),
                                        op: ExpansionOp::GetOrDefault(Word(vec![Fragment::Literal("Current".into())])),
                                    }]),
                                    Word(vec![Fragment::Parameter {
                                        name: "undefined".into(),
                                        op: ExpansionOp::GetOrDefaultAndAssign(Word(vec![Fragment::Literal("TERM".into())])),
                                    }]),
                                    Word(vec![Fragment::Literal("is".into())]),
                                    Word(vec![Fragment::Parameter {
                                        name: "TERM".into(),
                                        op: ExpansionOp::GetOrEmpty,
                                    }]),
                                    Word(vec![
                                        Fragment::Literal("len=".into()),
                                        Fragment::Parameter {
                                            name: "TERM".into(),
                                            op: ExpansionOp::Length,
                                        }
                                    ]),
                                ],
                                redirects: vec![],
                                assignments: vec![],
                            },
                        ],
                    },
                ]
            }]
        }
    );

    assert_eq!(
        parse_line("ls `echo -l`").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![
                    Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![
                            Command::SimpleCommand {
                                argv: vec![
                                    Word(vec![Fragment::Literal("ls".into())]),
                                    Word(vec![Fragment::Command {
                                        body: vec![Term {
                                            async: false,
                                            pipelines: vec![
                                                Pipeline {
                                                    run_if: RunIf::Always,
                                                    commands: vec![
                                                        Command::SimpleCommand {
                                                            argv: vec![
                                                                Word(vec![Fragment::Literal("echo".into())]),
                                                                Word(vec![Fragment::Literal("-l".into())]),
                                                            ],
                                                            redirects: vec![],
                                                            assignments: vec![],
                                                        }
                                                    ]
                                                }
                                            ]
                                        }]
                                    }])
                                ],
                                redirects: vec![],
                                assignments: vec![],
                            },
                        ],
                    },
                ]
            }]
        }
    );

    assert_eq!(
        parse_line("ls $(echo -l)").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![
                    Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![
                            Command::SimpleCommand {
                                argv: vec![
                                    Word(vec![Fragment::Literal("ls".into())]),
                                    Word(vec![Fragment::Command {
                                        body: vec![Term {
                                            async: false,
                                            pipelines: vec![
                                                Pipeline {
                                                    run_if: RunIf::Always,
                                                    commands: vec![
                                                        Command::SimpleCommand {
                                                            argv: vec![
                                                                Word(vec![Fragment::Literal("echo".into())]),
                                                                Word(vec![Fragment::Literal("-l".into())]),
                                                            ],
                                                            redirects: vec![],
                                                            assignments: vec![],
                                                        }
                                                    ]
                                                }
                                            ]
                                        }]
                                    }])
                                ],
                                redirects: vec![],
                                assignments: vec![],
                            },
                        ],
                    },
                ]
            }]
        }
    );
}

#[test]
pub fn test_invalid_inputs() {
    assert_eq!(parse_line(""), Err(SyntaxError::Empty));
    assert_eq!(parse_line("\n"), Err(SyntaxError::Empty));
    assert_eq!(parse_line("\n\n\n"), Err(SyntaxError::Empty));
    assert_eq!(parse_line("\n\t\n"), Err(SyntaxError::Empty));
    assert_eq!(parse_line("  "), Err(SyntaxError::Empty));
    assert!(parse_line(";;;;;;").is_err());
    assert!(parse_line("||").is_err());
    assert!(parse_line("& &&").is_err());
    assert!(parse_line("echo foo ; &").is_err());
}
