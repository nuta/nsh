use nom::types::CompleteStr as Input;
use nom::{self, IResult};

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
pub struct CaseItem {
    patterns: Vec<Word>,
    body: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElIf {
    condition: Vec<Term>,
    then_part: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Command {
    SimpleCommand {
        argv: Vec<Word>,
        redirects: Vec<Redirection>,
        /// Assignment prefixes. (e.g. "RAILS_ENV=production rails server")
        assignments: Vec<(String, Word)>,
    },
    // foo=1, bar="Hello World", ...
    Assignment {
        assignments: Vec<(String, Word)>,
    },
    If {
        condition: Vec<Term>,
        then_part: Vec<Term>,
        elif_parts: Vec<ElIf>,
        else_part: Option<Vec<Term>>,
        redirects: Vec<Redirection>,
    },
    For {
        var_name: String,
        words: Vec<Word>,
        body: Vec<Term>,
    },
    Case {
        word: Word,
        items: Vec<CaseItem>,
    },
    FunctionDef {
        name: String,
        body: Box<Command>, // Typically Command::Group.
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
    pub terms: Vec<Term>, // Separated by `&', or `;'.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SyntaxError {
    Fatal(String),
    Empty,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpansionOp {
    Length,                              // ${#parameter}
    GetOrEmpty,                          // $parameter and ${parameter}
    GetOrDefault(Word),                  // ${parameter:-word}
    GetNullableOrDefault(Word),          // ${parameter-word}
    GetOrDefaultAndAssign(Word),         // ${parameter:=word}
    GetNullableOrDefaultAndAssign(Word), // ${parameter=word}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryExpr {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Add(BinaryExpr),
    Sub(BinaryExpr),
    Mul(BinaryExpr),
    Div(BinaryExpr),
    Literal(i32),
    // $foo, ${foo}, ${foo:-default}, ...
    Parameter { name: String, op: ExpansionOp },
    // $(echo hello && echo world)
    Command { body: Vec<Term> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Span {
    Literal(String),
    // ~, ~mike, ...
    Tilde(Option<String>),
    // $foo, ${foo}, ${foo:-default}, ...
    Parameter { name: String, op: ExpansionOp },
    // $(echo hello && echo world)
    Command { body: Vec<Term> },
    // $((1 + 2 * 3))
    ArithExpr { expr: Expr },
    // *
    AnyString,
    // ?
    AnyChar,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WordOrRedirection {
    Word(Word),
    Redirection(Redirection),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Word(pub Vec<Span>);

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_valid_word_char(ch: char) -> bool {
    match ch {
        '&' | '|' | ';' | '(' | ')' | '`' | '\n' | '\\' | '$' | '*' | '?' | '"' => false,
        _ if is_whitespace(ch) => false,
        _ => true,
    }
}

fn is_valid_first_word_char(ch: char) -> bool {
    match ch {
        '&' | '|' | ';' | '(' | ')' | '{' | '}' | '\n' => false,
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
    " \t".to_owned().contains(ch)
}

named!(escape_sequence<Input, Span>,
    do_parse!(tag!("\\") >> s: recognize!(one_of!("\\n")) >> ( Span::Literal(s.to_string()) ))
);

named!(pattern<Input, Span>,
    alt!(
        map!(tag!("*"), |_| Span::AnyString) |
        map!(tag!("?"), |_| Span::AnyChar)
    )
);

named!(parameter_wo_braces<Input, Span>,
    map!(recognize!(var_name),
        |name| {
            Span::Parameter {
                name: name.to_string(),
                op: ExpansionOp::GetOrEmpty
            }
        })
);

named!(parameter_w_braces<Input, Span>,
    do_parse!(
        tag!("{") >>
        length_op: opt!(tag!("#")) >>
        name: var_name >>
        modifier: opt!(alt!(
            tag!("-") | tag!(":-") | tag!("=") | tag!(":=")
        )) >>
        default: opt!(word_in_expansion) >>
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

            Span::Parameter {
                name: name.to_string(),
                op,
            }
        })
    )
);

named!(parameter_expansion<Input, Span>,
    alt!(parameter_w_braces | parameter_wo_braces)
);

named!(command_expansion<Input, Span>,
    do_parse!(
        // Use tag! here instead of keyword because `(' must comes
        // right after `$'.
        tag!("(") >>
        body: compound_list >>
        call!(keyword, ")") >>
        ( Span::Command { body } )
    )
);

named!(backquoted_command_expansion<Input, Span>,
    do_parse!(
        tag!("`") >>
        body: compound_list >>
        call!(keyword, "`") >>
        ( Span::Command { body } )
    )
);

named!(expansion<Input, Span>,
    do_parse!(
        tag!("$") >>
        span: alt!(
            arith_expr |
            command_expansion |
            parameter_expansion
        ) >>
        ( span )
    )
);

named!(expr_factor<Input, Expr>,
    do_parse!(
        span: alt!(
            map!(
                take_while1!(|c: char| c.is_ascii_digit()),
                |s| Span::Literal(s.to_string())
            ) |
            // $(( $i ))
            do_parse!(peek!(tag!("$")) >> span: expansion >> ( span )) |
            // $(( i ))
            parameter_wo_braces
        ) >>
        ({
            match span {
                // TODO: throw an syntax error instead of .unwrap()
                Span::Literal(s) => Expr::Literal(s.parse().unwrap()),
                Span::Parameter { name, op } => Expr::Parameter { name, op },
                Span::Command { body } => Expr::Command { body },
                // TODO: throw an syntax error instead
                _ => unreachable!(),
            }
        })
    )
);

named!(expr_term<Input, Expr>,
    do_parse!(
        lhs: expr_factor >>
        rest: opt!(do_parse!(
            opt!(whitespaces) >>
            op: alt!(tag!("*") | tag!("/")) >>
            opt!(whitespaces) >>
            rhs: expr_term >>
            ( (op, rhs) )
        )) >>
        ({
            if let Some((op, rhs)) = rest {
                match op {
                    Input("*") => Expr::Mul(BinaryExpr {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Input("/") => Expr::Div(BinaryExpr {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    _ => unreachable!(),
                }
            } else {
                lhs
            }
        })
    )
);

named!(expr<Input, Expr>,
    do_parse!(
        lhs: expr_term >>
        rest: opt!(do_parse!(
            opt!(whitespaces) >>
            op: alt!(tag!("+") | tag!("-")) >>
            opt!(whitespaces) >>
            rhs: expr >>
            ( (op, rhs) )
        )) >>
        ({
            if let Some((op, rhs)) = rest {
                match op {
                    Input("+") => Expr::Add(BinaryExpr {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Input("-") => Expr::Sub(BinaryExpr {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    _ => unreachable!(),
                }
            } else {
                lhs
            }
        })
    )
);

// TODO: implement <<, >>, %, ==, !=, &&, ||, ?:
named!(arith_expr<Input, Span>,
    do_parse!(
        tag!("((") >>
        opt!(whitespaces) >>
        expr: expr >>
        opt!(whitespaces) >>
        call!(keyword, "))") >>
        ( Span::ArithExpr { expr } )
    )
);

named!(tilde_expansion<Input, Span>,
    do_parse!(
        tag!("~") >>
        name: opt!(recognize!(
            take_while1!(|c| c != '/' && is_valid_word_char(c))
        )) >>
        ( Span::Tilde(name.map(|s| s.to_string())) )
    )
);

named!(whitespaces<Input, ()>,
    do_parse!(
        take_while!(|c| c != '\n' && is_whitespace(c)) >>
        opt!(do_parse!(
            tag!("#") >>
            take_while!(|c| c != '\n') >>
            ( () )
        )) >>
        take_while!(|c| c != '\n' && is_whitespace(c)) >>
        ( () )
    )
);

named!(var_name<Input, String>,
    map!(recognize!(take_while1!(is_valid_var_name_char)), |name| name.to_string())
);

named!(comment<Input, ()>,
    do_parse!(
        tag!("#") >>
        take_while!(|c| c != '\n') >>
        ( () )
    )
);

named_args!(literal_span(in_stirng_literal: bool, in_expansion: bool)<Input, Span>,
    map!(
        take_while1!(|c| {
            if in_expansion {
                c != '}' &&
                (is_valid_word_char(c)
                || (in_stirng_literal && is_whitespace(c)))
            } else {
                c == '}'
                || is_valid_word_char(c)
                || (in_stirng_literal && is_whitespace(c))
            }
        }),
            |s| Span::Literal(s.to_string()
        )
    )
);

fn parse_word(_buf: Input, in_expansion: bool) -> IResult<Input, Word> {
    let first_len = _buf.len();
    let mut buf = _buf;
    let mut spans = Vec::new();
    let mut literal = String::new();

    if let Ok((rest, span)) = call!(buf, tilde_expansion) {
        spans.push(span);
        buf = rest;
    }

    let in_string = if let Ok((rest, _)) = tag!(buf, "\"") {
        buf = rest;
        true
    } else {
        false
    };

    info!(
        "parse_word({}): '{}' ------------------------",
        in_expansion, buf
    );
    loop {
        match alt!(
            buf,
            pattern
                | expansion
                | backquoted_command_expansion
                | escape_sequence
                | call!(literal_span, in_string, in_expansion)
        ) {
            Ok((rest, span)) => {
                trace!("rest='{}', span={:?}", rest, span);
                buf = rest;
                match span {
                    // Don't simply add `s` into `spans`; merge continuous ones.
                    Span::Literal(s) => literal += s.as_str(),
                    _ => {
                        if literal.len() > 0 {
                            spans.push(Span::Literal(literal));
                            literal = String::new();
                        }
                        spans.push(span)
                    }
                }
            }
            Err(_) => break,
        }
    }

    if literal.len() > 0 {
        spans.push(Span::Literal(literal));
    }

    if in_string {
        if let Ok((rest, _)) = tag!(buf, "\"") {
            buf = rest;
        }
    }

    if let Ok((rest, _)) = call!(buf, whitespaces) {
        buf = rest;
    }

    trace!("end_of_word: {:?}, {:?}", buf, spans);
    if first_len == buf.len() {
        Err(nom::Err::Error(error_position!(
            buf,
            nom::ErrorKind::TakeWhile1
        )))
    } else {
        Ok((buf, Word(spans)))
    }
}

named!(word<Input, Word>,
    call!(do_word, false)
);

named!(word_in_expansion<Input, Word>,
    call!(do_word, true)
);

named_args!(do_word(in_expansion: bool)<Input, Word>,
    alt!(
        do_parse!(
            peek!(do_parse!(
                whitespaces >>
                take_while1!(is_valid_first_word_char) >>
                ( () )
            )) >>
            whitespaces >>
            word: call!(parse_word, in_expansion) >>
            ( word )
        )
    )
);

named!(nonreserved_word<Input, Word>,
    do_parse!(
        whitespaces >>
        not!(peek!(tag!("if"))) >>
        not!(peek!(tag!("elif"))) >>
        not!(peek!(tag!("then"))) >>
        not!(peek!(tag!("else"))) >>
        not!(peek!(tag!("fi"))) >>
        not!(peek!(tag!("for"))) >>
        not!(peek!(tag!("in"))) >>
        not!(peek!(tag!("do"))) >>
        not!(peek!(tag!("done"))) >>
        not!(peek!(tag!("case"))) >>
        not!(peek!(tag!("esac"))) >>
        not!(peek!(tag!("{"))) >>
        not!(peek!(tag!("}"))) >>
        not!(peek!(tag!("("))) >>
        not!(peek!(tag!(")"))) >>
        not!(peek!(tag!("`"))) >>
        word: word >>
        ( word )
    )
);

named!(redirection<Input, Redirection>,
    do_parse!(
        peek!(do_parse!(whitespaces >> opt!(take_while1!(is_digit)) >> whitespaces >> one_of!("<>") >> (()))) >>
        whitespaces >>
        fd_string: opt!(recognize!(take_while1!(is_digit))) >>
        whitespaces >>
        op: recognize!(alt!(tag!("<") | tag!(">") | tag!(">>"))) >>
        whitespaces >>
        target: word >>
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
        peek!(do_parse!(var_name >> tag!("=") >> (()))) >>
        name: var_name >>
        tag!("=") >>
        value: word >>
        whitespaces >>
        ( (name, value) )
    )
);

named!(simple_command<Input, Command>,
    do_parse!(
        assignments: many0!(assignment) >>
        head: nonreserved_word >>
        whitespaces >>
        opt!(comment) >>
        words: many0!(do_parse!(
            word: alt!(
                map!(redirection, |r| WordOrRedirection::Redirection(r)) |
                map!(word, |w| WordOrRedirection::Word(w))
            ) >>
            whitespaces >>
            opt!(comment) >>
            ( word )
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

named_args!(keyword<'a>(keyword: &'static str)<Input<'a>, ()>,
    do_parse!(
        take_while!(|c| is_whitespace(c) || c == ';' || c == '\n') >>
        tag!(keyword) >>
        take_while!(|c| is_whitespace(c) || c == ';' || c == '\n') >>
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
        condition: compound_list >>
        call!(keyword, "then") >>
        opt!(tag!("\n")) >>
        then_part: compound_list >>
        elif_parts: many0!(do_parse!(
            call!(keyword, "elif") >>
            condition: compound_list >>
            call!(keyword, "then") >>
            then_part: compound_list >>
            ( ElIf { condition, then_part } )
        )) >>
        else_part: opt!(do_parse!(
            call!(keyword, "else") >>
            body: compound_list >>
            ( body )
        )) >>
        dbg_dmp!(call!(keyword, "fi")) >>
        ({
            Command::If {
                condition,
                then_part,
                elif_parts,
                else_part,
                redirects: vec![], // TODO:
            }
        })
    )
);

named!(for_command<Input, Command>,
    do_parse!(
        call!(keyword, "for") >>
        var_name: var_name >>
        words: opt!(do_parse!(
            call!(keyword, "in") >>
            words: many0!(nonreserved_word)  >>
            ( words )
        )) >>
        call!(keyword, "do") >>
        opt!(tag!("\n")) >>
        body: compound_list >>
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

named!(case_item_patterns<Input, Vec<Word>>,
    alt!(
        do_parse!(
            head: word >>
            rest: opt!(do_parse!(
                call!(operator, "|") >>
                rest: case_item_patterns >>
                (rest)
            )) >>
            ({
                let mut words = vec![head];
                if let Some(rest_words) = rest {
                    words.extend(rest_words);
                }

                words
            })
        )
    )
);

named!(case_item<Input, CaseItem>,
    do_parse!(
        opt!(call!(keyword, "(")) >>
        patterns: case_item_patterns >>
        call!(keyword, ")") >>
        body: compound_list >>
        // We cannot use call!(keyword) here; it ignores semicolons.
        take_while!(is_whitespace) >>
        tag!(";;") >>
        take_while!(is_whitespace) >>
        ({
            CaseItem {
                patterns,
                body,
            }
        })
    )
);

named!(case_command<Input, Command>,
    do_parse!(
        call!(keyword, "case") >>
        word: word >>
        call!(keyword, "in") >>
        items: many0!(call!(case_item)) >>
        call!(keyword, "esac") >>
        ({
            Command::Case {
                word,
                items,
            }
        })
    )
);

named!(group<Input, Command>,
    do_parse!(
        whitespaces >>
        call!(keyword, "{") >>
        terms: compound_list >>
        whitespaces >>
        tag!("}") >>
        ( Command::Group { terms } )
    )
);

named!(func_def<Input, Command>,
    do_parse!(
        peek!(do_parse!(
            whitespaces >>
            var_name >>
            call!(keyword, "(") >>
            call!(keyword, ")") >>
            ( () )
        )) >>
        whitespaces >>
        name: var_name >>
        call!(keyword, "(") >>
        call!(keyword, ")") >>
        body: command >>
        ( Command::FunctionDef { name, body: Box::new(body) } )
    )
);

named!(assignment_command<Input, Command>,
    do_parse!(
        assignments: many1!(assignment) >>
        ( Command::Assignment { assignments } )
    )
);

named!(command<Input, Command>,
    alt!(
        if_command |
        for_command |
        case_command |
        group |
        func_def |
        do_parse!(
            peek!(do_parse!(
                many0!(assignment) >>
                nonreserved_word >>
                ( () )
            )) >>
            command: simple_command >>
            ( command )
        ) |
        assignment_command
    )
);

named!(pipeline<Input, Pipeline>,
    alt!(
        do_parse!(
            head: command >>
            rest: opt!(do_parse!(
                call!(operator, "|") >>
                rest: pipeline >>
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
            head: pipeline >>
            rest: opt!(do_parse!(
                rest: and_or_list >>
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
        head: and_or_list >>
        sep: opt!(alt!(
            do_parse!(
                not!(call!(operator, ";;")) >>
                op: alt!(tag!("\n") | call!(operator, ";")) >>
                ( op )
            ) |
            call!(operator, "&")
        )) >>
        many0!(do_parse!(
        opt!(take_while!(|c| c == '\n' || is_whitespace(c))) >>
            opt!(comment) >>
            opt!(take_while!(|c| c == '\n' || is_whitespace(c))) >>
            ( () )
        )) >>
        rest: opt!(do_parse!(
            rest: compound_list >>
            (rest)
        )) >>
        ({
            let async = match sep {
                None | Some(Input(";")) | Some(Input("\n")) => false,
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

named!(comments<Input, ()>,
    do_parse!(
        many0!(do_parse!(
            take_while!(|c| is_whitespace(c) || c == '\n') >>
            comment >>
            take_while!(|c| is_whitespace(c) || c == '\n') >>
            ( () )
        )) >>
        ( () )
    )
);

named!(parse_script<Input, Ast>,
    do_parse!(
        take_while!(|c| is_whitespace(c) || c == '\n') >>
        comments >>
        terms: opt!(compound_list) >>
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
        vec![$( Word(vec![Span::Literal($x.to_string())]), )*]
    };
}

#[allow(unused)]
macro_rules! lit {
    ($x:expr) => {
        Word(vec![Span::Literal($x.to_string())])
    };
}

#[allow(unused)]
macro_rules! param {
    ($name:expr, $op:expr) => {
        Word(vec![Span::Parameter {
            name: $name.to_string(),
            op: $op,
        }])
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
                            ("PORT".into(), Word(vec![Span::Literal("1234".into())])),
                            (
                                "RAILS_ENV".into(),
                                Word(vec![Span::Literal("production".into())]),
                            ),
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
                                target: RedirectionType::File(lit!("foo.txt")),
                            },
                            Redirection {
                                direction: RedirectionDirection::Output,
                                fd: 2,
                                target: RedirectionType::File(lit!("bar.txt")),
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
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                async: false,
            }],
        }
    );

    assert_eq!(
        parse_line(concat!(
            "if [ foo = \"foo\" ];\n",
            "then\n",
            "    echo hello\n",
            "    echo world\n",
            "fi"
        )).unwrap(),
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
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                async: false,
            }],
        }
    );

    assert_eq!(
        parse_line(concat!(
            "if [ $name = \"john\" ];",
            "then;",
            "    echo Hello, John!;",
            "elif [ $name = \"mike\" ];",
            "then;",
            "    echo Hello, Mike!;",
            "elif [ $name = \"emily\" ];",
            "then;",
            "    echo Hello, Emily!;",
            "else;",
            "    echo Hello, stranger!;",
            "fi"
        )).unwrap(),
        Ast {
            terms: vec![Term {
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: vec![
                                        lit!("["),
                                        param!("name", ExpansionOp::GetOrEmpty),
                                        lit!("="),
                                        lit!("john"),
                                        lit!("]"),
                                    ],
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
                                    argv: literal_word_vec!["echo", "Hello,", "John!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            async: false,
                        }],
                        elif_parts: vec![
                            ElIf {
                                condition: vec![Term {
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                lit!("["),
                                                param!("name", ExpansionOp::GetOrEmpty),
                                                lit!("="),
                                                lit!("mike"),
                                                lit!("]"),
                                            ],
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
                                            argv: literal_word_vec!["echo", "Hello,", "Mike!"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    async: false,
                                }],
                            },
                            ElIf {
                                condition: vec![Term {
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                lit!("["),
                                                param!("name", ExpansionOp::GetOrEmpty),
                                                lit!("="),
                                                lit!("emily"),
                                                lit!("]"),
                                            ],
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
                                            argv: literal_word_vec!["echo", "Hello,", "Emily!"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    async: false,
                                }],
                            },
                        ],
                        else_part: Some(vec![Term {
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["echo", "Hello,", "stranger!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            async: false,
                        }]),
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
                                            lit!("cowsay"),
                                            param!("arg", ExpansionOp::GetOrEmpty),
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
        parse_line(concat!(
            "case $action in ",
            "echo) echo action is echo ;;",
            "date | time) echo action is date; date ;;",
            "esac"
        )).unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Case {
                        word: param!("action", ExpansionOp::GetOrEmpty),
                        items: vec![
                            CaseItem {
                                patterns: vec![lit!("echo")],
                                body: vec![Term {
                                    async: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: literal_word_vec!["echo", "action", "is", "echo"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                }],
                            },
                            CaseItem {
                                patterns: vec![lit!("date"), lit!("time")],
                                body: vec![
                                    Term {
                                        async: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                argv: literal_word_vec![
                                                    "echo", "action", "is", "date"
                                                ],
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
                                                argv: literal_word_vec!["date"],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                ],
                            },
                        ],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("func1() { echo hello; echo world; }; func1").unwrap(),
        Ast {
            terms: vec![
                Term {
                    async: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::FunctionDef {
                            name: "func1".into(),
                            body: Box::new(Command::Group {
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
                            }),
                        }],
                    }],
                },
                Term {
                    async: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: vec![lit!("func1")],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        },
    );
}

#[test]
pub fn test_expansions() {
    assert_eq!(
        parse_line("ls `echo -l`").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("ls".into())]),
                            Word(vec![Span::Command {
                                body: vec![Term {
                                    async: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                Word(vec![Span::Literal("echo".into())]),
                                                Word(vec![Span::Literal("-l".into())]),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                }],
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("foo ${var1:-a${xyz}b} bar").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            lit!("foo"),
                            Word(vec![Span::Parameter {
                                name: "var1".into(),
                                op: ExpansionOp::GetOrDefault(Word(vec![
                                    Span::Literal("a".into()),
                                    Span::Parameter {
                                        name: "xyz".into(),
                                        op: ExpansionOp::GetOrEmpty,
                                    },
                                    Span::Literal("b".into()),
                                ])),
                            }]),
                            lit!("bar"),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("ls $(echo -l)").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("ls".into())]),
                            Word(vec![Span::Command {
                                body: vec![Term {
                                    async: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                Word(vec![Span::Literal("echo".into())]),
                                                Word(vec![Span::Literal("-l".into())]),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                }],
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("echo ${undefined:-Current} ${undefined:=TERM} is $TERM len=${#TERM}").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::Parameter {
                                name: "undefined".into(),
                                op: ExpansionOp::GetOrDefault(Word(vec![Span::Literal(
                                    "Current".into(),
                                )])),
                            }]),
                            Word(vec![Span::Parameter {
                                name: "undefined".into(),
                                op: ExpansionOp::GetOrDefaultAndAssign(Word(vec![Span::Literal(
                                    "TERM".into(),
                                )])),
                            }]),
                            Word(vec![Span::Literal("is".into())]),
                            Word(vec![Span::Parameter {
                                name: "TERM".into(),
                                op: ExpansionOp::GetOrEmpty,
                            }]),
                            Word(vec![
                                Span::Literal("len=".into()),
                                Span::Parameter {
                                    name: "TERM".into(),
                                    op: ExpansionOp::Length,
                                },
                            ]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );
}

#[test]
pub fn test_assignments() {
    assert_eq!(
        parse_line("foo=bar").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![("foo".into(), Word(vec![Span::Literal("bar".into())]))],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("nobody=expects the=\"spanish inquisition\"").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![
                            ("nobody".into(), Word(vec![Span::Literal("expects".into())])),
                            (
                                "the".into(),
                                Word(vec![Span::Literal("spanish inquisition".into())]),
                            ),
                        ],
                    }],
                }],
            }],
        }
    );
}

#[test]
pub fn test_tilde() {
    assert_eq!(
        parse_line("echo ~ ~/usr ~seiya ~seiya/usr a/~/b").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::Tilde(None)]),
                            Word(vec![Span::Tilde(None), Span::Literal("/usr".into())]),
                            Word(vec![Span::Tilde(Some("seiya".into()))]),
                            Word(vec![
                                Span::Tilde(Some("seiya".into())),
                                Span::Literal("/usr".into()),
                            ]),
                            Word(vec![Span::Literal("a/~/b".into())]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );
}

#[test]
pub fn test_arith_expr() {
    assert_eq!(
        parse_line("echo $(( 1 + 2-3 ))").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::ArithExpr {
                                expr: Expr::Add(BinaryExpr {
                                    lhs: Box::new(Expr::Literal(1)),
                                    rhs: Box::new(Expr::Sub(BinaryExpr {
                                        lhs: Box::new(Expr::Literal(2)),
                                        rhs: Box::new(Expr::Literal(3)),
                                    })),
                                }),
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );

    assert_eq!(
        parse_line("echo $((1+2*$foo-bar))").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::ArithExpr {
                                expr: Expr::Add(BinaryExpr {
                                    lhs: Box::new(Expr::Literal(1)),
                                    rhs: Box::new(Expr::Sub(BinaryExpr {
                                        lhs: Box::new(Expr::Mul(BinaryExpr {
                                            lhs: Box::new(Expr::Literal(2)),
                                            rhs: Box::new(Expr::Parameter {
                                                name: "foo".into(),
                                                op: ExpansionOp::GetOrEmpty,
                                            }),
                                        })),
                                        rhs: Box::new(Expr::Parameter {
                                            name: "bar".into(),
                                            op: ExpansionOp::GetOrEmpty,
                                        }),
                                    })),
                                }),
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );
}

#[test]
pub fn test_patterns() {
    assert_eq!(
        parse_line("echo * a?c").unwrap(),
        Ast {
            terms: vec![Term {
                async: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::AnyString]),
                            Word(vec![
                                Span::Literal("a".into()),
                                Span::AnyChar,
                                Span::Literal("c".into()),
                            ]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }
    );
}

#[test]
pub fn test_comments() {
    assert_eq!(
        parse_line("foo bar # this is comment\n#comment line\nls -G /tmp # hello world\n"),
        Ok(Ast {
            terms: vec![
                Term {
                    async: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["foo", "bar"],
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
                            argv: literal_word_vec!["ls", "-G", "/tmp"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(parse_line("# Hello"), Err(SyntaxError::Empty));
    assert_eq!(parse_line("# Hello\n#World"), Err(SyntaxError::Empty));
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
