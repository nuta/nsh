use pest::Parser;
use termion::color::Fg;
use termion::color;
use termion::style;

#[derive(Parser)]
#[grammar = "shell.pest"]
struct ShellParser;

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
    pub fd: usize,
    pub direction: RedirectionDirection,
    pub target: RedirectionType,
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
    pub patterns: Vec<Word>,
    pub body: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElIf {
    pub condition: Vec<Term>,
    pub then_part: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Initializer {
    Array(Vec<Word>),
    String(Word),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub name: String,
    pub initializer: Initializer,
    pub index: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LocalDeclaration {
    // local foo=123
    // local bar[0]=123 (same as `local bar=(123)`)
    Assignment(Assignment),
    // local foo
    Name(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Command {
    SimpleCommand {
        argv: Vec<Word>,
        redirects: Vec<Redirection>,
        /// Assignment prefixes. (e.g. "RAILS_ENV=production rails server")
        assignments: Vec<Assignment>,
    },
    // foo=1, bar="Hello World", ...
    Assignment {
        assignments: Vec<Assignment>,
    },
    If {
        condition: Vec<Term>,
        then_part: Vec<Term>,
        elif_parts: Vec<ElIf>,
        else_part: Option<Vec<Term>>,
        redirects: Vec<Redirection>,
    },
    While {
        condition: Vec<Term>,
        body: Vec<Term>,
    },
    For {
        var_name: String,
        words: Vec<Word>,
        body: Vec<Term>,
    },
    Break,
    Continue,
    Return {
        status: Option<i32>
    },
    Case {
        word: Word,
        cases: Vec<CaseItem>,
    },
    FunctionDef {
        name: String,
        body: Box<Command>, // Typically Command::Group.
    },
    LocalDef {
        declarations: Vec<LocalDeclaration>,
    },
    Group {
        terms: Vec<Term>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pipeline {
    pub run_if: RunIf,
    pub commands: Vec<Command>, // Separated by `|'.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Term {
    pub code: String,
    pub pipelines: Vec<Pipeline>, // Separated by `&&' or `||'.
    pub background: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ast {
    pub terms: Vec<Term>, // Separated by `&', or `;'.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
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
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Add(BinaryExpr),
    Sub(BinaryExpr),
    Mul(BinaryExpr),
    Div(BinaryExpr),
    Literal(i32),
    // `foo` in $((foo + 1))
    Parameter { name: String },
    Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Span {
    Literal(String),
    // ~, ~mike, ...
    Tilde(Option<String>),
    // $foo, ${foo}, ${foo:-default}, ...
    Parameter { name: String, op: ExpansionOp, quoted: bool },
    // $${foo[1]} ...
    ArrayParameter { name: String, index: Expr, quoted: bool },
    // $(echo hello && echo world)
    Command { body: Vec<Term>, quoted: bool },
    // $((1 + 2 * 3))
    ArithExpr { expr: Expr },
    // *
    AnyString { quoted: bool },
    // ?
    AnyChar { quoted: bool },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Word(pub Vec<Span>);

impl Word {
    #[inline]
    pub fn spans(&self) -> &[Span] {
        &self.0
    }
}

#[allow(unused)]
fn dump(pairs: pest::iterators::Pairs<Rule>, level: usize) {
    for pair in pairs {
        for _ in 0..level {
            print!("  ");
        }
        println!("{}{}{}{:?}{}: {:?}",
            style::Reset, style::Bold, Fg(color::Magenta),
            pair.as_rule(), style::Reset, pair.as_span().as_str());

        dump(pair.into_inner(), level + 1);
    }
}

#[allow(unused)]
pub fn parse_and_dump(script: &str) {
    let merged_script = script.to_string().replace("\\\n", "");
    let pairs = ShellParser::parse(Rule::script, &merged_script).unwrap_or_else(|e| panic!("{}", e));
    dump(pairs, 0);
}

use pest::iterators::Pair;
use pest::iterators::Pairs;

fn visit_command_span(pair: Pair<Rule>, quoted: bool) -> Span {
    let body = visit_compound_list(pair.into_inner().next().unwrap());
    Span::Command { body, quoted}
}

// param_ex_span = { "$" ~ "{" ~ length_op ~ expandable_var_name ~ param_opt? ~ "}" }
fn visit_param_ex_span(pair: Pair<Rule>, quoted: bool) -> Span {
    let mut inner = pair.into_inner();
    let length_op = inner.next().unwrap().as_span().as_str().to_owned();
    let name = inner.next().unwrap().as_span().as_str().to_owned();
    let index = inner.next().unwrap().into_inner().next().map(visit_expr);

    let op = if length_op == "#" {
        ExpansionOp::Length
    } else if let Some(param_opt) = inner.next() {
        let mut inner = param_opt.into_inner();
        let symbol = inner.next().unwrap().as_span().as_str();
        let default_word = inner.next().map(visit_word).unwrap_or_else(|| Word(vec![]));
        match symbol {
            "-"  => ExpansionOp::GetNullableOrDefault(default_word),
            ":-" => ExpansionOp::GetOrDefault(default_word),
            "="  => ExpansionOp::GetNullableOrDefaultAndAssign(default_word),
            ":=" => ExpansionOp::GetOrDefaultAndAssign(default_word),
            _ => unreachable!(),
        }
    } else {
        ExpansionOp::GetOrEmpty
    };

    if let Some(index) = index {
        Span::ArrayParameter { name, index, quoted }
    } else {
        Span::Parameter { name, op, quoted }
    }
}

// param_span = { "$" ~ expandable_var_name }
fn visit_param_span(pair: Pair<Rule>, quoted: bool) -> Span {
    let name = pair.into_inner().next().unwrap().as_span().as_str().to_owned();
    let op = ExpansionOp::GetOrEmpty;
    Span::Parameter { name, op, quoted }
}

// factor = { sign ~ primary }
// primary = _{ num | ("$"? ~ var_name) |  ("(" ~ expr ~ ")") }
// num = { ASCII_DIGIT+ }
fn visit_factor(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let sign = if inner.next().unwrap().as_span().as_str() == "-" {
        -1
    } else {
        1
    };

    let primary = inner.next().unwrap();
    match primary.as_rule() {
        Rule::num => {
            let lit: i32 = primary.as_span().as_str().parse().unwrap();
            Expr::Literal(sign * lit)
        },
        Rule::var_name => {
            let name = primary.as_span().as_str().to_owned();
            Expr::Parameter { name }
        },
        Rule::expr => Expr::Expr(Box::new(visit_expr(primary))),
        _ => unreachable!(),
    }
}

// term = { factor ~ (factor_op ~ expr)? }
// factor_op = { "*" | "/" }
fn visit_term(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let lhs = visit_factor(inner.next().unwrap());
    if let Some(op) = inner.next() {
        let rhs = visit_term(inner.next().unwrap());
        match op.as_span().as_str() {
            "*" => Expr::Mul(BinaryExpr { lhs: Box::new(lhs), rhs: Box::new(rhs) }),
            "/" => Expr::Div(BinaryExpr { lhs: Box::new(lhs), rhs: Box::new(rhs) }),
            _ => unreachable!()
        }
    } else {
        lhs
    }
}

// expr = { term ~ (term_op ~ expr)? }
// term_op = { "+" | "-" }
fn visit_expr(pair: Pair<Rule>) -> Expr {
    let mut inner = pair.into_inner();
    let lhs = visit_term(inner.next().unwrap());
    if let Some(op) = inner.next() {
        let rhs = visit_expr(inner.next().unwrap());
        match op.as_span().as_str() {
            "+" => Expr::Add(BinaryExpr { lhs: Box::new(lhs), rhs: Box::new(rhs) }),
            "-" => Expr::Sub(BinaryExpr { lhs: Box::new(lhs), rhs: Box::new(rhs) }),
            _ => unreachable!()
        }
    } else {
        lhs
    }
}

// expr_span = !{ "$((" ~ expr ~ "))" }
fn visit_expr_span(pair: Pair<Rule>) -> Span {
    let expr = visit_expr(pair.into_inner().next().unwrap());
    Span::ArithExpr { expr }
}

// `a\b\$cd' -> `echo ab$cd'
fn visit_escape_sequences(pair: Pair<Rule>, escaped_chars: Option<&str>) -> String {
    let mut s = String::new();
    let mut escaped = false;
    for ch in pair.as_str().chars() {
        if escaped {
            escaped = false;
            if let Some(escaped_chars) = escaped_chars {
                if !escaped_chars.contains(ch) {
                    s.push('\\');
                }
            }
            s.push(ch);
        } else if ch == '\\' {
            escaped = true;
        } else {
            s.push(ch);
        }
    }

    s
}

fn visit_word(pair: Pair<Rule>) -> Word {
    assert_eq!(pair.as_rule(), Rule::word);

    let mut spans = Vec::new();
    for span in pair.into_inner() {
        match span.as_rule() {
            Rule::literal_span => {
                spans.push(Span::Literal(visit_escape_sequences(span, None)));
            },
            Rule::double_quoted_span => {
                for span_in_quote in span.into_inner() {
                    match span_in_quote.as_rule() {
                        Rule::literal_in_double_quoted_span => {
                            spans.push(Span::Literal(visit_escape_sequences(span_in_quote, Some("\"`$"))));
                        }
                        Rule::backtick_span => spans.push(visit_command_span(span_in_quote, true)),
                        Rule::command_span => spans.push(visit_command_span(span_in_quote, true)),
                        Rule::param_span => spans.push(visit_param_span(span_in_quote, true)),
                        Rule::param_ex_span => spans.push(visit_param_ex_span(span_in_quote, true)),
                        _ => unreachable!(),
                    }
                }
            },
            Rule::single_quoted_span => {
                for span_in_quote in span.into_inner() {
                    match span_in_quote.as_rule() {
                        Rule::literal_in_single_quoted_span => {
                            spans.push(Span::Literal(span_in_quote.as_str().to_owned()));
                        }
                        _ => unreachable!(),
                    }
                }
            },
            Rule::expr_span => spans.push(visit_expr_span(span)),
            Rule::param_span => spans.push(visit_param_span(span, false)),
            Rule::param_ex_span => spans.push(visit_param_ex_span(span, false)),
            Rule::backtick_span => spans.push(visit_command_span(span, false)),
            Rule::command_span => spans.push(visit_command_span(span, false)),
            Rule::tilde_span => {
                let username = span.into_inner().next().map(|p| p.as_span().as_str().to_owned());
                spans.push(Span::Tilde(username));
            },
            Rule::any_string_span => {
                spans.push(Span::AnyString { quoted: false });
            },
            Rule::any_char_span => {
                spans.push(Span::AnyChar { quoted: false });
            },
            _ => {
                println!("unimpl: {:?}", span);
                unimplemented!();
            },
        }
    }

    Word(spans)
}

// fd = { ASCII_DIGIT+ }
// redirect_direction = { "<" | ">" | ">>" }
// redirect = { fd? ~ redirect_direction ~ word }
fn visit_redirect(pair: Pair<Rule>) -> Redirection {
    let mut inner = pair.into_inner();
    let fd     = inner.next().unwrap();
    let symbol = inner.next().unwrap();
    let target = inner.next().unwrap();

    let (direction, default_fd) = match symbol.as_span().as_str() {
        "<" => (RedirectionDirection::Input, 0),
        ">" => (RedirectionDirection::Output, 1),
        ">>" => (RedirectionDirection::Append, 1),
        _ => unreachable!(),
    };

    let fd = fd.as_span().as_str().parse().unwrap_or(default_fd);
    let target = RedirectionType::File(visit_word(target));

    Redirection {
        fd,
        direction,
        target,
    }
}

// assignment = { var_name ~ index ~ "=" ~ initializer ~ WHITESPACE? }
// index = { ("[" ~ expr ~ "]")? }
// initializer = { array_initializer | string_initializer }
// string_initializer = { word }
// array_initializer = { ("(" ~ word* ~ ")") }
fn visit_assignment(pair: Pair<Rule>) -> Assignment {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_span().as_str().to_owned();
    let index = inner.next().unwrap().into_inner().next().map(visit_expr);
    let initializer = inner.next().unwrap().into_inner().next().unwrap();
    match initializer.as_rule() {
        Rule::string_initializer => {
            let word = Initializer::String(visit_word(initializer.into_inner().next().unwrap()));
            Assignment { name, initializer: word, index }
        },
        Rule::array_initializer => {
            let word = Initializer::Array(initializer.into_inner().map(visit_word).collect());
            let index = None;
            Assignment { name, initializer: word, index }
        },
        _ => unreachable!()
    }
}

fn visit_simple_command(pair: Pair<Rule>) -> Command {
    assert_eq!(pair.as_rule(), Rule::simple_command);

    let mut argv = Vec::new();
    let mut redirects = Vec::new();

    let mut inner = pair.into_inner();
    let assignments_pairs = inner.next().unwrap().into_inner();
    let argv0 = inner.next().unwrap().into_inner().next().unwrap();
    let args = inner.next().unwrap().into_inner();

    argv.push(visit_word(argv0));
    for word_or_redirect in args {
        match word_or_redirect.as_rule() {
            Rule::word => argv.push(visit_word(word_or_redirect)),
            Rule::redirect => redirects.push(visit_redirect(word_or_redirect)),
            _ => unreachable!()
        }
    }

    let mut assignments = Vec::new();
    for assignment in assignments_pairs {
        assignments.push(visit_assignment(assignment));
    }

    Command::SimpleCommand {
        argv,
        redirects,
        assignments,
    }
}

// if_command = {
//     "if" ~ compound_list ~
//     "then" ~ compound_list ~
//     elif_part* ~
//     else_part? ~
//     "fi"
// }
// elif_part = { "elif" ~ compound_list ~ "then" ~ compound_list }
// else_part = { "else" ~ compound_list }
fn visit_if_command(pair: Pair<Rule>) -> Command {
    assert_eq!(pair.as_rule(), Rule::if_command);

    let mut inner = pair.into_inner();
    let condition = visit_compound_list(inner.next().unwrap());
    let then_part = visit_compound_list(inner.next().unwrap());
    let mut elif_parts = Vec::new();
    let mut else_part = None;
    for elif in inner {
        match elif.as_rule() {
            Rule::elif_part => {
                let mut inner = elif.into_inner();
                let condition = visit_compound_list(inner.next().unwrap());
                let then_part = visit_compound_list(inner.next().unwrap());
                elif_parts.push(ElIf { condition, then_part });
            },
            Rule::else_part => {
                let mut inner = elif.into_inner();
                let body = visit_compound_list(inner.next().unwrap());
                else_part = Some(body);
            },
            _ => unreachable!(),
        }
    }

    Command::If {
        condition,
        then_part,
        elif_parts,
        else_part,
        redirects: vec![] // TODO:
    }
}

// patterns = { word ~ ("|" ~ patterns)* }
// case_item = {
//     !("esac") ~ patterns ~ ")" ~ compound_list ~ ";;"
// }
//
// case_command = {
//     "case" ~ word ~ "in" ~ (wsnl | case_item)* ~ "esac"
// }
fn visit_case_command(pair: Pair<Rule>) -> Command {
    let mut inner = pair.into_inner();
    let word = visit_word(inner.next().unwrap());
    let mut cases = Vec::new();
    for case in inner {
        match case.as_rule() {
            Rule::case_item => {
                let mut inner = case.into_inner();
                let patterns = inner.next().unwrap().into_inner().map(visit_word).collect();
                let body = visit_compound_list(inner.next().unwrap());
                cases.push(CaseItem { patterns, body });
            },
            _ => unreachable!(),
        }
    }

    Command::Case { word, cases }
}

// while_command = {
//     "while" ~ compound_list ~ "do" ~ compound_list ~ "done"
// }
fn visit_while_command(pair: Pair<Rule>) -> Command {
    let mut inner = pair.into_inner();
    let condition = visit_compound_list(inner.next().unwrap());
    let body = visit_compound_list(inner.next().unwrap());

    Command::While {
        condition,
        body,
    }
}

// word_list = { word* }
// for_command = {
//     "for" ~ var_name ~ "in" ~ word_list ~ "do" ~ compound_list ~ "done"
// }
fn visit_for_command(pair: Pair<Rule>) -> Command {
    let mut inner = pair.into_inner();
    let var_name = inner.next().unwrap().as_span().as_str().to_owned();
    let words = inner.next().unwrap().into_inner().map(visit_word).collect();
    let body = visit_compound_list(inner.next().unwrap());

    Command::For {
        var_name,
        words,
        body,
    }
}

// function_definition = {
//     ("function")? ~ var_name ~ "()" ~ wsnl ~ compound_list
// }
fn visit_function_definition(pair: Pair<Rule>) -> Command {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_span().as_str().to_owned();
    let body = Box::new(visit_command(inner.next().unwrap()));

    Command::FunctionDef {
        name,
        body,
    }
}

// local_definition = { "local" ~ (assignment | var_name)+ }
fn visit_local_definition(pair: Pair<Rule>) -> Command {
    let mut declarations = Vec::new();
    for inner in pair.into_inner() {
        declarations.push(match inner.as_rule() {
            Rule::assignment => LocalDeclaration::Assignment(visit_assignment(inner)),
            Rule::var_name => LocalDeclaration::Name(inner.as_span().as_str().to_owned()),
            _ => unreachable!(),
        });
    }

    Command::LocalDef { declarations }
}

// assignment_command = { assignment+ }
fn visit_assignment_command(pair: Pair<Rule>) -> Command {
    let assignments = pair.into_inner().map(visit_assignment).collect();
    Command::Assignment { assignments }
}

// group = { "{" ~ compound_list ~ "}" }
fn visit_group_command(pair: Pair<Rule>) -> Command {
    let mut inner = pair.into_inner();
    let terms = visit_compound_list(inner.next().unwrap());

    Command::Group { terms }
}

// return = { return ~ num? }
fn visit_return_command(pair: Pair<Rule>) -> Command {
    let mut inner = pair.into_inner();
    let status = inner.next().map(|status| status.as_span().as_str().parse().unwrap());

    Command::Return { status }
}

fn visit_command(pair: Pair<Rule>) -> Command {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::simple_command => { visit_simple_command(inner) },
        Rule::if_command => { visit_if_command(inner) },
        Rule::while_command => { visit_while_command(inner) },
        Rule::for_command => { visit_for_command(inner) },
        Rule::case_command => { visit_case_command(inner) },
        Rule::group => { visit_group_command(inner) },
        Rule::break_command => { Command::Break },
        Rule::continue_command => { Command::Continue },
        Rule::return_command => { visit_return_command(inner) },
        Rule::assignment_command => { visit_assignment_command(inner) },
        Rule::local_definition => { visit_local_definition(inner) },
        Rule::function_definition => { visit_function_definition(inner) },
        _ => unreachable!()
    }
}

fn visit_pipeline(pair: Pair<Rule>) -> Vec<Command> {
    let mut commands = Vec::new();
    for command in pair.into_inner() {
        commands.push(visit_command(command));
    }

    commands
}

fn visit_and_or_list(pair: Pair<Rule>, run_if: RunIf) -> Vec<Pipeline> {
    let mut terms = Vec::new();
    let mut inner = pair.into_inner();
    if let Some(pipeline) = inner.next() {
        let commands = visit_pipeline(pipeline);
        terms.push(Pipeline {
            commands,
            run_if
        });

        let next_run_if = inner.next().map(|sep| {
            match sep.as_span().as_str() {
                "||" => RunIf::Failure,
                "&&" => RunIf::Success,
                _ => RunIf::Always,
            }
        }).unwrap_or(RunIf::Always);

        if let Some(rest) = inner.next() {
            terms.extend(visit_and_or_list(rest, next_run_if));
        }
    }

    terms
}

fn visit_compound_list(pair: Pair<Rule>) -> Vec<Term> {
    let mut terms = Vec::new();
    let mut inner = pair.into_inner();
    if let Some(and_or_list) = inner.next() {
        let background = inner.next().map(|s| s.as_str() == "&").unwrap_or(false);
        let rest = inner.next();

        if and_or_list.as_rule() == Rule::and_or_list {
            let code = and_or_list.as_str().to_owned().trim().to_owned();
            let pipelines = visit_and_or_list(and_or_list, RunIf::Always);
            terms.push(Term {
                code,
                pipelines,
                background,
            });
        }

        if let Some(rest) = rest {
            terms.extend(visit_compound_list(rest));
        }
    }

    terms
}

fn pairs2ast(mut pairs: Pairs<Rule>) -> Ast {
    let compound_list = pairs.next().unwrap();
    Ast { terms: visit_compound_list(compound_list) }
}

pub fn parse(script: &str) -> Result<Ast, ParseError> {
    match ShellParser::parse(Rule::script, script) {
        Ok(pairs) => {
            let ast = pairs2ast(pairs);
            if ast.terms.is_empty() {
                Err(ParseError::Empty)
            } else {
                Ok(ast)
            }
        }
        Err(err) => {
            Err(ParseError::Fatal(err.to_string()))
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
    ($name:expr, $op:expr, $quoted:expr) => {
        Word(vec![Span::Parameter {
            name: $name.to_string(),
            op: $op,
            quoted: $quoted,
        }])
    };
}

#[test]
pub fn test_simple_commands() {
    assert_eq!(
        parse("ls -G /tmp\n"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls -G /tmp".into(),
                background: false,
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
        parse("echo hello | hexdump -C | date"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo hello | hexdump -C | date".into(),
                background: false,
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
        parse("false || false && echo unreachable; echo \\\nreachable"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "false || false && echo unreachable".into(),
                    background: false,
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
                    code: "echo \\\nreachable".into(),
                    background: false,
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
        parse("echo -n \"Hello world\" from; echo nsh"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "echo -n \"Hello world\" from".into(),
                    background: false,
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
                    code: "echo nsh".into(),
                    background: false,
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
        })
    );

    assert_eq!(
        parse("echo foo & sleep 1 &\n echo bar; echo baz &; echo foo2 &"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "echo foo".into(),
                    background: true,
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
                    code: "sleep 1".into(),
                    background: true,
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
                    code: "echo bar".into(),
                    background: false,
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
                    code: "echo baz".into(),
                    background: true,
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
                    code: "echo foo2".into(),
                    background: true,
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
        }),
    );

    assert_eq!(
        parse("PORT=1234 RAILS_ENV=production rails s"),
        Ok(Ast {
            terms: vec![Term {
                code: "PORT=1234 RAILS_ENV=production rails s".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: literal_word_vec!["rails", "s"],
                        redirects: vec![],
                        assignments: vec![
                            Assignment {
                                name: "PORT".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal("1234".into())])),
                                index: None,
                            },
                            Assignment {
                                name: "RAILS_ENV".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal("production".into())])),
                                index: None,
                            }
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("ls -G <foo.txt 2> bar.txt"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls -G <foo.txt 2> bar.txt".into(),
                background: false,
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
        })
    );
}

#[test]
pub fn test_compound_commands() {
    assert_eq!(
        parse("if true; then echo it works; fi"),
        Ok(Ast {
            terms: vec![Term {
                code: "if true; then echo it works; fi".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            code: "true".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![Term {
                            code: "echo it works".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["echo", "it", "works"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parse("while maybe-true;\ndo\n echo \"while loop!\"; done"),
        Ok(Ast {
            terms: vec![Term {
                code: "while maybe-true;\ndo\n echo \"while loop!\"; done".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::While {
                        condition: vec![Term {
                            code: "maybe-true".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["maybe-true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        body: vec![Term {
                            code: "echo \"while loop!\"".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["echo", "while loop!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parse(concat!(
            "if [ foo = \"foo\" ];\n",
            "then\n",
            "    echo hello\n",
            "    echo world\n",
            "fi"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "if [ foo = \"foo\" ];\nthen\n    echo hello\n    echo world\nfi".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            code: "[ foo = \"foo\" ]".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["[", "foo", "=", "foo", "]"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![
                            Term {
                                code: "echo hello".into(),
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: literal_word_vec!["echo", "hello"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                background: false,
                            },
                            Term {
                                code: "echo world".into(),
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: literal_word_vec!["echo", "world"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                background: false,
                            },
                        ],
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parse(concat!(
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
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "if [ $name = \"john\" ];then;    echo Hello, John!;elif [ $name = \"mike\" ];then;    echo Hello, Mike!;elif [ $name = \"emily\" ];then;    echo Hello, Emily!;else;    echo Hello, stranger!;fi".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            code: "[ $name = \"john\" ]".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: vec![
                                        lit!("["),
                                        param!("name", ExpansionOp::GetOrEmpty, false),
                                        lit!("="),
                                        lit!("john"),
                                        lit!("]"),
                                    ],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![Term {
                            code: "echo Hello, John!".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["echo", "Hello,", "John!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        elif_parts: vec![
                            ElIf {
                                condition: vec![Term {
                                    code: "[ $name = \"mike\" ]".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                lit!("["),
                                                param!("name", ExpansionOp::GetOrEmpty, false),
                                                lit!("="),
                                                lit!("mike"),
                                                lit!("]"),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                                then_part: vec![Term {
                                    code: "echo Hello, Mike!".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: literal_word_vec!["echo", "Hello,", "Mike!"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                            },
                            ElIf {
                                condition: vec![Term {
                                    code: "[ $name = \"emily\" ]".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                lit!("["),
                                                param!("name", ExpansionOp::GetOrEmpty, false),
                                                lit!("="),
                                                lit!("emily"),
                                                lit!("]"),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                                then_part: vec![Term {
                                    code: "echo Hello, Emily!".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: literal_word_vec!["echo", "Hello,", "Emily!"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                            },
                        ],
                        else_part: Some(vec![Term {
                            code: "echo Hello, stranger!".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    argv: literal_word_vec!["echo", "Hello,", "stranger!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }]),
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parse("for arg in hello world; do echo ---------; cowsay $arg; done"),
        Ok(Ast {
            terms: vec![Term {
                code: "for arg in hello world; do echo ---------; cowsay $arg; done".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::For {
                        var_name: "arg".into(),
                        words: literal_word_vec!["hello", "world"],
                        body: vec![
                            Term {
                                code: "echo ---------".into(),
                                background: false,
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
                                code: "cowsay $arg".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        argv: vec![
                                            lit!("cowsay"),
                                            param!("arg", ExpansionOp::GetOrEmpty, false),
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
        })
    );

    assert_eq!(
        parse(concat!(
            "for arg in hello world; do",
            "   if sometimes-true; then\n",
            "       break\n",
            "   fi\n",
            "   if sometimes-true; then\n",
            "       continue;\n",
            "   fi\n",
            "   something &\n",
            "done"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "for arg in hello world; do   if sometimes-true; then\n       break\n   fi\n   if sometimes-true; then\n       continue;\n   fi\n   something &\ndone".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::For {
                        var_name: "arg".into(),
                        words: literal_word_vec!["hello", "world"],
                        body: vec![
                            Term {
                                code: "if sometimes-true; then\n       break\n   fi".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::If {
                                        condition: vec![Term {
                                            code: "sometimes-true".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    argv: vec![lit!("sometimes-true")],
                                                    redirects: vec![],
                                                    assignments: vec![],
                                                }],
                                            }],
                                            background: false,
                                        }],
                                        then_part: vec![Term {
                                            code: "break".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::Break],
                                            }],
                                            background: false,
                                        }],
                                        elif_parts: vec![],
                                        else_part: None,
                                        redirects: vec![],
                                    }]
                                }]
                            },
                            Term {
                                code: "if sometimes-true; then\n       continue;\n   fi".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::If {
                                        condition: vec![Term {
                                            code: "sometimes-true".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    argv: vec![lit!("sometimes-true")],
                                                    redirects: vec![],
                                                    assignments: vec![],
                                                }],
                                            }],
                                            background: false,
                                        }],
                                        then_part: vec![Term {
                                            code: "continue".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::Continue],
                                            }],
                                            background: false,
                                        }],
                                        elif_parts: vec![],
                                        else_part: None,
                                        redirects: vec![],
                                    }]
                                }]
                            },
                            Term {
                                code: "something".into(),
                                background: true,
                                pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                lit!("something"),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }
                                ]
                            },
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("{ echo hello; echo world; }"),
        Ok(Ast {
            terms: vec![Term {
                code: "{ echo hello; echo world; }".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Group {
                        terms: vec![
                            Term {
                                code: "echo hello".into(),
                                background: false,
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
                                code: "echo world".into(),
                                background: false,
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
        })
    );

    assert_eq!(
        parse(concat!(
            "case $action in\n",
            "echo) echo action is echo ;;\n",
            "date | time) echo action is date; date ;;\n",
            "esac"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "case $action in\necho) echo action is echo ;;\ndate | time) echo action is date; date ;;\nesac".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Case {
                        word: param!("action", ExpansionOp::GetOrEmpty, false),
                        cases: vec![
                            CaseItem {
                                patterns: vec![lit!("echo")],
                                body: vec![Term {
                                    code: "echo action is echo".into(),
                                    background: false,
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
                                        code: "echo action is date".into(),
                                        background: false,
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
                                        code: "date".into(),
                                        background: false,
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
        })
    );

    assert_eq!(
        parse("function func1() { echo hello; echo world; return 3; }; func1"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "function func1() { echo hello; echo world; return 3; }".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::FunctionDef {
                            name: "func1".into(),
                            body: Box::new(Command::Group {
                                terms: vec![
                                    Term {
                                        code: "echo hello".into(),
                                        background: false,
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
                                        code: "echo world".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                argv: literal_word_vec!["echo", "world"],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "return 3".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::Return { status: Some(3) }],
                                        }],
                                    },
                                ],
                            }),
                        }],
                    }],
                },
                Term {
                    code: "func1".into(),
                    background: false,
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
        })
    );

    assert_eq!(
        parse("x=$((123)); func2() { local x=456 y z; echo $((x * 2))\n return; }; echo $x"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "x=$((123))".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::Assignment {
                            assignments: vec![
                                Assignment {
                                    name: "x".into(),
                                    initializer: Initializer::String(Word(vec![
                                        Span::ArithExpr {
                                            expr: Expr::Literal(123)
                                        }
                                    ])),
                                    index: None,
                                }
                            ],
                        }],
                    }],
                },
                Term {
                    code: "func2() { local x=456 y z; echo $((x * 2))\n return; }".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::FunctionDef {
                            name: "func2".into(),
                            body: Box::new(Command::Group {
                                terms: vec![
                                    Term {
                                        code: "local x=456 y z".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::LocalDef {
                                                declarations: vec![
                                                    LocalDeclaration::Assignment(Assignment {
                                                        name: "x".into(),
                                                        initializer: Initializer::String(Word(vec![Span::Literal("456".into())])),
                                                        index: None,
                                                    }),
                                                    LocalDeclaration::Name("y".into()),
                                                    LocalDeclaration::Name("z".into())
                                                ]
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "echo $((x * 2))".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                argv: vec![
                                                    lit!("echo"),
                                                    Word(vec![Span::ArithExpr {
                                                        expr: Expr::Mul(BinaryExpr {
                                                            lhs: Box::new(Expr::Parameter { name: "x".into() }),
                                                            rhs: Box::new(Expr::Literal(2)),
                                                        })
                                                    }])
                                                ],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "return".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::Return { status: None }],
                                        }],
                                    },
                                ],
                            }),
                        }],
                    }],
                },
                Term {
                    code: "echo $x".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: vec![
                                lit!("echo"),
                                Word(vec![Span::Parameter {
                                    name: "x".into(),
                                    op: ExpansionOp::GetOrEmpty,
                                    quoted: false,
                                }])
                            ],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );
}

#[test]
pub fn test_expansions() {
    assert_eq!(
        parse("ls `echo   -l`"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls `echo   -l`".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("ls".into())]),
                            Word(vec![Span::Command {
                                quoted: false,
                                body: vec![Term {
                                    code: "echo   -l".into(),
                                    background: false,
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
        })
    );

    assert_eq!(
        parse("ls $(echo -l)"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls $(echo -l)".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("ls".into())]),
                            Word(vec![Span::Command {
                                quoted: false,
                                body: vec![Term {
                                    code: "echo -l".into(),
                                    background: false,
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
        })
    );

    assert_eq!(
        parse("echo \"$TERM\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo \"$TERM\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::Parameter {
                                name: "TERM".into(),
                                op: ExpansionOp::GetOrEmpty,
                                quoted: true,
                            }],),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("echo $? $7"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo $? $7".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::Parameter {
                                name: "?".into(),
                                op: ExpansionOp::GetOrEmpty,
                                quoted: false,
                            }],),
                            Word(vec![Span::Parameter {
                                name: "7".into(),
                                op: ExpansionOp::GetOrEmpty,
                                quoted: false,
                            }],),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("foo ${var1:-a${xyz}b} bar"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo ${var1:-a${xyz}b} bar".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            lit!("foo"),
                            Word(vec![Span::Parameter {
                                name: "var1".into(),
                                quoted: false,
                                op: ExpansionOp::GetOrDefault(Word(vec![
                                    Span::Literal("a".into()),
                                    Span::Parameter {
                                        name: "xyz".into(),
                                        op: ExpansionOp::GetOrEmpty,
                                        quoted: false,
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
        })
    );

    assert_eq!(
        parse(r#"echo ${undefined:-Current} "\"\$"${undefined:=TERM}\" "is $TERM len=${#TERM}""#),
        Ok(Ast {
            terms: vec![Term {
                code: r#"echo ${undefined:-Current} "\"\$"${undefined:=TERM}\" "is $TERM len=${#TERM}""#.into(),
                background: false,
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
                                quoted: false,
                            }]),
                            Word(vec![
                                Span::Literal("\"$".to_owned()),
                                Span::Parameter {
                                    name: "undefined".into(),
                                    op: ExpansionOp::GetOrDefaultAndAssign(Word(vec![Span::Literal(
                                        "TERM".into(),
                                    )])),
                                    quoted: false,
                                },
                                Span::Literal("\"".to_owned()),
                            ]),
                            Word(vec![
                                Span::Literal("is ".into()),
                                Span::Parameter {
                                    name: "TERM".into(),
                                    op: ExpansionOp::GetOrEmpty,
                                    quoted: true,
                                },
                                Span::Literal(" len=".into()),
                                Span::Parameter {
                                    name: "TERM".into(),
                                    op: ExpansionOp::Length,
                                    quoted: true,
                                },
                            ]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_assignments() {
    assert_eq!(
        parse("foo=bar"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo=bar".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![
                            Assignment {
                                name: "foo".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal("bar".into())])),
                                index: None,
                            }
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("foo=('I wanna quit gym' 'holy moly' egg 'spam spam beans spam')"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo=('I wanna quit gym' 'holy moly' egg 'spam spam beans spam')".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![
                            Assignment {
                                name: "foo".into(),
                                initializer: Initializer::Array(vec![
                                    Word(vec![Span::Literal("I wanna quit gym".into())]),
                                    Word(vec![Span::Literal("holy moly".into())]),
                                    Word(vec![Span::Literal("egg".into())]),
                                    Word(vec![Span::Literal("spam spam beans spam".into())]),
                                ]),
                                index: None,
                            }
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("foo[k + 7 * c]=bar"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo[k + 7 * c]=bar".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![
                            Assignment {
                                name: "foo".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal("bar".into())])),
                                index: Some(Expr::Add(
                                    BinaryExpr {
                                        lhs: Box::new(Expr::Parameter { name: "k".into() }),
                                        rhs: Box::new(Expr::Mul(BinaryExpr {
                                            lhs: Box::new(Expr::Literal(7)),
                                            rhs: Box::new(Expr::Parameter { name: "c".into() }),
                                        }))
                                    }
                                )),
                            }
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("nobody=expects the=\"spanish inquisition\""),
        Ok(Ast {
            terms: vec![Term {
                code: "nobody=expects the=\"spanish inquisition\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![
                            Assignment {
                                name: "nobody".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal("expects".into())])),
                                index: None
                            },
                            Assignment {
                                name: "the".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal("spanish inquisition".into())])),
                                index: None
                            },
                        ],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_tilde() {
    assert_eq!(
        parse("echo ~ ~/usr ~seiya ~seiya/usr a/~/b"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo ~ ~/usr ~seiya ~seiya/usr a/~/b".into(),
                background: false,
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
        })
    );
}

#[test]
pub fn test_arith_expr() {
    assert_eq!(
        parse("echo $(( 1 + 2+(-3) ))"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo $(( 1 + 2+(-3) ))".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::ArithExpr {
                                expr: Expr::Add(BinaryExpr {
                                    lhs: Box::new(Expr::Literal(1)),
                                    rhs: Box::new(Expr::Add(BinaryExpr {
                                        lhs: Box::new(Expr::Literal(2)),
                                        rhs: Box::new(Expr::Expr(
                                            Box::new(Expr::Literal(-3))
                                        )),
                                    })),
                                }),
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parse("echo $((1+2*$foo-bar))"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo $((1+2*$foo-bar))".into(),
                background: false,
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
                                            }),
                                        })),
                                        rhs: Box::new(Expr::Parameter {
                                            name: "bar".into(),
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
        })
    );
}

#[test]
pub fn test_patterns() {
    assert_eq!(
        parse("echo * a?c"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo * a?c".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::AnyString { quoted: false }]),
                            Word(vec![
                                Span::Literal("a".into()),
                                Span::AnyChar { quoted: false },
                                Span::Literal("c".into()),
                            ]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_comments() {
    assert_eq!(
        parse("foo bar # this is comment\n#comment line\nls -G /tmp # hello world\n"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "foo bar # this is comment".into(),
                    background: false,
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
                    code: "ls -G /tmp # hello world".into(),
                    background: false,
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

    assert_eq!(parse("# Hello"), Err(ParseError::Empty));
    assert_eq!(parse("# Hello\n#World"), Err(ParseError::Empty));
}

#[test]
pub fn test_string_literal() {
    assert_eq!(
        parse("echo \"hello\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo \"hello\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![lit!("echo"), lit!("hello")],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );

    assert_eq!(
        parse("echo -e \"\" \"hello\\tworld\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo -e \"\" \"hello\\tworld\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![lit!("echo"), lit!("-e"), Word(vec![]), lit!("hello\\tworld")],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );

    assert_eq!(
        parse("echo abc\"de\"fg \"1'2''3\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo abc\"de\"fg \"1'2''3\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            lit!("echo"),
                            Word(vec![
                                Span::Literal("abc".into()),
                                Span::Literal("de".into()),
                                Span::Literal("fg".into()),
                            ]),
                            lit!("1'2''3")
                        ],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );
}

#[test]
pub fn test_escape_sequences() {
    assert_eq!(
        parse(r#"echo "\e[1m" \$a"b;\n\"c"d \\n \this_\i\s_\normal"#),
        Ok(Ast {
            terms: vec![Term {
                code: "echo \"\\e[1m\" \\$a\"b;\\n\\\"c\"d \\\\n \\this_\\i\\s_\\normal".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            lit!("echo"),
                            lit!("\\e[1m"),
                            Word(vec![
                                Span::Literal("$a".into()),
                                Span::Literal("b;\\n\"c".into()),
                                Span::Literal("d".into()),
                            ]),
                            lit!("\\n"),
                            lit!("this_is_normal")
                        ],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );
}

#[test]
pub fn test_courner_cases() {
    assert_eq!(parse(""), Err(ParseError::Empty));
    assert_eq!(parse("\n"), Err(ParseError::Empty));
    assert_eq!(parse("\n\n\n"), Err(ParseError::Empty));
    assert_eq!(parse("\n\t\n"), Err(ParseError::Empty));
    assert_eq!(parse("  "), Err(ParseError::Empty));
    assert!(parse(";;;;;;").is_err());
    assert!(parse("||").is_err());
    assert!(parse("& &&").is_err());
}

#[cfg(test)]
mod benchmarks {
    use test::Bencher;
    use super::*;

    #[bench]
    fn newline_parsing_bench(b: &mut Bencher) {
        b.iter(|| {
            parse("\n")
        });
    }

    #[bench]
    fn simple_oneliner_parsing_bench(b: &mut Bencher) {
        b.iter(|| {
            parse("RAILS_ENV=development rails server -p 10022 && echo exited")
        });
    }

    #[bench]
    fn complex_oneliner_parsing_bench(b: &mut Bencher) {
        b.iter(|| {
            parse("func1() { while read line; do echo \"read $line from the prompt!\"; done }")
        });
    }
}
