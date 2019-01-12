use pest::Parser;
use pest::iterators::Pair;
use termion::color::Fg;
use termion::color;
use termion::style;
use std::os::unix::io::RawFd;

#[derive(Parser)]
#[grammar = "shell.pest"]
struct PestShellParser;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectionDirection {
    Input,  // cat < foo.txt or here document
    Output, // cat > foo.txt
    Append, // cat >> foo.txt
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectionType {
    File(Word),
    Fd(RawFd),
    HereDoc(HereDoc),
    /// Since the contents of a here document comes after the current
    /// elemenet (e.g., Command::SimpleCommand), the parser memorizes the
    /// index of here document in `UnresolvedHereDoc` and replace this
    /// with `HereDoc` later. Used internally by the parser.
    UnresolvedHereDoc(usize),
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
    SubShellGroup {
        terms: Vec<Term>,
    },
    Cond(Box<CondExpr>)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CondExpr {
    Or(Box<CondExpr>, Box<CondExpr>),
    And(Box<CondExpr>, Box<CondExpr>),
    // = or ==
    StrEq(Box<CondExpr>, Box<CondExpr>),
    // !=
    StrNe(Box<CondExpr>, Box<CondExpr>),
    // -eq
    Eq(Box<CondExpr>, Box<CondExpr>),
    Ne(Box<CondExpr>, Box<CondExpr>),
    Lt(Box<CondExpr>, Box<CondExpr>),
    Le(Box<CondExpr>, Box<CondExpr>),
    Gt(Box<CondExpr>, Box<CondExpr>),
    Ge(Box<CondExpr>, Box<CondExpr>),
    Word(Word),
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
pub enum ProcSubstType {
    // <(echo hello)
    StdoutToFile,
    // >(grep hello)
    FileToStdin,
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
    // <(echo hello)
    ProcSubst { body: Vec<Term>, subst_type: ProcSubstType },
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

/// Contains heredoc body. The outer Vec represents lines and
/// `Vec<Word>` represents the contents of a line.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HereDoc(Vec<Vec<Word>>);

impl HereDoc {
    pub fn lines(&self) -> &[Vec<Word>] {
        &self.0
    }
}

macro_rules! wsnl {
    ($self:expr, $pairs:expr) => {
        if let Some(next) = $pairs.next() {
            match next.as_rule() {
                Rule::newline => {
                    $self.visit_newline(next);
                    $pairs.next()
                },
                _ => Some(next),
            }
        } else {
            None
        }
    }
}

struct ShellParser {
    heredocs: Vec<HereDoc>,
    next_heredoc_index: usize,
}

impl ShellParser {
    pub fn new() -> ShellParser {
        ShellParser {
            heredocs: Vec::new(),
            next_heredoc_index: 0,
        }
    }

    // proc_subst_direction = { "<" | ">" }
    // proc_subst_span = !{ proc_subst_direction ~ "(" ~ compound_list ~ ")" }
    fn visit_proc_subst_span(&mut self, pair: Pair<Rule>) -> Span {
        let mut inner = pair.into_inner();
        let direction = inner.next().unwrap();
        let compound_list = inner.next().unwrap();

        let subst_type = match direction.as_span().as_str() {
            "<(" => ProcSubstType::StdoutToFile,
            ">(" => ProcSubstType::FileToStdin,
            _ => unreachable!()
        };

        let body = self.visit_compound_list(compound_list);
        Span::ProcSubst { body, subst_type }
    }

    // command_span = !{ "$(" ~ compound_list ~ ")" }
    fn visit_command_span(&mut self, pair: Pair<Rule>, quoted: bool) -> Span {
        let body = self.visit_compound_list(pair.into_inner().next().unwrap());
        Span::Command { body, quoted}
    }

    // param_ex_span = { "$" ~ "{" ~ length_op ~ expandable_var_name ~ param_opt? ~ "}" }
    fn visit_param_ex_span(&mut self, pair: Pair<Rule>, quoted: bool) -> Span {
        let mut inner = pair.into_inner();
        let length_op = inner.next().unwrap().as_span().as_str().to_owned();
        let name = inner.next().unwrap().as_span().as_str().to_owned();
        let index = inner.next().unwrap().into_inner().next().map(|p| self.visit_expr(p));

        let op = if length_op == "#" {
            ExpansionOp::Length
        } else if let Some(param_opt) = inner.next() {
            let mut inner = param_opt.into_inner();
            let symbol = inner.next().unwrap().as_span().as_str();
            let default_word = inner.next().map(|p| self.visit_word(p)).unwrap_or_else(|| Word(vec![]));
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
    fn visit_param_span(&mut self, pair: Pair<Rule>, quoted: bool) -> Span {
        let name = pair.into_inner().next().unwrap().as_span().as_str().to_owned();
        let op = ExpansionOp::GetOrEmpty;
        Span::Parameter { name, op, quoted }
    }

    // factor = { sign ~ primary }
    // primary = _{ num | ("$"? ~ var_name) |  ("(" ~ expr ~ ")") }
    // num = { ASCII_DIGIT+ }
    fn visit_factor(&mut self, pair: Pair<Rule>) -> Expr {
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
            Rule::expr => Expr::Expr(Box::new(self.visit_expr(primary))),
            _ => unreachable!(),
        }
    }

    // term = { factor ~ (factor_op ~ expr)? }
    // factor_op = { "*" | "/" }
    fn visit_term(&mut self, pair: Pair<Rule>) -> Expr {
        let mut inner = pair.into_inner();
        let lhs = self.visit_factor(inner.next().unwrap());
        if let Some(op) = inner.next() {
            let rhs = self.visit_term(inner.next().unwrap());
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
    fn visit_expr(&mut self, pair: Pair<Rule>) -> Expr {
        let mut inner = pair.into_inner();
        let lhs = self.visit_term(inner.next().unwrap());
        if let Some(op) = inner.next() {
            let rhs = self.visit_expr(inner.next().unwrap());
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
    fn visit_expr_span(&mut self, pair: Pair<Rule>) -> Span {
        let expr = self.visit_expr(pair.into_inner().next().unwrap());
        Span::ArithExpr { expr }
    }

    // `a\b\$cd' -> `echo ab$cd'
    fn visit_escape_sequences(&mut self, pair: Pair<Rule>, escaped_chars: Option<&str>) -> String {
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

    fn visit_cond_primary(&mut self, pair: Pair<Rule>) -> Box<CondExpr> {
        let mut inner = pair.into_inner();
        let primary = inner.next().unwrap();
        match primary.as_rule() {
            Rule::word => {
                let word = self.visit_word(primary);
                Box::new(CondExpr::Word(word))
            },
            Rule::cond_expr => {
                self.visit_cond_expr(primary)
            },
            _ => unreachable!(),
        }
    }

    fn visit_cond_term(&mut self, pair: Pair<Rule>) -> Box<CondExpr> {
        let mut inner = pair.into_inner();
        let lhs = self.visit_cond_primary(inner.next().unwrap());
        if let Some(op) = inner.next() {
            let rhs = self.visit_cond_term(inner.next().unwrap());
            match op.as_span().as_str() {
                "-eq" => Box::new(CondExpr::Eq(lhs, rhs)),
                "-ne" => Box::new(CondExpr::Ne(lhs, rhs)),
                "-lt" => Box::new(CondExpr::Lt(lhs, rhs)),
                "-le" => Box::new(CondExpr::Le(lhs, rhs)),
                "-gt" => Box::new(CondExpr::Gt(lhs, rhs)),
                "-ge" => Box::new(CondExpr::Ge(lhs, rhs)),
                "==" | "=" => Box::new(CondExpr::StrEq(lhs, rhs)),
                "!=" => Box::new(CondExpr::StrNe(lhs, rhs)),
                _ => unimplemented!()
            }
        } else {
            lhs
        }
    }

    fn visit_cond_and(&mut self, pair: Pair<Rule>) -> Box<CondExpr> {
        let mut inner = pair.into_inner();
        let lhs = self.visit_cond_term(inner.next().unwrap());
        if let Some(rhs) = inner.next() {
            let rhs = self.visit_cond_and(rhs);
            Box::new(CondExpr::And(lhs, rhs))
        } else {
            lhs
        }
    }

    fn visit_cond_or(&mut self, pair: Pair<Rule>) -> Box<CondExpr> {
        let mut inner = pair.into_inner();
        let lhs = self.visit_cond_and(inner.next().unwrap());
        if let Some(rhs) = inner.next() {
            let rhs = self.visit_cond_or(rhs);
            Box::new(CondExpr::Or(lhs, rhs))
        } else {
            lhs
        }
    }

    // cond_expr =  _{ cond_or }
    fn visit_cond_expr(&mut self, pair: Pair<Rule>) -> Box<CondExpr> {
        self.visit_cond_or(pair)
    }

    // cond_ex = { "[[" ~ cond_expr ~ "]]" }
    fn visit_cond_ex(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let expr = self.visit_cond_expr(inner.next().unwrap());

        Command::Cond(expr)
    }

    // word = ${ (tilde_span | span) ~ span* }
    // span = _{
    //     double_quoted_span
    //     | single_quoted_span
    //     | literal_span
    //     | any_string_span
    //     | any_char_span
    //     | expr_span
    //     | command_span
    //     | backtick_span
    //     | param_ex_span
    //     | param_span
    // }
    fn visit_word(&mut self, pair: Pair<Rule>) -> Word {
        assert_eq!(pair.as_rule(), Rule::word);

        let mut spans = Vec::new();
        for span in pair.into_inner() {
            match span.as_rule() {
                Rule::literal_span => {
                    spans.push(Span::Literal(self.visit_escape_sequences(span, None)));
                },
                Rule::double_quoted_span => {
                    for span_in_quote in span.into_inner() {
                        match span_in_quote.as_rule() {
                            Rule::literal_in_double_quoted_span => {
                                spans.push(Span::Literal(self.visit_escape_sequences(span_in_quote, Some("\"`$"))));
                            }
                            Rule::backtick_span => spans.push(self.visit_command_span(span_in_quote, true)),
                            Rule::command_span => spans.push(self.visit_command_span(span_in_quote, true)),
                            Rule::param_span => spans.push(self.visit_param_span(span_in_quote, true)),
                            Rule::param_ex_span => spans.push(self.visit_param_ex_span(span_in_quote, true)),
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
                Rule::expr_span => spans.push(self.visit_expr_span(span)),
                Rule::param_span => spans.push(self.visit_param_span(span, false)),
                Rule::param_ex_span => spans.push(self.visit_param_ex_span(span, false)),
                Rule::backtick_span => spans.push(self.visit_command_span(span, false)),
                Rule::command_span => spans.push(self.visit_command_span(span, false)),
                Rule::proc_subst_span => spans.push(self.visit_proc_subst_span(span)),
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
    // redirect_to_fd = ${ "&" ~ ASCII_DIGIT* }
    // redirect = { fd ~ redirect_direction ~ (word | redirect_to_fd) }
    fn visit_redirect(&mut self, pair: Pair<Rule>) -> Redirection {
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
        let target = match target.as_rule() {
            Rule::word => RedirectionType::File(self.visit_word(target)),
            Rule::redirect_to_fd => {
                let target_fd = target.into_inner().next().unwrap().as_span().as_str().parse().unwrap();
                RedirectionType::Fd(target_fd)
            },
            _ => unreachable!()
        };

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
    fn visit_assignment(&mut self, pair: Pair<Rule>) -> Assignment {
        let mut inner = pair.into_inner();

        let name = inner.next().unwrap().as_span().as_str().to_owned();
        let index = inner.next().unwrap().into_inner().next().map(|p| self.visit_expr(p));
        let initializer = inner.next().unwrap().into_inner().next().unwrap();
        match initializer.as_rule() {
            Rule::string_initializer => {
                let word = Initializer::String(self.visit_word(initializer.into_inner().next().unwrap()));
                Assignment { name, initializer: word, index }
            },
            Rule::array_initializer => {
                let word = Initializer::Array(initializer.into_inner().map(|p| self.visit_word(p)).collect());
                let index = None;
                Assignment { name, initializer: word, index }
            },
            _ => unreachable!()
        }
    }

    fn fetch_and_add_heredoc_index(&mut self) -> usize {
        let old = self.next_heredoc_index;
        self.next_heredoc_index += 1;
        old
    }

    fn visit_simple_command(&mut self, pair: Pair<Rule>) -> Command {
        assert_eq!(pair.as_rule(), Rule::simple_command);

        let mut argv = Vec::new();
        let mut redirects = Vec::new();

        let mut inner = pair.into_inner();
        let assignments_pairs = inner.next().unwrap().into_inner();
        let argv0 = inner.next().unwrap().into_inner().next().unwrap();
        let args = inner.next().unwrap().into_inner();

        argv.push(self.visit_word(argv0));
        for word_or_redirect in args {
            match word_or_redirect.as_rule() {
                Rule::word => argv.push(self.visit_word(word_or_redirect)),
                Rule::redirect => redirects.push(self.visit_redirect(word_or_redirect)),
                Rule::heredoc => {
                    redirects.push(Redirection {
                        fd: 0, // stdin
                        direction: RedirectionDirection::Input,
                        target: RedirectionType::UnresolvedHereDoc(self.fetch_and_add_heredoc_index())
                    });
                }
                _ => unreachable!()
            }
        }

        let mut assignments = Vec::new();
        for assignment in assignments_pairs {
            assignments.push(self.visit_assignment(assignment));
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
    fn visit_if_command(&mut self, pair: Pair<Rule>) -> Command {
        assert_eq!(pair.as_rule(), Rule::if_command);

        let mut inner = pair.into_inner();
        let condition = self.visit_compound_list(inner.next().unwrap());
        let then_part = self.visit_compound_list(inner.next().unwrap());
        let mut elif_parts = Vec::new();
        let mut else_part = None;
        for elif in inner {
            match elif.as_rule() {
                Rule::elif_part => {
                    let mut inner = elif.into_inner();
                    let condition = self.visit_compound_list(inner.next().unwrap());
                    let then_part = self.visit_compound_list(inner.next().unwrap());
                    elif_parts.push(ElIf { condition, then_part });
                },
                Rule::else_part => {
                    let mut inner = elif.into_inner();
                    let body = self.visit_compound_list(inner.next().unwrap());
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
    fn visit_case_command(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let word = self.visit_word(inner.next().unwrap());
        let mut cases = Vec::new();
        while let Some(case) = wsnl!(self, inner) {
            match case.as_rule() {
                Rule::case_item => {
                    let mut inner = case.into_inner();
                    let patterns = inner.next().unwrap().into_inner().map(|w| self.visit_word(w)).collect();
                    let body = self.visit_compound_list(inner.next().unwrap());
                    cases.push(CaseItem { patterns, body });
                },
                Rule::newline => self.visit_newline(case),
                _ => unreachable!(),
            }
        }

        Command::Case { word, cases }
    }

    // while_command = {
    //     "while" ~ compound_list ~ "do" ~ compound_list ~ "done"
    // }
    fn visit_while_command(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let condition = self.visit_compound_list(inner.next().unwrap());
        let body = self.visit_compound_list(inner.next().unwrap());

        Command::While {
            condition,
            body,
        }
    }

    // word_list = { word* }
    // for_command = {
    //     "for" ~ var_name ~ "in" ~ word_list ~ "do" ~ compound_list ~ "done"
    // }
    fn visit_for_command(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let var_name = inner.next().unwrap().as_span().as_str().to_owned();
        let words = inner.next().unwrap().into_inner().map(|w| self.visit_word(w)).collect();
        let compound_list = wsnl!(self, inner).unwrap();
        let body = self.visit_compound_list(compound_list);

        Command::For {
            var_name,
            words,
            body,
        }
    }

    // function_definition = {
    //     ("function")? ~ var_name ~ "()" ~ wsnl ~ compound_list
    // }
    fn visit_function_definition(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let name = inner.next().unwrap().as_span().as_str().to_owned();
        let compound_list = wsnl!(self, inner).unwrap();
        let body = Box::new(self.visit_command(compound_list));

        Command::FunctionDef {
            name,
            body,
        }
    }

    // local_definition = { "local" ~ (assignment | var_name)+ }
    fn visit_local_definition(&mut self, pair: Pair<Rule>) -> Command {
        let mut declarations = Vec::new();
        for inner in pair.into_inner() {
            declarations.push(match inner.as_rule() {
                Rule::assignment => LocalDeclaration::Assignment(self.visit_assignment(inner)),
                Rule::var_name => LocalDeclaration::Name(inner.as_span().as_str().to_owned()),
                _ => unreachable!(),
            });
        }

        Command::LocalDef { declarations }
    }

    // assignment_command = { assignment+ }
    fn visit_assignment_command(&mut self, pair: Pair<Rule>) -> Command {
        let assignments = pair.into_inner().map(|inner| self.visit_assignment(inner)).collect();
        Command::Assignment { assignments }
    }

    // group = { "{" ~ compound_list ~ "}" }
    fn visit_group_command(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let terms = self.visit_compound_list(inner.next().unwrap());

        Command::Group { terms }
    }

    // group = { "(" ~ compound_list ~ ")" }
    fn visit_subshell_group_command(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let terms = self.visit_compound_list(inner.next().unwrap());

        Command::SubShellGroup { terms }
    }

    // return = { return ~ num? }
    fn visit_return_command(&mut self, pair: Pair<Rule>) -> Command {
        let mut inner = pair.into_inner();
        let status = inner.next().map(|status| status.as_span().as_str().parse().unwrap());

        Command::Return { status }
    }

    // command = {
    //     if_command
    //     | case_command
    //     | while_command
    //     | for_command
    //     | break_command
    //     | continue_command
    //     | return_command
    //     | local_definition
    //     | function_definition
    //     | group
    //     | cond_ex
    //     | simple_command
    //     | assignment_command
    // }
    fn visit_command(&mut self, pair: Pair<Rule>) -> Command {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::simple_command => { self.visit_simple_command(inner) },
            Rule::if_command => { self.visit_if_command(inner) },
            Rule::while_command => { self.visit_while_command(inner) },
            Rule::for_command => { self.visit_for_command(inner) },
            Rule::case_command => { self.visit_case_command(inner) },
            Rule::group => { self.visit_group_command(inner) },
            Rule::subshell_group => { self.visit_subshell_group_command(inner) },
            Rule::break_command => { Command::Break },
            Rule::continue_command => { Command::Continue },
            Rule::return_command => { self.visit_return_command(inner) },
            Rule::assignment_command => { self.visit_assignment_command(inner) },
            Rule::local_definition => { self.visit_local_definition(inner) },
            Rule::function_definition => { self.visit_function_definition(inner) },
            Rule::cond_ex => { self.visit_cond_ex(inner) },
            _ => unreachable!()
        }
    }

    // pipeline = { command ~ ((!("||") ~ "|") ~ wsnl? ~ command)* }
    fn visit_pipeline(&mut self, pair: Pair<Rule>) -> Vec<Command> {
        let mut commands = Vec::new();
        let mut inner = pair.into_inner();
        while let Some(command) = wsnl!(self, inner) {
            commands.push(self.visit_command(command));
        }

        commands
    }

    // and_or_list = { pipeline ~ (and_or_list_sep ~ wsnl? ~ and_or_list)* }
    // and_or_list_sep = { "||" | "&&" }
    fn visit_and_or_list(&mut self, pair: Pair<Rule>, run_if: RunIf) -> Vec<Pipeline> {
        let mut terms = Vec::new();
        let mut inner = pair.into_inner();
        if let Some(pipeline) = inner.next() {
            let commands = self.visit_pipeline(pipeline);
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

            if let Some(rest) = wsnl!(self, inner) {
                terms.extend(self.visit_and_or_list(rest, next_run_if));
            }
        }

        terms
    }

    // compound_list = { compound_list_inner ~ (compound_list_sep ~ wsnl? ~ compound_list)* }
    // compound_list_sep = { (!(";;") ~ ";") | !("&&") ~ "&" | "\n" }
    // empty_line = { "" }
    // compound_list_inner = _{ and_or_list | empty_line }
    fn visit_compound_list(&mut self, pair: Pair<Rule>) -> Vec<Term> {
        let mut terms = Vec::new();
        let mut inner = pair.into_inner();
        if let Some(and_or_list) = inner.next() {
            let mut background = false;
            let mut rest = None;
            while let Some(sep_or_rest) = wsnl!(self, inner) {
                match sep_or_rest.as_rule() {
                    Rule::compound_list => {
                        rest = Some(sep_or_rest);
                        break;
                    },
                    _ => {
                        let sep = sep_or_rest.into_inner().next().unwrap();
                        match sep.as_rule() {
                            Rule::background => {
                                background = true;
                            },
                            Rule::newline => {
                                self.visit_newline(sep);
                            },
                            Rule::seq_sep => (),
                            _ => (),
                        }
                    }
                }
            }

            if and_or_list.as_rule() == Rule::and_or_list {
                let code = and_or_list.as_str().to_owned().trim().to_owned();
                let pipelines = self.visit_and_or_list(and_or_list, RunIf::Always);
                terms.push(Term {
                    code,
                    pipelines,
                    background,
                });
            }

            if let Some(rest) = rest {
                terms.extend(self.visit_compound_list(rest));
            }
        }

        terms
    }

    pub fn visit_newline(&mut self, pair: Pair<Rule>) {
        if let Some(newline_inner) = pair.into_inner().next() {
           if Rule::heredoc_body == newline_inner.as_rule() {
                let lines: Vec<Vec<Word>> = newline_inner
                    .into_inner()
                    .map(|line| line.into_inner().map(|w| self.visit_word(w)).collect())
                    .collect();
                self.heredocs.push(HereDoc(lines));
           }
        }
    }

    /// Replaces all `UnresolvedHereDoc` with `HereDoc`.
    pub fn resolve_heredocs(&self, ast: Ast) -> Ast {
        let mut new_terms = Vec::new();
        for term in ast.terms {
            let mut new_pipelines = Vec::new();
            for pipeline in term.pipelines {
                let mut new_commands = Vec::new();
                for command in pipeline.commands {
                    match command {
                        Command::SimpleCommand { argv, redirects, assignments } => {
                            let mut new_redirects = Vec::new();
                            for redirect in redirects {
                                match redirect.target {
                                    RedirectionType::UnresolvedHereDoc(index) => {
                                        new_redirects.push(Redirection {
                                            fd: 0, // stdin
                                            direction: RedirectionDirection::Input,
                                            target: RedirectionType::HereDoc(self.heredocs[index].clone())
                                        })
                                    },
                                    _ => new_redirects.push(redirect)
                                }
                            }

                            new_commands.push(Command::SimpleCommand {
                                argv,
                                redirects: new_redirects,
                                assignments,
                            })
                        }
                        _ => new_commands.push(command)
                    }
                }

                new_pipelines.push(Pipeline {
                    run_if: pipeline.run_if,
                    commands: new_commands,
                });
            }

            new_terms.push(Term {
                code: term.code,
                background: term.background,
                pipelines: new_pipelines,
            });
        }

        Ast { terms: new_terms }
    }

    /// Parses a shell script.
    pub fn parse(&mut self, script: &str) -> Result<Ast, ParseError> {
        match PestShellParser::parse(Rule::script, script) {
            Ok(mut pairs) => {
                let terms = self.visit_compound_list(pairs.next().unwrap());

                if terms.is_empty() {
                    Err(ParseError::Empty)
                } else {
                    Ok(self.resolve_heredocs(Ast { terms }))
                }
            }
            Err(err) => {
                Err(ParseError::Fatal(err.to_string()))
            }
        }
    }

    /// Dumps the parsed pairs for debbuging.
    #[allow(unused)]
    fn dump(&mut self, pairs: pest::iterators::Pairs<Rule>, level: usize) {
        for pair in pairs {
            for _ in 0..level {
                print!("  ");
            }
            println!("{}{}{}{:?}{}: {:?}",
                style::Reset, style::Bold, Fg(color::Magenta),
                pair.as_rule(), style::Reset, pair.as_span().as_str());

            self.dump(pair.into_inner(), level + 1);
        }
    }

    /// Parses and dumps a shell  script for debbuging.
    #[allow(unused)]
    pub fn parse_and_dump(&mut self, script: &str) {
        let merged_script = script.to_string().replace("\\\n", "");
        let pairs = PestShellParser::parse(Rule::script, &merged_script).unwrap_or_else(|e| panic!("{}", e));
        self.dump(pairs, 0);
    }
}

pub fn parse(script: &str) -> Result<Ast, ParseError> {
    let mut parser = ShellParser::new();
    parser.parse(script)
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
        parse("false || false && echo unreachable; echo u\\nreachable"),
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
                    code: "echo u\\nreachable".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            argv: literal_word_vec!["echo", "unreachable"],
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
        parse("ls -G <foo.txt >> bar.txt 2> baz.txt 4>&2"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls -G <foo.txt >> bar.txt 2> baz.txt 4>&2".into(),
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
                                direction: RedirectionDirection::Append,
                                fd: 1,
                                target: RedirectionType::File(lit!("bar.txt")),
                            },
                            Redirection {
                                direction: RedirectionDirection::Output,
                                fd: 2,
                                target: RedirectionType::File(lit!("baz.txt")),
                            },
                            Redirection {
                                direction: RedirectionDirection::Output,
                                fd: 4,
                                target: RedirectionType::Fd(2),
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
        parse("( echo hello; echo world; )"),
        Ok(Ast {
            terms: vec![Term {
                code: "( echo hello; echo world; )".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SubShellGroup {
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

#[test]
pub fn test_process_substitution() {
    assert_eq!(
        parse("cat <(echo hello from a file)"),
        Ok(Ast {
            terms: vec![Term {
                code: "cat <(echo hello from a file)".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        argv: vec![
                            Word(vec![Span::Literal("cat".into())]),
                            Word(vec![Span::ProcSubst {
                                subst_type: ProcSubstType::StdoutToFile,
                                body: vec![Term {
                                    code: "echo hello from a file".into(),
                                    background: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            argv: vec![
                                                Word(vec![Span::Literal("echo".into())]),
                                                Word(vec![Span::Literal("hello".into())]),
                                                Word(vec![Span::Literal("from".into())]),
                                                Word(vec![Span::Literal("a".into())]),
                                                Word(vec![Span::Literal("file".into())]),
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
}

#[test]
pub fn test_cond_ex() {
    assert_eq!(
        parse("hello=world; [[ $hello == world ]]"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "hello=world".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::Assignment {
                            assignments: vec![
                                Assignment {
                                    name: "hello".into(),
                                    initializer: Initializer::String(lit!("world")),
                                    index: None,
                                }
                            ],
                        }],
                    }],
                },
                Term {
                    code: "[[ $hello == world ]]".into(),
                    background: false,
                    pipelines: vec![
                        Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![
                                Command::Cond(Box::new(
                                    CondExpr::StrEq(
                                        Box::new(CondExpr::Word(
                                            Word(vec![Span::Parameter {
                                                name: "hello".into(),
                                                op: ExpansionOp::GetOrEmpty,
                                                quoted: false,
                                            }])
                                        )),
                                        Box::new(CondExpr::Word(
                                            lit!("world"),
                                        ))
                                    )
                                ))
                            ]
                        }
                    ],
                },
            ]
        })
    );
}

#[test]
pub fn test_heredoc() {
    assert_eq!(
        parse(concat!(
            "cat << EOF > file.txt\n",
            "hello world\n",
            "from\n",
            "heredoc!\n",
            "EOF\n"
        )),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "cat << EOF > file.txt".into(),
                    background: false,
                    pipelines: vec![
                        Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![
                                Command::SimpleCommand {
                                    argv: vec![
                                        lit!("cat")
                                    ],
                                    redirects: vec![
                                        Redirection {
                                            fd: 0,
                                            direction: RedirectionDirection::Input,
                                            target: RedirectionType::HereDoc(HereDoc(vec![
                                                vec![lit!("hello"), lit!("world")],
                                                vec![lit!("from")],
                                                vec![lit!("heredoc!")],
                                            ])),
                                        },
                                        Redirection {
                                            fd: 1,
                                            direction: RedirectionDirection::Output,
                                            target: RedirectionType::File(lit!("file.txt")),
                                        }
                                    ],
                                    assignments: vec![],
                                }
                            ]
                        }
                    ],
                },
            ]
        })
    );
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
