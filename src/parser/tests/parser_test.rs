use nsh_parser::ast::*;
use nsh_parser::lexer::*;
use nsh_parser::parser::*;

fn parse(text: &'static str) -> Result<Ast, ParseError> {
    let lexer = Lexer::new(text.chars());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse(text)?;
    Ok(ast)
}

#[test]
fn simple_command() {
    assert_eq!(parse("echo hello"), Ok(Ast {}));
}
