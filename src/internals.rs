use std::collections::BTreeMap;
use alias::alias_command;
use builtins::{cd_command, exit_command};

#[derive(Debug)]
pub enum InternalCommandError {
    NotFound
}

pub enum InternalCommand {
    Alias,
    Cd,
    Exit,
}

lazy_static! {
    static ref INTERNAL_COMMANDS: BTreeMap<&'static str, InternalCommand> = {
        let mut commands = BTreeMap::new();
        commands.insert("alias", InternalCommand::Alias);
        commands.insert("cd", InternalCommand::Cd);
        commands.insert("exit", InternalCommand::Exit);
        commands
    };
}

pub fn run_internal_command(cmd: &str, argv: &[String]) -> Result<i32, InternalCommandError> {
    match INTERNAL_COMMANDS.get(cmd) {
        Some(InternalCommand::Alias) => Ok(alias_command(argv)),
        Some(InternalCommand::Cd) => Ok(cd_command(argv)),
        Some(InternalCommand::Exit) => Ok(exit_command(argv)),
        _ => Err(InternalCommandError::NotFound),
    }
}

