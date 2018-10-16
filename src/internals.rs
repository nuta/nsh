use std::collections::BTreeMap;
use alias::alias_command;

#[derive(Debug)]
pub enum InternalCommandError {
    NotFound
}

pub enum InternalCommand {
    Alias
}

lazy_static! {
    static ref INTERNAL_COMMANDS: BTreeMap<&'static str, InternalCommand> = {
        let mut commands = BTreeMap::new();
        commands.insert("alias", InternalCommand::Alias);
        commands
    };
}

pub fn run_internal_command(cmd: &str, argv: &Vec<String>) -> Result<i32, InternalCommandError> {
    match INTERNAL_COMMANDS.get(cmd) {
        Some(InternalCommand::Alias) => Ok(alias_command(argv)),
        _ => Err(InternalCommandError::NotFound),
    }
}

