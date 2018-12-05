use crate::parser;

#[derive(Debug)]
pub enum Value {
    String(String),
    Array(Vec<String>),
    Function(Box<parser::Command>),
}

#[derive(Debug)]
pub struct Variable {
    value: Value,
}

impl Variable {
    pub fn new(value: Value) -> Variable {
        Variable {
            value,
        }
    }

    #[inline]
    pub fn value(&self) -> &Value {
        &self.value
    }

    // References value as `$foo`.
    pub fn as_str(&self) -> &str {
        match &self.value {
            Value::String(value) => value,
            Value::Function(_) => "(function)",
            // Bash returns the first element in the array.
            Value::Array(elems) => {
                match elems.get(0) {
                    Some(elem) => elem.as_str(),
                    _ => "",
                }
            }
        }
    }

    // References value as `$foo[expr]`.
    pub fn value_at(&self, index: usize) -> &str {
        match &self.value {
            Value::Array(elems) => {
                match elems.get(index) {
                    Some(elem) => elem.as_str(),
                    _ => "",
                }
            },
            _ => "",
        }
    }
}
