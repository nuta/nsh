use crate::parser;

/// A variable value.
#[derive(Debug)]
pub enum Value {
    String(String),
    Array(Vec<String>),
    Function(Box<parser::Command>),
}

/// A shell variable.
#[derive(Debug)]
pub struct Variable {
    // The inner value. `None` represents *null*.
    value: Option<Value>,
}

impl Variable {
    /// Creates a `Variable`. This does not add to the
    /// any scope.
    pub fn new(value: Option<Value>) -> Variable {
        Variable {
            value,
        }
    }

    /// Returns a reference to the inner value.
    #[inline]
    pub fn value(&self) -> &Option<Value> {
        &self.value
    }

    /// References its value as `$foo`.
    pub fn as_str(&self) -> &str {
        match &self.value {
            Some(Value::String(value)) => value,
            Some(Value::Function(_)) => "(function)",
            // Bash returns the first element in the array.
            Some(Value::Array(elems)) => {
                match elems.get(0) {
                    Some(elem) => elem.as_str(),
                    _ => "",
                }
            },
            None => "",
        }
    }

    /// References its value as `$foo[expr]`.
    pub fn value_at(&self, index: usize) -> &str {
        match &self.value {
            Some(Value::Array(elems)) => {
                match elems.get(index) {
                    Some(elem) => elem.as_str(),
                    _ => "",
                }
            },
            _ => "",
        }
    }
}
