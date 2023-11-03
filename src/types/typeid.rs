use std::fmt::Display;

#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq)]
pub enum ValueTypeId {
    None,
    Any,
    Bool,
    Num,
    List,
    Str,
    Func,
    Lamda,
}

impl ValueTypeId {
    pub const fn type_name(&self) -> &'static str {
        match self {
            ValueTypeId::Bool => "boolean",
            ValueTypeId::Num => "number",
            ValueTypeId::Func => "function",
            ValueTypeId::List => "list",
            ValueTypeId::Str => "string",
            ValueTypeId::None => "none",
            ValueTypeId::Any => "any",
            ValueTypeId::Lamda => "closure",
        }
    }
}

impl PartialEq for ValueTypeId {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Any) | (Self::Any, _) => true,
            (Self::List, Self::Str) | (Self::Str, Self::List) => true,
            (s, o) => core::mem::discriminant(s) == core::mem::discriminant(o),
        }
    }
}

impl Display for ValueTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.type_name())
    }
}

impl<'v> From<&'v super::Value> for ValueTypeId {
    fn from(val: &'v super::Value) -> Self {
        val.type_id()
    }
}
