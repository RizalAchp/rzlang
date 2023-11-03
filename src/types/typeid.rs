use std::fmt::Display;

#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq)]
pub enum TypeId {
    None,
    Any,
    Bool,
    Num,
    List,
    Str,
    Func,
    Lamda,
}

impl TypeId {
    pub const fn type_name(&self) -> &'static str {
        match self {
            TypeId::Bool => "boolean",
            TypeId::Num => "number",
            TypeId::Func => "function",
            TypeId::List => "list",
            TypeId::Str => "string",
            TypeId::None => "none",
            TypeId::Any => "any",
            TypeId::Lamda => "closure",
        }
    }
}

impl PartialEq for TypeId {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Any) | (Self::Any, _) => true,
            (Self::List, Self::Str) | (Self::Str, Self::List) => true,
            (s, o) => core::mem::discriminant(s) == core::mem::discriminant(o),
        }
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.type_name())
    }
}

impl<'v> From<&'v super::Value> for TypeId {
    fn from(val: &'v super::Value) -> Self {
        val.type_id()
    }
}
