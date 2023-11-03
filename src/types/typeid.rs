use std::fmt::Display;

#[repr(u8)]
#[derive(Debug, Clone, Copy, Eq)]
/// TypeId
///
/// type representation id for [Value](crate::Value) as enum for type checking.
///
/// for example [TypeId::Any] is always equal to any type id
/// or [TypeId::Str] is equal to [TypeId::List] because string is just list of character
///
/// # example
/// ```
/// use rzcalc::TypeId;
/// assert_eq!(TypeId::None, TypeId::None);
///
/// assert_eq!(TypeId::Any, TypeId::None);
/// assert_eq!(TypeId::Any, TypeId::Bool);
/// assert_eq!(TypeId::Any, TypeId::Num);
/// assert_eq!(TypeId::Any, TypeId::List);
/// assert_eq!(TypeId::Any, TypeId::Str);
/// assert_eq!(TypeId::Any, TypeId::Func);
/// assert_eq!(TypeId::Any, TypeId::Lamda);
///
/// assert_eq!(TypeId::Func, TypeId::Lamda);
/// assert_eq!(TypeId::Str, TypeId::List);
///
/// assert_ne!(TypeId::Bool, TypeId::Num);
/// assert_ne!(TypeId::None, TypeId::List);
/// assert_ne!(TypeId::Func, TypeId::None);
/// ```
///
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
            (Self::Func, Self::Lamda) | (Self::Lamda, Self::Func) => true,
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
