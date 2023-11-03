use std::{error::Error, fmt::Display};

use crate::ValueTypeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueErrorKind {
    MismatchType {
        got: ValueTypeId,
        expect: ValueTypeId,
    },
    NotListAlikeType(ValueTypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueError(ValueErrorKind);

impl ValueError {
    pub const fn mismatch_type(got: ValueTypeId, expect: ValueTypeId) -> Self {
        Self(ValueErrorKind::MismatchType { got, expect })
    }
    pub const fn not_list_alike(got: ValueTypeId) -> Self {
        Self(ValueErrorKind::NotListAlikeType(got))
    }
}

impl Error for ValueError {}
impl Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            ValueErrorKind::MismatchType { got, expect } => {
                write!(
                    f,
                    "Mismatch Type, expected type <{expect}>, got type <{got}>",
                )
            }
            ValueErrorKind::NotListAlikeType(tp) => {
                write!(
                    f,
                    "expected list a like type <string> or <list> but got type <{tp}>"
                )
            }
        }
    }
}
