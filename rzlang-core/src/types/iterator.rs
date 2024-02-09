use std::ops;

use crate::{TypeId, Value};

pub trait ValueIter: Iterator<Item = Value> {
    fn value_type(&self) -> TypeId;
}

#[derive(Clone, Default, PartialEq, Hash)]
pub struct RangeValue(ops::Range<i64>);
impl ValueIter for RangeValue {
    fn value_type(&self) -> TypeId {
        TypeId::Num
    }
}
impl Iterator for RangeValue {
    type Item = Value;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Value::num)
    }
}
