use std::{
    any::{Any, TypeId},
    cell::UnsafeCell,
    collections::HashMap,
};

use crate::compile::parser::ast::Stmt;

pub struct CodeAnalyzerData<'a> {
    pub data: UnsafeCell<HashMap<TypeId, Box<dyn Any>>>,
    pub ast: &'a [Stmt],
}

impl<'a> CodeAnalyzerData<'a> {
    pub fn new(ast: &'a [Stmt]) -> Self {
        CodeAnalyzerData {
            data: UnsafeCell::new(HashMap::new()),
            ast,
        }
    }

    pub fn add_new<T: Any + 'static>(&self, value: T) {
        let type_id = TypeId::of::<T>();
        unsafe {
            let data = &mut *self.data.get();
            data.insert(type_id, Box::new(value));
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn get_mut<T: Any + 'static>(&self) -> &mut T {
        unsafe {
            let data_ref = &mut *self.data.get();
            data_ref
                .get_mut(&TypeId::of::<T>())
                .unwrap()
                .downcast_mut::<T>()
                .unwrap()
        }
    }
    pub fn get<T: Any + 'static>(&self) -> &T {
        unsafe {
            let data_ref = &*self.data.get();
            data_ref
                .get(&TypeId::of::<T>())
                .unwrap()
                .downcast_ref::<T>()
                .unwrap()
        }
    }
}
