use std::marker::PhantomData;

#[macro_export]
macro_rules! idx {
    ($name:ident) => {
        #[derive(Debug, Clone, Eq, PartialEq, Hash, Copy, Default)]
        pub struct $name {
            idx: usize,
        }

        impl Idx for $name {
            fn get(&self) -> usize {
                self.idx
            }

            fn new(idx: usize) -> Self {
                Self { idx }
            }
        }
    };
}

pub trait Idx {
    fn new(idx: usize) -> Self;
    fn get(&self) -> usize;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IdxVec<Index, T>
where
    Index: Idx,
{
    vec: Vec<T>,
    _marker: PhantomData<Index>,
}

impl<Index, T> IdxVec<Index, T>
where
    Index: Idx,
{
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.vec.iter()
    }

    pub fn push(&mut self, value: T) -> Index {
        let new_idx = self.vec.len();
        self.vec.push(value);
        Index::new(new_idx)
    }
}

impl<Index, T> std::ops::Index<Index> for IdxVec<Index, T>
where
    Index: Idx,
{
    type Output = T;

    fn index(&self, index: Index) -> &Self::Output {
        &self.vec[index.get()]
    }
}

impl<Index, T> std::ops::IndexMut<Index> for IdxVec<Index, T>
where
    Index: Idx,
{
    fn index_mut(&mut self, index: Index) -> &mut Self::Output {
        &mut self.vec[index.get()]
    }
}
