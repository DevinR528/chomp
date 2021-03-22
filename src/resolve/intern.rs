use fxhash::FxHashMap;

use super::{TyContext, TyKind};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TyId {
    Unknown(usize),
    Default,
    T(usize),
}

#[derive(Debug)]
pub struct TypeIntern {
    intern: FxHashMap<TyId, TyContext>,
}

impl Default for TypeIntern {
    fn default() -> Self {
        let mut intern = FxHashMap::default();
        intern.insert(TyId::Default, TyContext { ty: TyKind::Empty });
        Self { intern }
    }
}

impl TypeIntern {
    pub fn insert(&mut self, ty: TyContext) -> TyId {
        let id = TyId::T(self.intern.len());
        self.intern.insert(id, ty);
        id
    }

    pub fn get(&self, id: &TyId) -> &TyContext {
        self.intern.get(&id).unwrap()
    }
}
