use std::hash::{Hash, Hasher};

use fxhash::{FxHashMap, FxHasher64};

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
        let mut hasher = FxHasher64::default();
        ty.hash(&mut hasher);
        let id = TyId::T(hasher.finish() as usize);
        self.intern.insert(id, ty);
        id
    }

    pub fn get(&self, id: &TyId) -> Option<&TyContext> {
        self.intern.get(&id)
    }
}
