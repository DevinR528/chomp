use fxhash::FxHashMap;

use super::TyContext;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyId(usize);

#[derive(Debug, Default)]
pub struct TypeIntern {
    intern: FxHashMap<TyId, TyContext>,
}

impl TypeIntern {
    pub fn insert(&mut self, ty: TyContext) -> TyId {
        let id = TyId(self.intern.len());
        self.intern.insert(id, ty);
        id
    }

    pub fn get(&self, id: &TyId) -> &TyContext {
        self.intern.get(&id).unwrap()
    }
}
