use std::hash::{Hash, Hasher};
use std::{
    fmt, panic,
    sync::{Arc, Mutex},
};

use base_db::{
    fixture::WithFixture,
    salsa::{self, Durability},
    AnchoredPath, Change, CrateDisplayName, CrateGraph, CrateId, Env, FileId, FileLoader,
    FileLoaderDelegate, FileSet, SourceDatabase, SourceDatabaseExt, SourceRoot, SourceRootId,
    Upcast, VfsPath,
};
use chalk_ir::FnDefId;
use hir::{ItemInNs, Visibility};
use hir_def::{
    body::{Body, BodySourceMap},
    data::{ConstData, FunctionData},
    db::DefDatabase,
    expr::{Expr, Statement},
    item_scope::ItemScope,
    nameres::DefMap,
    per_ns::PerNs,
    src::HasSource,
    visibility::RawVisibility,
    AssocItemId, DefWithBodyId, LocalModuleId, Lookup, ModuleDefId, ModuleId,
};
use hir_expand::{db::AstDatabase, InFile};
use hir_ty::{
    db::HirDatabase, display::HirDisplay, CallableDefId, InferenceResult, Interner, TyKind,
};
use ide_db::symbol_index::{self, SymbolsDatabase};
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::{
    ast::{IdentPat, NameOwner},
    AstNode, SyntaxNode, TextRange,
};

mod inst;
mod low;

use low::FnBuilder;

#[salsa::database(
    base_db::SourceDatabaseExtStorage,
    base_db::SourceDatabaseStorage,
    hir_expand::db::AstDatabaseStorage,
    hir_def::db::InternDatabaseStorage,
    hir_def::db::DefDatabaseStorage,
    hir_ty::db::HirDatabaseStorage,
    symbol_index::SymbolsDatabaseStorage
)]
#[derive(Default)]
pub struct TypeResolver {
    storage: salsa::Storage<TypeResolver>,
    events: Mutex<Option<Vec<salsa::Event>>>,
}

impl fmt::Debug for TypeResolver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}

impl Upcast<dyn AstDatabase> for TypeResolver {
    fn upcast(&self) -> &(dyn AstDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn DefDatabase> for TypeResolver {
    fn upcast(&self) -> &(dyn DefDatabase + 'static) {
        &*self
    }
}

impl Upcast<dyn HirDatabase> for TypeResolver {
    fn upcast(&self) -> &(dyn HirDatabase + 'static) {
        &*self
    }
}

impl salsa::Database for TypeResolver {
    fn salsa_event(&self, event: salsa::Event) {
        let mut events = self.events.lock().unwrap();
        if let Some(events) = &mut *events {
            events.push(event);
        }
    }
}

impl salsa::ParallelDatabase for TypeResolver {
    fn snapshot(&self) -> salsa::Snapshot<TypeResolver> {
        salsa::Snapshot::new(TypeResolver {
            storage: self.storage.snapshot(),
            events: Default::default(),
        })
    }
}

impl FileLoader for TypeResolver {
    fn file_text(&self, file_id: FileId) -> Arc<String> {
        FileLoaderDelegate(self).file_text(file_id)
    }
    fn resolve_path(&self, path: AnchoredPath) -> Option<FileId> {
        FileLoaderDelegate(self).resolve_path(path)
    }
    fn relevant_crates(&self, file_id: FileId) -> Arc<FxHashSet<CrateId>> {
        FileLoaderDelegate(self).relevant_crates(file_id)
    }
}

impl TypeResolver {
    pub fn parse_file(input: &str) -> (Self, FileId) {
        Self::with_single_file(input)
    }

    /// Parse "files" into a `TypeResolver` that can walk the items in a crate.
    ///
    /// The crate root must be the first file.
    pub fn parse_crate(inputs: Vec<&str>) -> (Self, Vec<FileId>) {
        let mut ids = vec![];
        let mut db = TypeResolver::default();

        let mut files = FileSet::default();
        for (i, t) in inputs
            .iter()
            .enumerate()
            .map(|(i, t)| (FileId(i as u32), t))
        {
            ids.push(i);
            files.insert(i, VfsPath::new_virtual_path("/test".to_string()));
        }

        let root = SourceRoot::new_library(files);
        // Since we will never change the DB set high durability.
        let durability = Durability::HIGH;

        // TODO: loop over crates when we do that...
        let lib_roots = root
            .iter()
            .enumerate()
            .map(|(i, _)| SourceRootId(i as u32))
            .collect::<FxHashSet<_>>();

        db.set_library_roots_with_durability(Arc::new(lib_roots), durability);

        for (i, file_id) in root.iter().enumerate() {
            db.set_file_source_root_with_durability(file_id, SourceRootId(i as u32), durability);
        }

        for (file_id, text) in inputs
            .iter()
            .enumerate()
            .map(|(i, t)| (FileId(i as u32), t))
        {
            let source_root_id = db.file_source_root(file_id);
            // XXX: can't actually remove the file, just reset the text
            db.set_file_text_with_durability(file_id, Arc::new(text.to_string()), durability)
        }

        let mut crate_graph = CrateGraph::default();
        crate_graph.add_crate_root(
            ids[0],
            base_db::Edition::Edition2018,
            Some(CrateDisplayName::from_canonical_name("test".to_string())),
            Default::default(),
            Env::default(),
            vec![],
        );

        db.set_crate_graph_with_durability(Arc::new(crate_graph), durability);

        (db, ids)
    }

    pub fn walk_crate(&self, id: FileId) {
        let mod_ = self.module_from_file(id);
        let def_map = self.def_map(mod_);
        let mut def_ids = vec![];
        self.walk_module(&def_map, mod_.local_id, &mut def_ids);

        def_ids.sort_by_key(|def| match def {
            DefWithBodyId::FunctionId(it) => {
                let loc = it.lookup(self);
                loc.source(self).value.syntax().text_range().start()
            }
            DefWithBodyId::ConstId(it) => {
                let loc = it.lookup(self);
                loc.source(self).value.syntax().text_range().start()
            }
            DefWithBodyId::StaticId(it) => {
                let loc = it.lookup(self);
                loc.source(self).value.syntax().text_range().start()
            }
        });

        for def in def_ids {
            let (_body, source_map) = self.body_with_source_map(def);
            let infer = self.infer(def);
            println!("{:?}", _body)
        }
    }

    pub fn walk_module(
        &self,
        def_map: &DefMap,
        id: LocalModuleId,
        def_ids: &mut Vec<DefWithBodyId>,
    ) {
        let mut mod_name = vec![];
        let mut curr_mod = id;

        for decl in def_map[id].scope.declarations() {
            match decl {
                ModuleDefId::FunctionId(it) => {
                    let func: Arc<FunctionData> = self.function_data(it);
                    let (body, source_map): (Arc<Body>, Arc<BodySourceMap>) =
                        self.body_with_source_map(it.into());
                    let infer: Arc<InferenceResult> = self.infer(it.into());

                    let mut curr = decl;
                    while let Some(mod_) = curr.module(self) {
                        if let Some(parent) = mod_.containing_module(self) {
                            curr = parent.into();
                            if let Some((name, loc)) = def_map[parent.local_id]
                                .children
                                .iter()
                                .find(|(name, id)| &mod_.local_id == *id)
                            {
                                mod_name.push(name.clone());
                            }
                        } else {
                            break;
                        }
                    }

                    mod_name.reverse();

                    // Build the items in the body first so they can be called from the
                    // func we are about to build.
                    self.walk_body(&body, def_ids);

                    let func = self.build_fn(&func, &body, &source_map, &infer, &mod_name);
                }
                ModuleDefId::ConstId(it) => {
                    let konst: Arc<ConstData> = self.const_data(it);
                    println!("{:?}", konst);

                    let def = it.into();
                    def_ids.push(def);

                    let body = self.body(def);
                    self.walk_body(&body, def_ids);
                }
                ModuleDefId::StaticId(it) => {
                    let def = it.into();
                    def_ids.push(def);
                    let body = self.body(def);
                    self.walk_body(&body, def_ids);
                }
                ModuleDefId::TraitId(it) => {
                    let trait_data = self.trait_data(it);
                    for &(_, item) in trait_data.items.iter() {
                        match item {
                            AssocItemId::FunctionId(it) => def_ids.push(it.into()),
                            AssocItemId::ConstId(it) => def_ids.push(it.into()),
                            AssocItemId::TypeAliasId(_) => {}
                        }
                    }
                }
                ModuleDefId::ModuleId(it) => self.walk_module(def_map, it.local_id, def_ids),
                thing => todo!("{:?}", thing),
            }
        }

        for impl_id in def_map[id].scope.impls() {
            let impl_data = self.impl_data(impl_id);
            for &item in impl_data.items.iter() {
                match item {
                    AssocItemId::FunctionId(it) => def_ids.push(it.into()),
                    AssocItemId::ConstId(it) => def_ids.push(it.into()),
                    AssocItemId::TypeAliasId(_) => {}
                }
            }
        }
    }

    pub fn walk_body(&self, body: &Body, def_ids: &mut Vec<DefWithBodyId>) {
        for def_map in body
            .block_scopes
            .iter()
            .filter_map(|blk_id| self.block_def_map(*blk_id))
        {
            for (mod_id, _) in def_map.modules() {
                self.walk_module(&def_map, mod_id, def_ids)
            }
        }
    }

    pub fn def_map(&self, module: ModuleId) -> Arc<DefMap> {
        module.def_map(self)
    }

    fn build_fn<'a>(
        &self,
        func: &FunctionData,
        body: &'a Body,
        map: &BodySourceMap,
        infer: &'a InferenceResult,
        path: &[hir::Name],
    ) -> FnBuilder<'a> {
        let mut calls = FxHashMap::default();
        if let Expr::Block {
            id,
            statements,
            tail,
            label,
        } = &body[body.body_expr]
        {
            for stmt in statements {
                match stmt {
                    Statement::Let {
                        pat,
                        type_ref,
                        initializer: Some(init),
                    } => {
                        if let Expr::Call { callee, .. } = body[*init] {
                            if let Some(TyKind::FnDef(id, sub)) = &infer
                                .type_of_expr
                                .get(callee)
                                .map(|t| t.interned(&Interner))
                            {
                                let def = hir_ty::traits::chalk::from_chalk(self, *id);
                                let sig = self.callable_item_signature(def).subst(sub);
                                calls.insert(callee, sig.ret().clone());
                            }
                        }
                    }
                    Statement::Expr(ex) => if let Expr::Call { callee, .. } = body[*ex] {},
                    _ => {}
                }
            }

            if let Some(ret) = tail {}
        }

        FnBuilder::new(func, body, map, infer, path, calls)
    }

    fn module_from_file(&self, id: FileId) -> ModuleId {
        for &krate in self.relevant_crates(id).iter() {
            let def_map: Arc<DefMap> = self.crate_def_map(krate);
            for (local_id, data) in def_map.modules() {
                if data.origin.file_id() == Some(id) {
                    return def_map.module_id(local_id);
                }
            }
        }
        unreachable!("Wrong FileId given")
    }
}

fn into_vis(visibility: &RawVisibility, id: Option<ModuleId>) -> Visibility {
    println!("{:?}", visibility);
    match visibility {
        RawVisibility::Module(mod_) => Visibility::Module(id.unwrap()),
        RawVisibility::Public => Visibility::Public,
    }
}

fn match_with(item: ItemInNs, per_ns: PerNs) -> Option<Visibility> {
    match item {
        ItemInNs::Types(def) => per_ns
            .types
            .filter(|(other_def, _)| *other_def == def)
            .map(|(_, vis)| vis),
        ItemInNs::Values(def) => per_ns
            .values
            .filter(|(other_def, _)| *other_def == def)
            .map(|(_, vis)| vis),
        ItemInNs::Macros(def) => per_ns
            .macros
            .filter(|(other_def, _)| *other_def == def)
            .map(|(_, vis)| vis),
    }
}
