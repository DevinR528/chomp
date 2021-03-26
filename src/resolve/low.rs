use hir_def::{
    body::{Body, BodySourceMap},
    data::FunctionData,
    expr::PatId,
};
use hir_expand::name::Name;
use hir_ty::{InferenceResult, Ty};
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct Opp {}

#[derive(Debug)]
pub struct OppBuilder {}

#[derive(Debug)]
pub struct BranchBuilder {}

#[derive(Debug)]
pub struct StmtBuilder {
    branch: BranchBuilder,
    opp: OppBuilder,
    rep: Vec<Opp>,
}

#[derive(Debug)]
pub struct FnBuilder {
    name: Vec<Name>,
    params: FxHashMap<PatId, Ty>,
    stmts: Vec<Opp>,
}

impl FnBuilder {
    pub fn new(
        func: &FunctionData,
        body: &Body,
        map: &BodySourceMap,
        infer: &InferenceResult,
        path: &[hir::Name],
    ) -> Self {
        let mut name = path.to_vec();
        name.push(func.name.clone());

        println!(
            "{}",
            name.iter().map(|n| format!("::{}", n)).collect::<String>()
        );

        let mut params = FxHashMap::default();
        for p in &body.params {
            let ty = infer.type_of_pat.get(*p).unwrap();
            params.insert(*p, ty.clone());
        }

        let mut stmts = vec![];
        println!("{:?}", body[body.body_expr]);
        body[body.body_expr].walk_child_exprs(|id| {
            println!("{:?}", body[id]);
        });

        Self {
            name,
            params,
            stmts,
        }
    }
}
