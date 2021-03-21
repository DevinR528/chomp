use rustc_middle::{
    mir::{visit::Visitor, Body},
    ty::TyCtxt,
};

pub struct IlBuilder;

impl<'tcx> Visitor<'tcx> for IlBuilder {
    fn visit_body(&mut self, body: &Body<'tcx>) {
        panic!()
    }
}
