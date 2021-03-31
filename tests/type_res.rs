use chomp::resolve::TypeResolver;

#[test]
fn resolve_fn_call() {
    let input = r#"
fn add_one(x: u8, y: u8) -> u8 {
    x + 1
}
fn main() {
    let x = 1_u8;
    let z = add_one(x, 1 + 3);
}"#;

    let (res, id) = TypeResolver::parse_file(input);
    res.walk_crate(id)
}

#[test]
fn resolve_func() {
    let input = r#"fn main() {
    let x = 1_u8;
    let z = x + 10;
    let k = x + z;
}"#;

    let (res, id) = TypeResolver::parse_crate(vec![input]);
    res.walk_crate(id[0])
}

#[test]
fn resolve_const_str() {
    let input = r#"pub const FOO: &'static str = "hello";"#;

    let (res, id) = TypeResolver::parse_crate(vec![input]);
    res.walk_crate(id[0])
}

#[test]
fn resolve_mod() {
    let input = r#"
mod add_one {
    mod foo { fn hello() {} }
}
mod not {}
fn main() {
        let x = 1_u8;
        let z = add_one(x);
}"#;

    let (res, id) = TypeResolver::parse_crate(vec![input]);
    res.walk_crate(id[0])
}

#[test]
fn resolve_struct() {
    let input = r#"pub struct Thing { a: u8 }"#;

    let (res, id) = TypeResolver::parse_crate(vec![input]);
    res.walk_crate(id[0])
}
