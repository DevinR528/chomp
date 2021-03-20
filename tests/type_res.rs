use chomp::{parse, resolve::TypeResolver};

#[test]
fn resolve_num_type() {
    let input = r#"fn main() {
    let x = 1_u8;
    let z = x + 10;
}"#;

    let mut builder = parse::AstBuilder::new(input);
    builder.parse().unwrap();
    println!("{:#?}", builder.items());

    let resolver = TypeResolver::resolve(builder.items());
    println!("{:#?}", resolver);
}
