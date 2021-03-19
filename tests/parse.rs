use chomp::parse;

#[test]
fn parse_function() {
    let input = r#"fn main() {
    let mut x = 1;
    let y = 10;
    let z = x + y;
}"#;

    let mut builder = parse::AstBuilder::new(input);
    builder.parse().unwrap();
    println!("{:#?}", builder.items());
}

#[test]
fn parse_const() {
    let input = r#"pub const FOO: &'static str = "hello";"#;

    let mut builder = parse::AstBuilder::new(input);
    builder.parse().unwrap();
    println!("{:#?}", builder.items());
}

#[test]
fn parse_const_call() {
    let input = r#"pub const FOO: usize = "hello".len();"#;

    let mut builder = parse::AstBuilder::new(input);
    builder.parse().unwrap();
    println!("{:#?}", builder.items());
}
