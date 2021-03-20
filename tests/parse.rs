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
fn parse_const_str() {
    let input = r#"pub const FOO: &'static str = "hello";"#;

    let mut builder = parse::AstBuilder::new(input);
    builder.parse().unwrap();
    println!("{:#?}", builder.items());
}

#[test]
fn parse_const_uint() {
    let input = r#"pub const FOO: u32 = 42;"#;

    let mut builder = parse::AstBuilder::new(input);
    builder.parse().unwrap();
    println!("{:#?}", builder.items());
}

#[test]
fn parse_const_sint() {
    let input = r#"pub const FOO: i16 = -1;"#;

    let mut builder = parse::AstBuilder::new(input);
    builder.parse().unwrap();
    println!("{:#?}", builder.items());
}

#[test]
fn parse_const_float() {
    let input = r#"pub const FOO: f64 = 0.42;"#;

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
