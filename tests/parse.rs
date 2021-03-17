use chomp::parse;

#[test]
fn parse_stuff() {
    let input = r#"fn main() {
    let mut x = 1;
    let y = 10;
    let z = x + y;
}"#;

    let stream = parse::TokenStream::new(input);
    println!("{:?}", stream);
}
