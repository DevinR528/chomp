// use chomp::middle::TypeResolver;

// #[test]
// fn resolve_num_type() {
//     let input = r#"
// fn add_one(x: u8) -> u8 {
//     x + 1
// }
// fn main() {
//     let x = 1_u8;
//     let z = add_one(x);
// }"#;

//     let (res, id) = TypeResolver::parse_file(input);
//     res.walk_crate(id)
// }

// #[test]
// fn resolve_func() {
//     let input = r#"fn main() {
//     let x = 1_u8;
//     let z = x + 10;
// }"#;

//     let (res, id) = TypeResolver::parse_crate(vec![input]);
//     res.walk_crate(id[0])
// }
