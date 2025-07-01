use crate::compile::compile;

#[test]
fn test_prefix_op() {
    compile(
        r#"
fn main() {
    let a = -1;
    let b = !a;
}
"#,
    );
}
