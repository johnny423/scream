use env::Env;

mod parser;
mod expr;
mod env;
mod apply;
mod val;

fn main() {
    let input = "((lambda (a b) (+ a b)) -1 2))";
    let ast = parser::parse_program(input);

    let env = Env::new();
    let v = ast.evaluate(&env);
    println!("eval {v:?}");
}
