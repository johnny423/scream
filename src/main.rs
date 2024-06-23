use ast::Env;

mod parser;
mod ast;

fn main() {
    let input = "(let ((x 1)(add (lambda (a b) (+ a b)))) (add -1 2))";
    let ast = parser::parse_program(input);
    
    let env = Env::new();
    let v = ast.evaluate(&env);
    println!("eval {v:?}");
    //
    // let a = Exp::Let {
    //     vars: vec!["x".into()],
    //     vals: vec![Exp::App(
    //         "list".into(),
    //         vec![Exp::Bool("t".into()), Exp::Num("-6".into())],
    //     )],
    //     body: Box::new(Exp::App("cdr".into(), vec![Exp::Var("x".into())])),
    // }
    //     .evaluate(&env);
    //
    // println!("{:?}", a)
}
