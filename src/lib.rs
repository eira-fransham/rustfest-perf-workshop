#[macro_use]
extern crate combine;

use std::collections::HashMap;

#[derive(Clone)]
pub enum Ast {
    Lit(Value),
    Variable(String),
    Call(Box<Ast>, Vec<Ast>),
    Define(String, Box<Ast>),
}

#[derive(Clone)]
pub enum Value {
    Void,
    False,
    Int(u64),
    Function(Vec<String>, Vec<Ast>),
    InbuiltFunc(fn(Vec<Value>) -> Value),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (&Void, &Void) => true,
            (&False, &False) => true,
            (&Int(a), &Int(b)) => a == b,
            _ => false,
        }
    }
}

pub fn eval(program: Ast, variables: &mut HashMap<String, Value>) -> Value {
    use self::Ast::*;
    use self::Value::*;

    match program {
        Lit(val) => val,
        Variable(name) => match variables.get(&name) {
            Some(v) => v.clone(),
            _ => panic!("Variable does not exist: {}", &name),
        },
        Call(func, arguments) => {
            let func = eval(*func, variables);

            match func {
                Function(args, body) => {
                    // Start a new scope, so all variables defined in the body of the
                    // function don't leak into the surrounding scope.
                    let mut new_scope = variables.clone();

                    if arguments.len() != args.len() {
                        println!("Called function with incorrect number of arguments (expected {}, got {})", args.len(), arguments.len());
                    }

                    for (name, val) in args.into_iter().zip(arguments) {
                        let val = eval(val, variables);
                        new_scope.insert(name, val);
                    }

                    let mut out = Void;

                    for stmt in body {
                        out = eval(stmt, &mut new_scope);
                    }

                    out
                }
                InbuiltFunc(func) => func(
                    arguments
                        .into_iter()
                        .map(|ast| eval(ast, variables))
                        .collect(),
                ),
                _ => panic!("Attempted to call a non-function"),
            }
        }
        Define(name, value) => {
            let value = eval(*value, variables);

            variables.insert(name, value);

            Void
        }
    }
}

parser! {
    pub fn expr[I]()(I) -> Ast where [I: combine::Stream<Item = char>] {
        use combine::parser::char::*;
        use combine::*;

        macro_rules! white {
            ($prs:expr) => {
                between(
                    skip_many(satisfy(char::is_whitespace)),
                    skip_many(satisfy(char::is_whitespace)),
                    $prs,
                )
            };
        }

        let lambda = char('\\');
        let eq = char('=');
        let flse = white!(string("#f")).map(|_| Ast::Lit(::Value::False));
        let ident = || white!(many1::<String, _>(letter()));
        let function = (
            white!(lambda),
            white!(between(char('('), char(')'), many::<Vec<_>, _>(ident()))),
            many::<Vec<_>, _>(expr()),
        ).map(|(_, a, b)| Ast::Lit(::Value::Function(a, b)));
        let define = (white!(eq), ident(), expr()).map(|(_, a, b)| Ast::Define(a, Box::new(b)));
        let lit_num = many1::<String, _>(digit())
            .map(|i| Ast::Lit(::Value::Int(i.parse().expect("Parsing integer failed"))));
        let call = (expr(), many(expr())).map(|(func, args)| Ast::Call(Box::new(func), args));

        white!(choice!(
            flse,
            lit_num,
            ident().map(Ast::Variable),
            between(char('('), char(')'), choice!(function, define, call))
        ))
    }
}
