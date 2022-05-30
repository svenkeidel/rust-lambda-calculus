type Var = String;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    Lit(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Var(Var),
    Lam(Var, Box<Expr>),
    App(Box<Expr>, Box<Expr>)
}

type Env<'a> = im::HashMap<&'a Var, Val<'a>>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Val<'a> {
    Num(i32),
    Closure(&'a Var, &'a Expr, Env<'a>),
}

pub type Err = String;


static TYPE_ERROR: &str = "Type Error";

#[inline(never)]
pub fn eval<'a>(env: Env<'a>, e: &'a Expr) -> Val<'a> {
    match e {
        Expr::Lit(n) =>
            Val::Num(*n),

        Expr::Add(e1, e2) => {
            let v1 = eval(env.clone(),e1);
            let v2 = eval(env,e2);
            match (v1,v2) {
                (Val::Num(n1), Val::Num(n2)) => Val::Num(n1 + n2),
                (_, _) => panic!("{}", TYPE_ERROR),
            }
        },

        Expr::Mul(e1, e2) => {
            let v1 = eval(env.clone(),e1);
            let v2 = eval(env,e2);
            match (v1,v2) {
                (Val::Num(n1), Val::Num(n2)) => Val::Num(n1 * n2),
                (_, _) => panic!("{}", TYPE_ERROR),
            }
        },

        Expr::If(cond, e1, e2) => {
            let v = eval(env.clone(),cond);
            match v {
                Val::Num(0) => eval(env, e1),
                Val::Num(_) => eval(env, e2),
                _ => panic!("{}", TYPE_ERROR),
            }
        },

        Expr::Var(x) =>
            match env.get(x) {
                Some(v) => v.clone(),
                None => panic!("Variable not in scope"),
            },

        Expr::Lam(x,e) =>
            Val::Closure(x, e, env),

        Expr::App(e1, e2) => {
            let cls = eval(env.clone(),e1);
            match cls {
                Val::Closure(x,e,env_cls) => {
                    let arg = eval(env.clone(),e2);
                    let new_env = env_cls.update(x, arg);
                    eval(new_env, &e)
                },
                _ => panic!("{}", TYPE_ERROR),
            }
        },
    }
}

// Smart constructors for expressions
#[inline(always)]
pub fn lit(n: i32) -> Expr {
    Expr::Lit(n)
}

#[inline(always)]
pub fn add(e1: Expr, e2: Expr) -> Expr {
    Expr::Add(Box::new(e1), Box::new(e2))
}

#[inline(always)]
pub fn mul(e1: Expr, e2: Expr) -> Expr {
    Expr::Mul(Box::new(e1), Box::new(e2))
}

#[inline(always)]
pub fn dec(e: Expr) -> Expr {
    Expr::Add(Box::new(e), Box::new(Expr::Lit(-1)))
}

#[inline(always)]
pub fn if_(cond: Expr, e1: Expr, e2: Expr) -> Expr {
    Expr::If(
        Box::new(cond),
        Box::new(e1),
        Box::new(e2)
    )
}

#[inline(always)]
pub fn var(x: String) -> Expr {
    Expr::Var(x)
}

#[inline(always)]
pub fn lam(x: Var, e: Expr) -> Expr {
    Expr::Lam(x, Box::new(e))
}

#[inline(always)]
pub fn app(e1: Expr, e2: Expr) -> Expr {
    Expr::App(Box::new(e1), Box::new(e2))
}

#[inline(always)]
pub fn let_(x: Var, e1: Expr, e2: Expr) -> Expr {
    app(lam(x, e2), e1)
}

#[inline(always)]
pub fn fix(e: Expr) -> Expr {
    let f = String::from("f");
    let x = String::from("x");
    let v = String::from("v");
    let inner = lam(x.clone(),
                    app(var(f.clone()),
                        lam(v.clone(),
                            app(app(var(x.clone()), var(x.clone())),
                                var(v)
                                ))));
    let fix = lam(f.clone(),
                  app(inner.clone(), inner));
    app(fix, e)
}

fn main() {
    let env = im::HashMap::new();
    let expr = lit(1);
    let v = eval(env, &expr);
    format!("Hello, world! {v:?}");
}

#[test]
fn test_eval_arithmetic_expressions() {
    let env = im::HashMap::new();
    let expr = add(lit(1), lit(2));
    let v = eval(env, &expr);
    assert_eq!(v, Val::Num(3))
}

#[test]
fn test_eval_factorial() {
    let fact = String::from("fact");
    let x = String::from("x");
    let factorial = fix(lam(fact.clone(),
                          lam(x.clone(),
                            if_(var(x.clone()),
                                lit(1),
                                mul(var(x.clone()), app(var(fact.clone()), dec(var(x.clone()))))
                            )
                        )));

    let env = im::HashMap::new();
    let expr = app(factorial, lit(10));
    let v = eval(env, &expr);
    assert_eq!(v, Val::Num(3628800))
}

#[test]
fn test_eval_fibonacci() {
    let fib = String::from("fib");
    let x = String::from("x");
    let fibonacci = fix(lam(fib.clone(),
                            lam(x.clone(),
                                if_(var(x.clone()),
                                    lit(0),
                                    if_(dec(var(x.clone())),
                                        lit(1),
                                        add(app(var(fib.clone()), add(var(x.clone()), lit(-1))),
                                            app(var(fib.clone()), add(var(x.clone()), lit(-2)))
                                        )
                                    )
                                )
                            )
                        ));

    let env = im::HashMap::new();
    let expr = app(fibonacci, lit(30));
    let v = eval(env, &expr);
    assert_eq!(v, Val::Num(832040))
}