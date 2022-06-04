use std::rc::Rc;
use std::cell::RefCell;

type Var = &'static str;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    NumLit(i32),
    UnaryOp(UnaryOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Var(Var),
    Lam(Var, Box<Expr>, im::HashMap<Var,()>),
    Rec(Var, Var, Box<Expr>, im::HashMap<Var,()>),
    App(Box<Expr>, Box<Expr>),
    Let(Var, Box<Expr>, Box<Expr>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Inc,
    Dec,
    Abs,
    Neg,
    Not,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
}

type Env<'a> = im::HashMap<Var, Val<'a>>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Val<'a> {
    Num(i32),
    Bool(bool),
    Closure(Var, &'a Expr, Rc<RefCell<Env<'a>>>),
}

pub type Err = String;


static TYPE_ERROR: &str = "Type Error";

fn free_vars(e: &Expr) -> im::HashMap<Var, ()> {
    match e {
        Expr::Var(x)           => im::hashmap!{ *x => () },
        Expr::NumLit(_)        => im::hashmap!{},
        Expr::UnaryOp(_, e)    => free_vars(e),
        Expr::BinOp(_, e1, e2) => free_vars(e1) + free_vars(e2),
        Expr::If(e1, e2, e3)   => free_vars(e1) + free_vars(e2) + free_vars(e3),
        Expr::App(e1, e2)      => free_vars(e1) + free_vars(e2),
        Expr::Lam(x, _, fv)    => fv.without(x),
        Expr::Rec(f, x, _, fv) => fv.without(f).without(x),
        Expr::Let(x, e1, e2)   => free_vars(e1) + free_vars(e2).without(x),
    }
}

#[inline(always)]
fn unop<'a>(op: &UnaryOp, v: Val<'a>) -> Val<'a> {
    match (op,v) {
        (UnaryOp::Inc, Val::Num(n))  => Val::Num(n + 1),
        (UnaryOp::Dec, Val::Num(n))  => Val::Num(n - 1),
        (UnaryOp::Abs, Val::Num(n))  => Val::Num(n.abs()),
        (UnaryOp::Neg, Val::Num(n))  => Val::Num(-n),
        (UnaryOp::Not, Val::Bool(b)) => Val::Bool(!b),
        (_, _) => panic!("{}", TYPE_ERROR),
    }
}

#[inline(always)]
fn binop<'a>(op: &BinOp, v1: Val<'a>, v2: Val<'a>) -> Val<'a> {
    match (op,v1,v2) {
        (BinOp::Add, Val::Num(n1),  Val::Num(n2))  => Val::Num(n1 + n2),
        (BinOp::Sub, Val::Num(n1),  Val::Num(n2))  => Val::Num(n1 - n2),
        (BinOp::Mul, Val::Num(n1),  Val::Num(n2))  => Val::Num(n1 * n2),
        (BinOp::Div, Val::Num(n1),  Val::Num(n2))  => Val::Num(n1 / n2),
        (BinOp::And, Val::Bool(b1), Val::Bool(b2)) => Val::Bool(b1 && b2),
        (BinOp::Eq,  Val::Num(n1),  Val::Num(n2))  => Val::Bool(n1 == n2),
        (_, _, _) => panic!("{}", TYPE_ERROR),
    }
}

#[inline(never)]
pub fn eval<'a>(env: Env<'a>, e: &'a Expr) -> Val<'a> {
    match e {
        Expr::NumLit(n) =>
            Val::Num(*n),

        Expr::UnaryOp(op, e1) => {
            let v = eval(env.clone(),e1);
            unop(op, v)
        },

        Expr::BinOp(op, e1, e2) => {
            let v1 = eval(env.clone(),e1);
            let v2 = eval(env,e2);
            binop(op, v1, v2)
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

        Expr::Lam(x,e,free_vars) => {
            // remove unused variables from closure
            let closed_env = env.intersection_with(free_vars.clone(), |v, _| v);
            Val::Closure(x, e, Rc::new(RefCell::new(closed_env)))
        },

        Expr::Rec(f, x, e, free_vars) => {
            let closed_env = Rc::new(RefCell::new(env.intersection_with(free_vars.clone(), |v, _| v)));
            let v = Val::Closure(x, e, closed_env.clone());
            closed_env.borrow_mut().insert(f, v.clone());
            v
        },

        Expr::App(e1, e2) => {
            let cls = eval(env.clone(),e1);
            match cls {
                Val::Closure(x,e,env_cls) => {
                    let arg = eval(env.clone(),e2);
                    let new_env = env_cls.borrow().update(x, arg);
                    eval(new_env, &e)
                },
                _ => panic!("{}", TYPE_ERROR),
            }
        },

        Expr::Let(_, _, _) => todo!(),
    }
}

// Smart constructors for expressions
#[inline(always)]
pub fn lit(n: i32) -> Expr {
    Expr::NumLit(n)
}

#[inline(always)]
pub fn add(e1: Expr, e2: Expr) -> Expr {
    Expr::BinOp(BinOp::Add, Box::new(e1), Box::new(e2))
}

#[inline(always)]
pub fn mul(e1: Expr, e2: Expr) -> Expr {
    Expr::BinOp(BinOp::Mul, Box::new(e1), Box::new(e2))
}

#[inline(always)]
pub fn dec(e: Expr) -> Expr {
    Expr::UnaryOp(UnaryOp::Dec, Box::new(e))
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
pub fn var(x: Var) -> Expr {
    Expr::Var(x)
}

#[inline(always)]
pub fn lam(x: Var, e: Expr) -> Expr {
    let fv = free_vars(&e);
    Expr::Lam(x, Box::new(e),fv)
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
pub fn rec(f: Var, x: Var, e: Expr) -> Expr {
    let fv = free_vars(&e);
    Expr::Rec(f, x, Box::new(e), fv)
}

fn main() {
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
    let fact = "fact";
    let x = "x";
    let factorial = rec(fact.clone(), x.clone(),
                            if_(var(x.clone()),
                                lit(1),
                                mul(var(x.clone()), app(var(fact.clone()), dec(var(x.clone()))))
                            )
                        );

    let env = im::HashMap::new();
    let expr = app(factorial, lit(10));
    let v = eval(env, &expr);
    assert_eq!(v, Val::Num(3628800))
}

#[test]
fn test_eval_fibonacci() {
    let fib = "fib";
    let x = "x";
    let fibonacci = rec(fib.clone(), x.clone(),
                            if_(var(x.clone()),
                                lit(0),
                                if_(dec(var(x.clone())),
                                    lit(1),
                                    add(app(var(fib.clone()), add(var(x.clone()), lit(-1))),
                                        app(var(fib.clone()), add(var(x.clone()), lit(-2)))
                                    )
                                )
                            )
                        );

    let env = im::HashMap::new();
    let expr = app(fibonacci, lit(30));
    let v = eval(env, &expr);
    assert_eq!(v, Val::Num(832040))
}