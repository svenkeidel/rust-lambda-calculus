# An interpreter for the lambda calculus in Rust

## Build and Run

You can build and run the tests with the [Cargo](https://doc.rust-lang.org/cargo/index.html) build tool.
```
$ cargo build
$ cargo test 
```

## Ownership

The interpreter is a function from an expression and a variable environment to a value or an error:
```
eval<'a>(env: Env<'a>, e: &'a Expr) -> Result<Val<'a>, Err>
```

The interpreter takes ownership of the environment `env`, because it needs to extend it during function application.
If the interpreter would not take ownership of the environment `env` but borrow it, then there is no owner of the newly created environment `new_env`.
This also means that we must clone the environment for sucessive calls of the interpreter, e.g.:
```
let v1 = eval(env.clone(),e1)?;
let v2 = eval(env,e2)?;
```
Fortunately, cloning is O(1) since the environment is immutable.

The interpreter borrows expressions, since no new expressions are created during interpretation.
This avoids costly O(n) cloning of expressions.

Closure values take ownership of environments.
If closure values would not take ownership of environments, then the type checker cannot guarantee safe deallocation, since closures and environments could recursively refer to each other.