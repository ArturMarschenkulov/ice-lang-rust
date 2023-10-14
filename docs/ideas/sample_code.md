



# 

## Fibonacci

```
fn fib(n: i32): i32 {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
```

## Factorial

```
int factorial(int n) {
    if (n == 0) return 1;
    return n * factorial(n - 1);
}

fn factorial(n: i32): i32 {
    if n == 0 { return 1;}
    n * factorial(n - 1)
}
fn factorial(n: i32): i32 {
    switch n {
        0: 1,
        _: n * factorial(n - 1),
    }
}
```