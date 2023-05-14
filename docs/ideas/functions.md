

```rust
fn add(x: i32, y: i32): i32 {
    x + y + 1
}
fn add(x: i32, y: i32) -> i32 {
    x + y + 1
}
fn twice(
    f: fn(i32, i32) i32, 
    x: i32,
) i32 {
    f(f(x))
}
fn twice(
    f: fn(i32, i32): i32, 
    x: i32,
): i32 {
    f(f(x))
}
fn twice(f: fn(i32) -> i32, x: i32) -> i32 {
    f(f(x))
}

twice(|x| x + 1, 42);
```
## Higher
```rust
fn foo_0(f: fn(): i32, x: i32): i32 {

}
fn foo_1(f: fn(i32): i32, x: i32): i32 {

}
fn foo_2(f: fn(i32, i32): i32, x: i32): i32 {

}

```

# Discussion
## Use of `:`, `->` or nothing at all, for denoting the function return type

I'm torn on whether we should just use `:` for denoting the function return type, or stick to `->` or even no punctuator at all.

`->` looks prettier in my eyes, however introduces a new structural token.
`:` on the other hand looks less prettier, but it does not introduce a new token, it reused a token which is already used to signify the types of variables.

For now we stick with `:`

