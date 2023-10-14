# Ad-Hoc sum type
```
var l: (i32 | f32) = 32;
``` 
# Ad-Hoc product type
```
var l: (i32, f32) = (32, 20.0);
```


# Namd sum type
```
type Point(i32 | f32);
```
# Named product type

```
type Point(i32, i32);
```


```
// type copy. Note it is not a type alias, but a completely new type.
type MyI32 : i32;

// This is an alias type
use i32 as MyI32;

// unit type
type Unit;
type Context;


// The above is syntactic sugar for this below
type Unit : ();
type Context : ();


// function type
type FunctionType : fn(i32, i32): i32;


// named product type, usually called class or struct by other languages
type Person : struct {
    name: string,
    age: i32,
}

// named sum type, usually called enum or tagged union by other languages

type TrafficLight : enum {
    red,
    yellow,
    green,
}

type TrafficLight : union {
    red: (),
    yellow: (),
    green: (),
}

type ApiResponse : union {
    success: i32
    error: (string, i32)
}

type Shape : union {
    circle: f64,
    square: f64,
    rectangle: (f64, f64),
}

type Shape : union {
    circle: struct { radius: f64 },
    rectangle: struct { width: f64, height: f64},
}
```

```swift
type Option<T> : union {
    some: T,
    none,
}

type Result<T, E> : union {
    ok: T,
    error: E,
}

fn main(): () {
    var x := Option<i32>::some{32};
    var x := Option::some{32};
    var x := some{32};
}
```