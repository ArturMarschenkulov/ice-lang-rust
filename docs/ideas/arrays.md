

# How it's done in other languages

## C

```c
<data_type> <name>'['<array_size>']'
int data_0[100];
int data_1[100] = {0};

```

## Rust
```rust
let data_0: [i32; 100];
let data_1: [i32; 100] = [0; 100];
let data_2: [i32; 3] = [0, 1, 2];
```

## Swift
```swift
var data = [Int](repeating: 0, count 100)
var data = Array(repeating: 0, count: 100)
```

## Go
```go
var data [100]int
```

## Zig
```zig
var data_0: [100]i32 = undefined;
var data_1: [100]i32 = [100]i32{0} ** 100;
var data_1: [_]i32 = [_]i32{0} ** 100;
var data_1 = [_]i32{0} ** 100;
```

## Odin
```odin
data := [100]int{}
```