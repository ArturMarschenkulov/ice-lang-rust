In this folder, general ideas for icelang are discussed and talked about.

# Languages for inspiration
- [Rust](https://www.rust-lang.org/)
- [Zig](https://ziglang.org/)
- [Go](https://golang.org/)
- [Swift](https://swift.org/)
- [Odin](https://odin.handmade.network/)
- [C](https://en.wikipedia.org/wiki/C_(programming_language))
- [C++](https://en.wikipedia.org/wiki/C%2B%2B)


# Grand plan
The language development will be done in 2 main overarching phases.

## Phase 1
In this stage, the goal is to develop a `better C`, in the sense of that it should be simple as C, but with a clearer syntax and more modern features.

The goal of this stage is to create a basis.

## Phase 2
In this stage, more advanced features will be added.

Generally speaking, this is the part where the language will take on many functional features.

# There should be one way to do something
A general idea of Ice is that there should be usually only one way to do something. This is not a hard rule, but a general principle.

For example, hexedecimal literals allow only uppercase letters as numbers, that is something like `0xFFFF` is valid, while `OxfFff` is not.