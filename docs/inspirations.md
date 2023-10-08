Here are listed the inspirations and other interesting stuff which might be relevant.
# Goals

Ice should be basically like a love child of Swift/C++/Rust and Haskell. Rust itself is already the love child between C++ and Haskell, however I find it is too restrictive.

My goal with Ice is that it should have all the modern features, but be generally speaking as unsafe as C++.

One of the design goals it to make the language a little hard coded as possible. That means a few things.
- Trying to keep the amount of keywords down, and use as many context sensitive keywords as possible.
- Structural tokens/punctuators should ideally be only simple singular tokens. For example, `:` for signifying the return type instead of `->`, `:` in switch expressions instead of `=>`. However we still use `::` for paths :(
- Having only one keyword for loops `loop` with overloaded functionalities, instead of having `for`, `while`, `loop`, etc.
- use of abbreeviations for keywords. `fn` instead of `func/fun/function`, `ret` instead of `return`, `brk` instead of `break`, `cnt` instead of `continue`. This may not have much to do with how hardcoded it is, but it makes the code shorter plus frees up more actual words.
- There should be no hardcoded non-structural tokens. They might have special meaning to the compiler, but otherwise they are always custom.

# Generally interesting languages

Here are interesting languages and interesting links to them.

## C++, 
## Rust
- [github](https://github.com/rust-lang/rust)
- [book](https://doc.rust-lang.org/book/title-page.html)
- [reference](https://doc.rust-lang.org/reference/introduction.html)
- [rust-by-example](https://doc.rust-lang.org/rust-by-example/index.html)
- [nomicon](https://doc.rust-lang.org/nomicon/intro.html)
- [patterns](https://github.com/rust-unofficial/patterns)
- [dev-guide](https://rustc-dev-guide.rust-lang.org/about-this-guide.html)
- [embedded](https://docs.rust-embedded.org/book/intro/index.html)
- [perf-book](https://nnethercote.github.io/perf-book/introduction.html)
## Swift
- [github](https://github.com/apple/swift),
## Carbon 
- [github](https://github.com/carbon-language/carbon-lang),
## Zig
- [github](https://github.com/ziglang/zig), 
## Odin
- [github](https://github.com/odin-lang/Odin),
- [docs](https://odin-lang.org/docs/)
- [overview](https://odin-lang.org/docs/overview/)
## Jai
- Jai
## Nim
## Scala
- [github](https://github.com/scala/scala)
- [tour-of_scala](https://docs.scala-lang.org/tour/tour-of-scala.html)

## Agda
## Haskell


# Compilers/Languages written in Rust
Since this language is writtein in Rust, projects of languages written in Rust are a great inspiration.

- [langs-in-rust](https://github.com/alilleybrinker/langs-in-rust)
- jakt
- [swc(typescript)](https://github.com/alilleybrinker/langs-in-rust)
- [rustcc(C)](https://github.com/ClementTsang/rustcc)
- [gleam](https://github.com/gleam-lang/gleam)
- [inko](https://github.com/YorickPeterse/inko)
- [crunch-lang](https://github.com/Kixiron/crunch-lang)


# Projects which might be interesting to look into, but are not compilers
- [ariadne](https://github.com/zesterer/ariadne)

