Ice intends to have higher kinded types.

Since it's still early stage, and I did not even work out the "trait/typeclass" system of the language, I'll simply steal the syntax of Rust.

As an inspiration as to how it could look like, it would be interesting to look at Haskell, Rust and Scala.

Probably we will come up with syntax which is basically like a "rustified" Scala. For now, I plan on using Haskell's naming convention.

Here are some articles which could be relevant to work this out.
Rust:

- [hugopeters article](https://hugopeters.me/posts/14/)
- [edmundsmith's gist](https://gist.github.com/edmundsmith/855fcf0cb35dd467c29a9350481f0ecf)
- [babysteps article part 1](http://smallcultfollowing.com/babysteps/blog/2016/11/02/associated-type-constructors-part-1-basic-concepts-and-introduction/)
- [babysteps article part 2](http://smallcultfollowing.com/babysteps/blog/2016/11/03/associated-type-constructors-part-2-family-traits/)
- [babysteps article part 3](http://smallcultfollowing.com/babysteps/blog/2016/11/04/associated-type-constructors-part-3-what-higher-kinded-types-might-look-like/)
- [rustyyato](https://rustyyato.github.io/type/system,type/families/2021/02/15/Type-Families-1.html)
- [FPComplete: Monads and GAT](https://www.fpcomplete.com/blog/monads-gats-nightly-rust/)
- [CMCDragonkai](https://gist.github.com/CMCDragonkai/a5638f50c87d49f815b8)
- [HKT with macros](https://gist.github.com/14427/af90a21b917d2892eace)

Scala:

- [typelever/cats](https://typelevel.org/cats/index.html)
- [Functors](https://typelevel.org/cats/typeclasses/functor.html)
```rust

type Functor<F<_>> = trait {
    fn fmap<A, B>(fa: F<A>, f: A -> B) -> F<B>;
    fn infix (<$)<A, B>(a, )
}

impl Functor<Option<_>> for Option<_> {
    fn fmap<A, B>(fa: Option<A>, f: fn(A) -> B) -> Option<B> {
        match fa {
            None => None,
            Some(a) => Some(f(a)),
        }
    }
    fn lift<A, B>(fa: Option<B>, a: A) -> Option<A> {
        a
    }
}

// Functor[List].compose[Option].map(listOption)(_ + 1)
// Functor<List<_>>::compose<Option<_>>::map(list_option, _ + 1);
// Functor::<List<_>>::compose::<Option<_>>::map(list_option, _ + 1);
// Functor{List{_}}::compose{Option{_}}::map(list_option, _ + 1);

type Monad<F<_>> = trait
    where
        
{

}
```