Ice intends to have higher kinded types.

Since it's still early stage, and I did not even work out the "trait/typeclass" system of the language, I'll simply steal the syntax of Rust.

As an inspiration as to how it could look like, it would be interesting to look at Haskell, Rust and Scala.

Probably we will come up with syntax which is basically like a "rustified" Scala. For now, I plan on using Haskell's naming convention.

Here are some articles which could be relevant to work this out.

Haskell:

```haskell
class Functor f where
    fmap        :: (a -> b) -> f a -> f b

    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const


class Functor f => Applicative f where
    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)

    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2

    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const


class Applicative m => Monad m where
 
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k

    return      :: a -> m a
    return      = pure
```

```rust

fn id<A>(a: A) -> A {
    a
}
fn const<A, B>(a: A, b: B) -> A {
    a
}
type Functor<T<_>> = trait {
    fn fmap<A, B>(fa: Self<A>, f: A -> B) -> T<B>;
    fn (<$)<A, B>(fb: Self<B>, a: A) -> T<A> {
        fb.fmap(|_| a)
    }
}
type Applicative<T<_>: Functor<F<_>>>: = trait {
    fn pure<A>(a: A) -> T<A>;
    fn (<*>)<A, B>(fa: T<A>, f: T<A -> B>) -> T<B>;
    fn liftA2<A, B, C>(f: A -> B -> C, fa: T<A>, fb: T<B>) -> T<C>;
    fn (*>)<A, B>(fa: T<A>, fb: T<B>) -> T<B>;
    fn (<*)<A, B>(fa: T<A>, fb: T<B>) -> T<A>;
}

type Monad<T<_>: Applicative<F<_>>> = trait {
    fn (>>=)<A, B>(fa: T<A>, f: A -> T<B>) -> T<B>;
    fn (>>) <A, B>(fa: T<A>, fb: T<B>) -> T<B>;
    fn return<A>(a: A) -> T<A>;
}
```
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
    fn (<$)<A, B>(a, )
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
impl Functor<Option<_>> for Option<_> {
    fn fmap<A, B>(fa: Option<A>, f: fn(A) -> B) -> Option<B> {
        match fa {
            None: None,
            Some(a): Some(f(a)),
        }
    }
}

fn add(x: i32, y: i32) -> i32 {
    
}
fn add x::i32 -> y::i32 -> i32 {
    
}

typeclass monad<arrow<type, type> t> m { 
    m<a> return(a x); 
    m<b> bind(m<a> x, fn<a, m<b>> f); 
}
typeclass monad<arrow<type, type> t> m { 
    fn return(a x) -> m<a>; 
    fn bind(m<a> x, fn<a, m<b>> f) -> m<b>; 
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

```rust

fn tuple<A, B>(x: List<A>, y: List: <B>): List<(A, B)> {
    x.flat_map(|a| y.map((a, _)))
}
fn tuple<A, B>(x: Option<A>, y: Option: <B>): Option<(A, B)> {
    x.flat_map(|a| y.map((a, _)))
}
fn tuple<S, A, B>(x: State<S, A>, y: State<S, B>): State<S, (A, B)> {
    x.flat_map(|a| y.map((a, _)))
}

fn tuple<F<_>, A, B>(x: F<A>, y: F<B>): F<(A, B)> {

}

```