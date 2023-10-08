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

fn id<T>(a: T): T {
    a
}
fn const<T, U>(a: T, b: U): T {
    a
}
type Functor<T<A>> = trait {
    type Self = T<_>;
    fn map<B>(self: Self, f: fn(A): B): Self<B>;

    // (<$) :: a -> f b -> f a
    fn replace_with<B>(self: Self, a: A): Self<B> {
        self.map(|_| a)
    }

    // --

    // void :: Functor f => f a -> f ()
    fn void(self: Self) -> Self<()> {
        self.replace_with(())
    }
}
type Applicative<T<A>> = trait
where T<A>: Functor<T<A>> {
    type Self = T<_>;

    fn pure(a: A) -> Self;

    // (<*>) :: f (a -> b) -> f a -> f b
    // f <*> x = liftA2 id f x
    fn apply<B>(self: Self, func: Self<fn(A): B>): Self<B> {
        self.lift_two(func, |a, f| f(a))
    }

    // liftA2 func right self = (<*>) (fmap func right) self
    fn lift_two<B, C>(self: Self, right: Self<B>, func: fn(A, B): C): Self<C> {
        self.apply(right.map(func))
    }
    // *>
    fn discard_left<B>(self: Self, right: Self<B>): Self<B> {
        // self.lift_two(right, |_, b| b)
        self.replace_with(right)
    }
    // <*
    fn discard_right<B>(self: Self, right: Self<B>): Self {
        self.lift_two(right, |a, _| a)
    }

    // --

    // join :: Monad m => m (m a) -> m a 
    fn join(self: Self<Self>): Self {
        self.bind(|x| x)
    }

    // liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
    fn lift_m_1<B>(self: Self, f: fn(A): B): Self<B> {
        self.bind(|a| Self::return(f(a)))
    }

    // liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
    fn lift_m_2<B, C>(self: Self, right: Self<B>, f: fn(A, B): C): Self<C> {
        self.bind(|a| 
            right.bind(|b| 
                Self::return(f(a, b))
            )
        )
    }

    // ap :: Monad m => m (a -> b) -> m a -> m b
    fn ap<A, B>(self: Self<fn(A): B>, right: Self<A>): Self<B> {
        self.bind(|f| 
            right.bind(|a| 
                Self::return(f(a))
            )
        )
    }
}

type Monad<T<A>> = trait
where T<A>: Applicative<T<_>> {
    type Self = T<_>;

    // (>>=) :: m a -> (a -> m b) -> m b
    fn bind<B>(self: Self, f: fn(A): Self<B>): Self<B>;
    
    // (>>) :: m a -> m b -> m b
    fn then<B>(self: Self, right: Self<B>): Self<B>  {
        self.bind(|_| right)
    }

    // return :: a -> m a
    fn return(a: A): T<A> {
        Self::pure(a)
    }
}
```

So now let's implement those basics on the type Option.

```rust
type Option<T> = enum {
    None,
    Some(T),
}
impl Functor<Option<_>> for Option<_> {
    // Some(1).map(|x| x + 1) == Some(2)
    // None.map(|x| x + 1) == None
    fn map<A, B>(self: Self<A>, func: fn(A): B): Self<B> {
        match self {
            Some(a) => Some(func(a)),
            None => None,
        }
    }

    // Some(1).replace_with(2) == Some(2)
    // None.replace_with(2) == None
    fn replace_with<A, B>(self: Self<A>, other: A) -> Self<B> {
        self.fmap(|_| other)
    }

    // Some(1) <$> 2 == Some(2)
    // None <$> 2 == None
    fn (<$)<A, B>(self: Self<A>, other: A) -> Self<B> {
        self.replace_with(other)
    }

}

impl Applicative<Option<_>> for Option<_> {
    // Option::pure(1) == Option::Some(1)
    fn pure<A>(a: A) -> Option<A> {
        Some(a)
    }

    // Some(1).apply(Some(|x| x + 1)) == Some(2)
    // None.apply(Some(|x| x + 1)) == None
    // Some(1).apply(None) == None
    // None.apply(None) == None
    fn apply<A, B>(self: Self<A>, func: Self<fn(A): B>) -> Self<B> {
        match (self, func) {
            (Some(s), Some(f)) => s.map(f),
            (_, _) => None,
        }
    }

    // Some(1).lift_two(Some(2), |x, y| x + y) == Some(3)
    // None.lift_two(Some(2), |x, y| x + y) == None
    // Some(1).lift_two(None, |x, y| x + y) == None
    // None.lift_two(None, |x, y| x + y) == None
    fn lift_two<A, B, C>(self: Self<A>, other: Self<B>, func: fn(A, B): C) -> Self<C> {
        match (self, other) {
            (Some(a), Some(b)) => Some(func(a, b)),
            (_, _) => None,
        }
    }

    // Some(1).discard_left(Some(2)) == Some(2)
    // None.discard_left(Some(2)) == None
    // Some(1).discard_left(None) == None
    // None.discard_left(None) == None
    fn discard_left<A, B>(self: Self<A>, right: Self<B>) -> Self<B> {
        match (self, right) {
            (Some(_), Some(b)) => Some(b),
            (_, _) => None,
        }
    }

    // Some(1).discard_right(Some(2)) == Some(1)
    // None.discard_right(Some(2)) == None
    // Some(1).discard_right(None) == None
    // None.discard_right(None) == None
    fn discard_right<A, B>(self: Self<A>, right: Self<B>) -> Self<A> {
        match (self, right) {
            (Some(a), Some(_)) => Some(a),
            (_, _) => None,
        }
    }
}
// fn map<A, B>(self: Self<A>, func: fn(A): B) -> Self<B>;
// fn app<A, B>(self: Self<A>, func: Self<fn(A): B>) -> Self<B>;
// fn bin<A, B>(self: Self<A>, func: fn(A): Self<B>): Self<B>; 

impl Monad<Option<_>> for Option {
    // Some(1).bind(|x| Some(x + 1)) == Some(2)
    // None.bind(|x| Some(x + 1)) == None
    // Some(1).bind(|x| None) == None
    // None.bind(|x| None) == None
    // (>>=)       :: m a -> (a -> m b) -> m b
    fn bind<A, B>(self: Self<A>, func: fn(A): Self<B>): Self<B> {
        match self {
            Some(a) => self.map(func),
            None => None,
        }
    }

    // Some(1).then(Some(2)) == Some(2)
    // None.then(Some(2)) == None
    // Some(1).then(None) == None
    // None.then(None) == None
    fn then<A, B>(self: Self<A>, func: fn(A): Self<B>) -> Self<B> {
        self.discard_left(func)
    }

    // Some(1) >> Some(2) == Some(2)
    // None >> Some(2) == None
    // Some(1) >> None == None
    // None >> None == None
    fn return<A>(a: A) -> Self<A> {
        Some(a)
    }
    // Option::return(1) == Some(1)
}
```

Now let's do the same but for Semigroups and Monoids.

```haskell

class Semigroup a where
    {-# MINIMAL (<>) | sconcat #-}

    (<>) :: a -> a -> a
    a <> b = sconcat (a :| [ b ])

    sconcat :: NonEmpty a -> a
    sconcat (a :| as) = go a as where
      go b (c:cs) = b <> go c cs
      go b []     = b

    stimes :: Integral b => b -> a -> a
    stimes = stimesDefault
        
```

```rust

type List = enum {
    Nil,
    Cons(T, List),
}

type NonEmpty<T> = enum {
    NonEmpty(T, List<T>),
}
type Semigroup<A> = trait {
    fn combine(self: Self<A>, right: Self<A>) -> Self<A> {

    }

    fn concatenate(self: Self) -> Self
        where Self: NonEmpty<A> {
        
    }

    fn repeat_times(self: Self, n: usize) -> Self {

    }
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