# Code importation convention
To keep the code clean, we have a few conventions for importing code so that it looks uniform.

1. `mod` imports
2. `use` external crates
3. `use` `std` modules
4. `use` own create, absolute. E.g. `use crate::foo::bar::baz`
5. `use` relative. E.g. `use self::foo::bar::baz`/`use foo::bar::baz`
6. `use` super. E.g. `use super::foo::bar::baz`