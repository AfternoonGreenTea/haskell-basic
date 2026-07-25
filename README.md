# haskell-basic

A learning repository for studying the fundamentals of Haskell.
Each `.hs` file contains sample code organized by topic.

## File Structure

| File | Contents |
|---|---|
| `Functions.hs` | Function definitions, guards, pattern matching, where / let-in, currying |
| `Lists.hs` | List operations, recursion, custom map / filter / fold implementations, list comprehensions |
| `Adt.hs` | Algebraic data types, record syntax, Maybe / Either, newtype, type-safe design |
| `Typeclasses.hs` | Typeclass definitions and instances, Functor / Applicative / Monad |

## Environment

- GHC 9.10.3
- Cabal 3.16

## Usage

### Run interactively with GHCi

```bash
ghci Functions.hs
```

Switch files within GHCi:

```
:l Lists.hs
```

Reload after editing a file:

```
:r
```

### Build and run with Cabal

```bash
cabal build
cabal run
```
