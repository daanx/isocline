# Haskell Binding for Repline

You can build and run the [example] program as:
```
$ ghc -ihaskell test/Example.hs src/repline.c
$ ./test/Example
```

## Building with Stack

```
$ stack build
$ stack exec example
```

## Building with Cabal

```
$ cabal build
$ cabal run example
```

You can install the `repline` package locally as:
```
$ cabal install
```

[example]: https://github.com/daanx/repline/test/Example.hs