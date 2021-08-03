# Haskell Binding for Isocline

You can build and run the [example] program as:
```
$ ghc -ihaskell test/Example.hs src/isocline.c
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

You can install the `isocline` package locally as:
```
$ cabal install
```

[example]: https://github.com/daanx/isocline/blob/main/test/Example.hs
