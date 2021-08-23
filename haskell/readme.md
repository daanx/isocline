# Haskell Binding for Isocline

You can build and run the [example] program as:
```
$ ghc -ihaskell test/Example.hs src/isocline.c
$ ./test/Example
```

See the [API documentation](https://hackage.haskell.org/package/isocline/docs/System-Console-Isocline.html) on hackage.

## Using with Stack

You can build with isocline by adding 
```
extra-deps:
  - isocline-<version>    
```
to your `stack.yaml` file, and 
```
dependencies:
  - isocline
```
to your `package.yaml` file (and also run `stack update`).

## Using with Cabal

To use it with Cabal, add
```
build-depends:
  isocline
```
to your `project.cabal` file (and perhaps run `cabal update`).


# Building Isocline from Source

## Building with Stack

From the isocline directory, run:

```
$ stack build
$ stack exec example
```

## Building with Cabal

From the isocline directory, run:

```
$ cabal build
$ cabal run example
```

You can install the locally build `isocline` package as:
```
$ cabal install
```

[example]: https://github.com/daanx/isocline/blob/main/test/Example.hs
