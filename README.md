# Nova
An R7RS implementation on the top of gambit scheme, and largely influenced by [chibi-scheme](http://code.google.com/p/chibi-scheme) and riaxpander.

Currently a test suite(tests/r5rs-tests.scm) from chibi-scheme is fully passed with small modification.

# Finished
1. Hygienic Macro System (define-syntax, let-syntax, letrec-syntax)

    Implemented with syntactic-closure. syntax-rules is supported with an modified chibi's version.

# TODO
1. R7RS module system support.

2. Error message improvement

    Currently only internal core forms have proper location information for erros. Should add these for macro expanding?

3. Proper local variable naming

    All local variable will compiles into a form of "name.location-number", this could be improved.

4. let-syntax, letrec-syntax lacks support for moving internal definition to proper location.

```scheme
    (let ()
      (+ 1 2)
      (let-syntax ()
        (define x 100))
      x)
```

expanded to

```scheme
    ((lambda ()
     (+ 1 2)
     (define x 100)
     x))
```

which will report ill placed define error.


# Internal document

1. Environment

    Mapping from `Name` to `Denotation`

2. Denotation

    A variable or macro transformer.

3. Name

    `Alias` or symbol

4. Alias

   A syntactic closure which has a `Name` form.
