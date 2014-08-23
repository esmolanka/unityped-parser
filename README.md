unityped-parser
===============

It's experimental parser from unityped data representation like JSON
to Haskell data structures. But main goal is not performance but
ability to track parsing errors, where they occur and what was
expected to get.

It outputs verbose error messages like the following:
```
*Test> parseIO pFooX2orBuqzFixx testVal1
Failure at @ Dict
           { at @ Dict .Foo Table{Class=XYTable}
                { at @ Dict .Foo Table{Class=XYTable}
                     required :Z or
                  at @ Dict .Foo Table{Class=XYTable} :Y [1]
                     expected IntLit, got DblLit } or
             at @ Dict .Buqz Dict
                { required .Fixx or
                  { required .Fixxx and
                    required .QQQ and
                    required .r and
                    required .p } or
                  required .Fixxxx or
                  required .Bazz } }
```

Please see `Test.hs` for details.

There are simple rules:
  1. `Monad` instance defines *then* behaviour. So `a >>= b` produces
     either error message of parser `a` or message of parser `b`, depending
     on which one is failing, but not both.
  2. `Applicative` instance defines *simultaneously* behaviour. So
     something like `(,,) <$> a <*> b <*> c` will produce up to 3 error
     messages for each failing parser, combined with `and` operation.
  3. `Alternative` instance defines *choose* behaviour. So `a <|> b <|> c`
     will produce 3 error messages, combined with `or` operation.
