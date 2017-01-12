## Things I would like to learn
* !
* ~
* forall
* Lazy IO, conduits, ...
* ~~WHNF~~, seq
* catch, throw
* text, bytestring
* react-flux, ghcjs

## Snippets

`a n | n > 0 = n | True = -n`   &emsp;  Guards can go on one line

`f 0 = 1 ; f n = n * f (n-1)`    &emsp;  This works in GHCi

`k :: Int ; k = 4`   &emsp;  So does this

``Just undefined `seq` 42``   &emsp; Demonstrates that `Just expr` is in WHNF

``a `seq` b `seq` f a b``   &emsp;  Forces a and b

`withFile "b.txt" ReadMode hGetContents`   &emsp;  hGetContents: illegal operation (delayed read on closed handle)

`withFile "b.txt" ReadMode $ \h -> hGetContents h >>= putStr`

`runEffect $ for (each [3,4]) $ lift . print`

`runEffect $ for (yield 4) $ lift . print`

`runEffect $ each ~> lift . print $ [1..3]`

`runEffect $ yield "hi" >-> P.stdoutLn`

`runEffect $ yield "hi" >-> (await >>= lift . putStrLn)`

## Definitions

f $! x = x \`seq\` f x

seq :: a -> b -> b   -- evaluates a to WHNF when seq a b is evaluated to WHNF

``hGetContents h  = IO.hGetContents h >>= \s -> length s `seq` return s``  -- strict version


## Examples of things in WHNF

### The outermost part is a data constructor

Just (2+2)

(1+1, 2+2)

'h' : ("e" ++ "llo")

### Lambda
\\x -> 2 + 2

