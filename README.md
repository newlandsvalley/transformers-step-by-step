Monad Transformers Step by Step
===============================

(not complete)

This is an implementation of the 2006 tutorial paper: [Monad Transformers Step by Step](https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf) by Martin Grabmüller.  In it, he starts from an evaluation function for simple expressions, introduces 'do-nothing' monadic behaviour and then extends capability by incrementally building a monad transformer stack.

As this is a PureScript implementation, there are multiple small differences between it and the Haskell original.  The most important are probably:

  * Partiality.  The early eveluation functions from the paper are partial and make use of the fact that the program fails when an input is not covered.  PureScript requires partial functions to be annotated with the ```Partial``` type class. I prefer to use total functions in most cases. In any case, PureScript is better disciplined and I don't think its monads define ```fail```.
  * Nomenclature. Some names differ - for example Haskell's ```ErrorT``` becomes PureScripts ```ExceptT```, ```return``` becomes ```pure``` etc..
  * Identity.  Haskell defines this in ```Control.Monad.Identity``` with an evaluation function of ```runIdentity```.  Purescript simply defines it as a newtype (together with a plethora of typeclass instances) in ```Data.Identity```

To build
--------

    spago build

To test
-------

    npm run test