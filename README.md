# elm-architecture-haskell

Elm is a pure functional language for the front-end. It enforces an architecture that allows programs to stay pure in an event-based setting.

It turns out that this architecture is a great idea for Haskell too. It provides a performant and easy to understand framework to organise state in long-lived or complex Haskell applications.

The Elm Architecture replaces the threading of a state monad or a stack of monad transformers through the program with single pure update function. Its event-based approach also provides a simple way to handle concurrency making things even more fun.

This is an implementation of the Elm Architecture in Haskell.

This implementation runs commands in parallel just like in Elm.

Internally commands run in separate threads. If any of the threads errors, the
error will be re-thrown in the main thread.

## Example Program

Checkout the example at [`src/WebCrawler.hs`](https://github.com/lazamar/elm-architecture-haskell/blob/master/src/WebCrawler.hs).
You can compile and run this with `stack build` and `stack run`.

The example illustrates how the Elm Architecture can help with the organisation of state updates in a multi-threaded application.

