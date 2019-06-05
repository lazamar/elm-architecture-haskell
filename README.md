# elm-architecture-haskell

Elm is a pure functional language for the front-end. It enforces an architecture that allows programs to stay pure in an event-based setting.

It turns out that this architecture is a great idea for Haskell too. It provides a performant and easy to understand framework to organise state in long-lived or complex Haskell applications. The Elm Architecture replaces the threading of a state monad or a stack of monad transformers through the program with single pure update function. Its event-based approach also provides a simple way to handle concurrency making things even more fun.

This is an implementation of the Elm Architecture in Haskell.

This implementation runs commands in parallel just like in Elm.

Internally commands run in separate threads. If any of the threads errors, the
error will be re-thrown in the main thread.

## Example Program

This example will does some synchronous and asynchronous IO as well as getting user input without blocking the main thread. It illustrates how the Elm Architecture can help with the organisation of a stateful multi-threaded application.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Lib ( main ) where

import Control.Concurrent (threadDelay)
import qualified ElmArchitecture 
import ElmArchitecture (Config(_init, _update, Config), Cmd)
import Prelude hiding (init)


main :: IO ()
main =
    ElmArchitecture.run Config
        { _init = init
        , _update = update
        }


-- Our application's state
newtype Model = Model String
    deriving (Show)


data Msg
    = DoNothing
    | SetModelValue String


-- State handling


init :: (Model, Cmd Msg)
init =
    (Model "Empty"
        -- Immediate IO action
    ,   [ ignoreResult $ putStrLn "Program started"  
        -- Non-blocking IO, wait for user input
        , SetModelValue <$> getLine 
        -- Async tasks
        , SetModelValue <$> waitFor 2 "Two" -- waits 2 seconds and sets model value to "Two"
        , SetModelValue <$> httpGet "https://hackage.haskell.org"
        , SetModelValue <$> httpGet "https://elm-lang.org"
        ]
    )


-- Here is where our main aplication logic goes.
-- Here we can react to events that happen in our program
-- and schedule IO actions without blocking the main thread.
update :: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DoNothing ->
            (model, [])

        SetModelValue newVal ->
            let
                newModel = Model newVal
                msgToPrint = "New model value: " ++ show newModel
            in
                ( newModel
                , [ ignoreResult $ putStrLn msgToPrint ]
                )


ignoreResult :: IO a -> IO Msg
ignoreResult io = io >> return DoNothing


-- Simulate HTTP requests
httpGet :: String -> IO String
httpGet url =
    case url of
        "https://hackage.haskell.org" ->
            waitFor 3 "Hackage website content"

        "https://elm-lang.org" ->
            waitFor 4 "Elm website content"

        _ ->
            waitFor 2 "404"


-- Return a value after a few seconds
waitFor :: Int -> a -> IO a
waitFor seconds val = do
    threadDelay $ seconds * 1000000
    return val
```
