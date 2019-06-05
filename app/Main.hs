{-# LANGUAGE NoImplicitPrelude #-}

module Main where

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
data Model = Model
    { counter :: Int
    , content :: String
    }
    deriving (Show)


data Msg
    = DoNothing
    | SetContent String
    | DecreaseCounter


-- State handling


init :: (Model, Cmd Msg)
init =
    (Model { counter = 10, content = "No content" } 
        -- Immediate IO action
    ,   [ ignoreResult $ putStrLn "Program started"  
        -- Non-blocking IO, wait for user input
        , SetContent <$> getLine 
        -- Async tasks
        , SetContent <$> waitFor 2 "Two" -- waits 2 seconds and sets model value to "Two"
        , SetContent <$> httpGet "https://hackage.haskell.org"
        , SetContent <$> httpGet "https://elm-lang.org"
        , return DecreaseCounter
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

        DecreaseCounter ->
            if counter model == 0 then 
                (model, [])
            else 
                (model { counter = counter model - 1 }
                ,   [ waitFor 1 DecreaseCounter 
                    , ignoreResult $ putStrLn $ show model 
                    ]
                )

        SetContent newContent ->
            ( model { content = newContent }
            , []
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