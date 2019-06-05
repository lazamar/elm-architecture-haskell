{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib ( main ) where

--------------------------------------------------------------------------------
import           Control.Concurrent       (threadDelay)
import qualified ElmArchitecture 
import ElmArchitecture (Config(_init, _update, Config), Cmd)
import           Prelude                  hiding (init)
import Debug.Trace (trace)
--------------------------------------------------------------------------------

-- ===================
-- MAIN
-- ===================

main :: IO ()
main =
    ElmArchitecture.run Config
        { _init = init
        , _update = update
        }


-- ===================
-- TYPES
-- ===================


-- Our application's state
newtype Model = Model String
    deriving (Show)


data Msg
    = DoNothing
    | SetModelValue String


-- ===================
-- State handling
-- ===================


init :: (Model, Cmd Msg)
init =
    (Model "Empty"
        -- Immediate IO action
    ,   [ ignoreResult $ putStrLn "Program started"  
        -- Non-blocking IO, wait for user input
        , SetModelValue <$> getLine 
        -- Async tasks
        , SetModelValue <$> waitFor 2 "Two" -- waits 2 seconds and sets model value to "Two"
        , SetModelValue <$> waitFor 5 "Five"
        , SetModelValue <$> waitFor 3 "Three"
        ]
    )


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


waitFor :: Int -> a -> IO a
waitFor seconds val = do
    threadDelay $ seconds * 1000000
    return val