{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib ( main ) where

--------------------------------------------------------------------------------
import           Control.Concurrent       (threadDelay)
import qualified ElmArchitecture 
import ElmArchitecture (Config(_init, _update, Config), Cmd)
import           Prelude                  hiding (init)
--------------------------------------------------------------------------------

-- ===================
-- MAIN
-- ===================
-- Program output :
--     Program started
--     New model value: Model 0
--     New model value: Model 2
--     New model value: Model 3

main :: IO ()
main =
    ElmArchitecture.run Config
        { _init = init
        , _update = update
        }


-- ===================
-- TYPES
-- ===================


newtype Model = Model Int
    deriving (Show)


data Msg
    = DoNothing
    | SetModelValue Int
    | WaitAndSetModel Int Int


-- ===================
-- UPDATE
-- ===================


init :: (Model, Cmd Msg)
init =
    (Model 0
    ,   [ return $ WaitAndSetModel 2 2 -- waits 2 seconds and sets model value to 2
        , return $ WaitAndSetModel 5 5
        , return $ WaitAndSetModel 3 3
        , doNothingOn $ putStrLn "Program started"
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
                msgToPrint = "New model value: " ++ show model
            in
                (newModel, return $ doNothingOn $ putStrLn msgToPrint)

        WaitAndSetModel secs val ->
            let
                cmd = do
                    threadDelay $ secs * 1000000
                    return $ SetModelValue val
            in
                (model, [cmd])


doNothingOn :: IO a -> IO Msg
doNothingOn io = io >> return DoNothing

