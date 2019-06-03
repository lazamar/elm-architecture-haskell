{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElmArchitecture 
	( run
	, Config(..)
	, Cmd
	) where

--------------------------------------------------------------------------------
import           Control.Concurrent.Async (Async, async, waitAny)
import           Prelude                  hiding (init)
--------------------------------------------------------------------------------

-- ===================
-- INTERNALS
-- ===================


type Cmd a = [ IO a ]


data Config model msg =
    Config
    { _init   :: (model, Cmd msg)
    , _update :: msg -> model -> (model, Cmd msg)
    }


{-| Runs a program
-}
run :: forall model msg. Config model msg -> IO ()
run config =
	let
        (initModel, initCmds) = _init config

        update' = _update config

        run' :: [Async msg] -> model -> IO ()
        run' asyncs model =
            if null asyncs then
                print "Finished"
            else
                do
                    -- This works like a pool of async commands with a queue
                    -- in the end. The first command to be resolved is the first
                    -- command dealt with.
                    (completedCmd, msg) <- waitAny asyncs :: IO (Async msg, msg)

                    let (newModel, newCmds) = update' msg model

                    newCmdsAsync <- traverse async newCmds

                    let newAsyncs =
                            filter (/= completedCmd)    -- Remove the cmd that we just ran
                            asyncs ++ newCmdsAsync   -- Let's add what our update returned

                    run' newAsyncs newModel
	in
    do
    	initAsyncs <- traverse async initCmds
    	run' initAsyncs initModel
