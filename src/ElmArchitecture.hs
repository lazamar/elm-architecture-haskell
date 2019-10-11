{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElmArchitecture (run , Config(..)) where

import           Control.Concurrent.Async (Async, async, waitAny)
import           System.Exit              (exitSuccess)
import           System.Console.ANSI      (clearScreen)
import           Data.Function            ((&))

-- | A record to describe the main parts of our application
data Config model msg = Config
    { _init   :: (model, [ IO msg ])
    , _update :: msg -> model -> (model, [ IO msg  ])
    }

-- | Run the program
run :: forall model msg. Config model msg -> IO ()
run config = do
    initAsyncs <- traverse async initCmds
    run' initAsyncs initModel
    where
        (initModel, initCmds) = _init config

        update' = _update config

        run' :: [ Async msg ] -> model -> IO ()
        run' asyncs model =
            if null asyncs
            then do
                print "Finished"
                exitSuccess
            else do
                -- | This works like a pool of async commands with a queue
                -- in the end. The first command to be resolved is the first
                -- command dealt with.
                (completedCmd, msg) <- waitAny asyncs :: IO (Async msg, msg)

                let (newModel, cmds) = update' msg model

                asyncCmds  <- traverse async cmds

                -- Remove the cmd that we just ran
                let newAsyncs = asyncs
                        & filter (/= completedCmd)
                        & (++ asyncCmds)

                run' newAsyncs newModel
