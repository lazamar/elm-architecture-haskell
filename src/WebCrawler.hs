{-# LANGUAGE OverloadedStrings #-}

module WebCrawler (main) where

import           ElmArchitecture            (run, Config(..))
import qualified Data.ByteString            as B
import           Data.Function              ((&))
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            (Map)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, isPrefixOf, isSuffixOf, unpack)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as L
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Network.HTTP.Types.Header  (Header)
import           Network.HTTP.Client.TLS    (newTlsManager)
import           Network.HTTP.Client        (Manager, method, httpLbs, parseRequest
                                            , responseHeaders, responseBody, responseStatus)
import           Network.HTTP.Types.Header  (hContentType)
import           Network.HTTP.Types.Status  (status200)
import qualified Data.Attoparsec.Text.Lazy  as P
import           Control.Applicative        ((<|>))
import           System.Console.ANSI        (clearScreen)
import           Control.Concurrent         (threadDelay)
import qualified Data.Set                   as Set
import           Data.Set                   (Set)

-----------------------------------------------------------------------------
-- | This example application is a web crawler that will access a webpage,
-- go through all of its links and will:
--      - if they are assets or external, check that they work
--      - if they link to pages in the same domain, load the page and
--        restart the process

main :: IO ()
main = do
    manager <- newTlsManager
    run $ Config
        { _init = init' manager
        , _update = update
        }

-- ===================
-- Types
-- ===================

-- | One type to describe the entire state of our application during its lifetime.
data State = State
    { endpoints       :: Map Url Status
    , httpManager   :: Manager
    }

type Url = Text

type Error = Text

data Status
    = Queued
    | Loading
    | Succeeded
    | Failed Error
    deriving (Show, Eq)

-- | All events that our application will respond to
data Msg
    = PageLoaded Url [Url]
    | FailedToLoad Url Error
    | DrawView


-- ===================
-- State Handling
-- ===================

website :: Url
website = "https://lazamar.github.io/"

-- | We can't create an infinite amount of http connections at the same time
-- so we will restrict them to just a few.
maxParallelConnections :: Int
maxParallelConnections = 10

-- | Get the application started
init' :: Manager -> (State, [ IO Msg ])
init' manager =
    ( State
        { endpoints     = Map.insert website Loading mempty
        , httpManager = manager
        }
    , [ loadPage manager website
      , return DrawView
      ]
    )

-- | Handle all possible state transitions
update :: Msg -> State -> ( State, [ IO Msg ] )
update msg state =
    case msg of
        PageLoaded url sublinks ->
            loadMore $ state
                    { endpoints = endpoints'
                    }
            where
                sublinksMap = Map.fromList [( link, Queued) | link <- sublinks ]

                endpoints' = endpoints state
                    & Map.insert url Succeeded
                    & Map.unionWith (flip const) sublinksMap


        FailedToLoad url err ->
            loadMore $ state
                { endpoints = Map.insert url (Failed err) $ endpoints state
                }

        -- | The Elm architecture should take care of handling when the view
        -- should be rendered, but for this very simple application we are
        -- handling it ourselves.
        DrawView ->
            ( state
            , return $ do
                clearScreen
                putStrLn $ view state
                threadDelay $ 500 * millisecond
                return DrawView
            )
            where
                millisecond = 1000

-- | Load as many more links as possible without having more open
-- connection than our maxParallelConnections value.
loadMore :: State -> (State, [ IO Msg  ])
loadMore state =
    ( state
        { endpoints = Map.union toLoad (endpoints state)
        }
    , fmap (fetch (httpManager state) website) $ Map.keys toLoad
    )
    where
        queued  = Map.filter (== Queued)  (endpoints state)
        loading = Map.filter (== Loading) (endpoints state)
        maxLoad = maxParallelConnections - Map.size loading
        toLoad  = fmap (const Loading) $ Map.take maxLoad queued

fetch :: Manager -> Url -> Url -> IO Msg
fetch manager domain link =
    if isLocalPage domain link
        then loadPage  manager link
        else loadLink manager link

loadPage :: Manager -> Url -> IO Msg
loadPage  manager url =
    either (FailedToLoad url) (PageLoaded url) <$> fetchContent manager url

loadLink :: Manager -> Url -> IO Msg
loadLink manager url =
    either (FailedToLoad url) (const $ PageLoaded url []) <$> fetchHead manager url

-- ===================
-- View
-- ===================

view :: State -> String
view state =
    report state

report :: State -> String
report state = unlines
    [ "Failed pages:"
    , unlines $ fmap indent $ Map.keys $ Map.filter (isFailure) $ endpoints state
    , "---"
    , "Total pages:    " <> show (queued + loading + successes + errors)
    , "---"
    , "Loading:        " <> show loading
    , "Queued:         " <> show queued
    , "---"
    , "Active links:   " <> show successes
    , "Broken links:   " <> show errors
    , "---"
    , "Local links:    " <> show (Map.size $ Map.filterWithKey (const . isLocalPage website) $ endpoints state)
    , "External links: " <> show (Map.size $ Map.filterWithKey (const . not . isLocalPage website) $ endpoints state)
    ]
    where
        (queued, loading, successes, errors) =
            Map.foldr
                (\v (q,l,s,e) -> case v of
                    Queued    -> (q + 1,l,s,e)
                    Loading   -> (q,l + 1,s,e)
                    Succeeded -> (q,l,s + 1,e)
                    Failed _  -> (q,l,s,e + 1)
                )
                (0,0,0,0)
                (endpoints state)

        isFailure (Failed _) = True
        isFailure _          = False

        indent t = "\t" <> unpack t

-- ===================
-- Internal
-- ===================

fetchHead :: Manager -> Url -> IO (Either Error [Header])
fetchHead manager url = do
    request <- parseRequest $ unpack url
    let req = request  { method = "HEAD"  }
    Right <$> responseHeaders <$> httpLbs req manager

fetchContent :: Manager -> Url -> IO (Either Error [Url])
fetchContent manager url = do
    eHeaders <- fetchHead manager url
    if not $ isWebpage eHeaders
    then return $ Right []
    else do
        request <- parseRequest $ unpack url
        let req = request
        response <- httpLbs req manager
        if isSuccess response
            then return . Right . allLinks url . decodeUtf8 . responseBody $ response
            else return $ Left "Bad status"
    where
        isWebpage (Left _)        = False
        isWebpage (Right headers) = headers
                    & lookup hContentType
                    & fmap (B.isInfixOf "text/html")
                    & fromMaybe False

        isSuccess = (status200 ==) . responseStatus




isLocalPage :: Url -> Url -> Bool
isLocalPage domain url = domain `isPrefixOf` url

-- Outside of the slides

isQuote :: Char -> Bool
isQuote c = c == '"'

urlParser :: Url -> P.Parser Url
urlParser url = do
    beginning <- P.string "http://"
                <|> P.string "https://"
                <|> absolutePath
                <|> relativePath

    end       <- P.takeWhile isUrlCharacter
    return $ joinWithSlash beginning end
    where
        isUrlCharacter c =
            case c of
                '"' -> False
                ' ' -> False
                _   -> True

        relativePath = P.string "./" >> return url

        absolutePath = P.string "/" >> return website

        joinWithSlash a b =
                if "/" `isSuffixOf` a
                then
                    if "/" `isPrefixOf` b
                        then a <> Text.take 1 b
                        else a <> b
                else
                    if "/" `isPrefixOf` b
                        then a <> b
                        else a <> "/" <> b

quoted :: P.Parser a -> P.Parser a
quoted parser = do
    P.skip isQuote
    res <- parser
    P.skip isQuote
    return res

tryAfterAll :: (Char -> Bool) -> P.Parser a -> P.Parser [ a ]
tryAfterAll predicate parser = do
    P.skipWhile (not . predicate)
    succeedAndContinueTrying <|> skipAndContinueTrying <|> finish
    where
        skipAndContinueTrying = do
            P.skip predicate
            tryAfterAll predicate parser

        finish = do
            P.endOfInput
            return []

        succeedAndContinueTrying = do
            res <- parser
            rest <- tryAfterAll predicate parser
            return $ res : rest

allLinks :: Url -> L.Text -> [Url]
allLinks url txt =
    let res = P.parse (tryAfterAll isQuote $ quoted $ urlParser url) txt
    in
    case res of
        P.Fail _ _ _ -> []
        P.Done _ urls -> urls

