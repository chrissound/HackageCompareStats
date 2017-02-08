{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (fromString)
import Prelude
import qualified Arch
import Data.Text (Text)
--import qualified Data.Text
import Data.String.Conversions
import Data.Maybe (isJust)
import qualified Heist
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as HeistCom
import Heist.Internal.Types
import qualified Text.XmlHtml as X
import Data.List (sortBy)
import Data.Map.Syntax
import Data.ByteString.Builder (toLazyByteString)

import Text.Mustache

-- imported
sortPs :: [APS] -> [APS]
sortPs ps = sortBy (\(_,x) (_,y) -> compare x y) ps

optionHtml :: Text -> X.Node
optionHtml v = X.Element "option" [("value", v)] []

generateRecordSplice :: Text -> I.Splice IO
generateRecordSplice = I.runNode . optionHtml

generateRecordSplices :: [Text] -> I.Splice IO
generateRecordSplices = I.mapSplices generateRecordSplice

statisticRemark :: [APS] -> [String]
statisticRemark ( (xhead,xp) : xs@(xshead,xsp) : xss ) =
  [convertString xhead ++ " is " ++ show (xp / xsp) ++ "x as popular as " ++ convertString xshead ++ ". "]
  ++ statisticRemark (xs:xss)
statisticRemark _ = []

compareFormBinds :: Show a => Arch.PackagesStats -> [a] -> [APS] -> HeistState IO -> HeistState IO
compareFormBinds x' rp' results = I.bindSplices $
                        do "packages" ##
                             (generateRecordSplices . fmap fst $ Arch.getPackages x')
                           "statisticResult" ##
                             I.runChildrenWith
                               (do "requestedPackage" ##
                                     (I.textSplice . convertString $ show rp')
                                   "statisticRemark" ##
                                     (I.textSplice .
                                      convertString .
                                      concat . statisticRemark . reverse . sortPs $
                                      results))
--

type APS = Arch.PackageStat
type APSm = Arch.PackagesStats

multiParam :: Text -> ActionM [Text]
multiParam x = do
  v <- params
  return $ convertString . snd <$> (filter (\z -> (fst z == convertString x)) $ v)

data ScriptInject = ScriptInject String

instance ToMustache ScriptInject where
    toMustache (ScriptInject value) = object
      [ "scriptInject" ~> value
      ]

renderTemplate :: String -> (HeistState IO -> HeistState IO) -> ActionM ()
renderTemplate fileName hsBinding = do
  let emptyI = return () :: MapSyntax Text (I.Splice IO)
  let emptyC = return () :: MapSyntax Text (HeistCom.Splice IO)
  let emptyA = return () :: MapSyntax Text (AttrSplice IO)
  let templateLocations = [Heist.loadTemplates "templates/"]
  let spliceConfig = SpliceConfig emptyI emptyI emptyC emptyA templateLocations (\_ -> True):: SpliceConfig IO
  heist <- lift $ Heist.initHeist (HeistConfig spliceConfig "" True)
  case heist of
    Right heist' -> do
      rendered <- lift $ I.renderTemplate (hsBinding heist') $ convertString fileName
      case (rendered) of
        Just (builder, _) -> do
          let grr = toLazyByteString builder
          let templateYo = compileTemplate "hmm" (convertString grr) 
          case templateYo of
            (Right tempalteYolo) -> html . convertString $ substituteValue tempalteYolo (toMustache $ ScriptInject "<h1>woah</h1>")
            (Left _) -> error "Failed to compile template"
        Nothing -> error "heist error"
    Left a -> error . convertString $ show a

comparePackage :: ActionM ()
comparePackage = do
  requestedPackages <- comparePackageRequestedPackages
  statisticsStore <- liftIO $ Arch.getPackagesStats "packageStatistics.json"
  case (statisticsStore) of
    Just statisticsStore' -> do
      aps <- comparePackageGetPackages requestedPackages statisticsStore'
      --_ <- renderTemplate "compareForm"  $ compareFormBinds statisticsStore' requestedPackages aps
      --renderTemplate "compareForm"  id
      renderTemplate "compareForm"  $ compareFormBinds statisticsStore' requestedPackages aps
    Nothing -> raise "Couldn't open database store"


comparePackageRequestedPackages :: ActionM [Text]
comparePackageRequestedPackages = do
  requestedPackages <- multiParam "package[]"
  when ( not $ length requestedPackages >= 2) $
    raise "You need to specify atleast two requestedPackages"
  return requestedPackages

comparePackageGetPackages :: [Text] -> APSm -> ActionM [APS]
comparePackageGetPackages requestedPackages statisticsStore = do
  let searchPackages = Arch.searchPackageStats statisticsStore
  let packagesResult = map (searchPackages . convertString) requestedPackages :: [Maybe APS]
  case (sequence packagesResult) of
    Just (results) -> return results
    Nothing -> raise $ convertString $ "Unable to find the following requestedPackages: "  ++ show packagesNotFound where
      packagesNotFound = join $ zipWith
        (\requestedPkg packageResult -> if isJust packageResult then [] else [requestedPkg])
        requestedPackages packagesResult


main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/comparePackage" comparePackage
    get "/foo" $ do
        v <- param "fooparam"
        html $ mconcat ["<h1>", v, "</h1>"]

    -- An uncaught error becomes a 500 page.
    get "/raise" $ raise "some error here"

    -- You can set status and headers directly.
    get "/redirect-custom" $ do
        status status302
        setHeader "Location" "http://www.google.com"
        -- note first arg to header is NOT case-sensitive

    -- redirects preempt execution
    get "/redirect" $ do
        void $ redirect "http://www.google.com"
        raise "this error is never reached"

    -- Of course you can catch your own errors.
    get "/rescue" $ do
        (do void $ raise "a rescued error"; redirect "http://www.we-never-go-here.com")
        `rescue` (\m -> text $ "we recovered from " `mappend` m)

    -- Parts of the URL that start with a colon match
    -- any string, and capture that value as a parameter.
    -- URL captures take precedence over query string parameters.
    get "/foo/:bar/required" $ do
        v <- param "bar"
        html $ mconcat ["<h1>", v, "</h1>"]

    -- Files are streamed directly to the client.
    get "/404" $ file "404.html"

    -- You can stop execution of this action and keep pattern matching routes.
    get "/random" $ do
        void next
        redirect "http://www.we-never-go-here.com"

    -- You can do IO with liftIO, and you can return JSON content.
    get "/random" $ do
        g <- liftIO newStdGen
        json $ take 20 $ randomRs (1::Int,100) g

    get "/ints/:is" $ do
        is <- param "is"
        json $ [(1::Int)..10] ++ is

    get "/setbody" $ do
        html $ mconcat ["<form method=POST action=\"readbody\">"
                       ,"<input type=text name=something>"
                       ,"<input type=submit>"
                       ,"</form>"
                       ]

    post "/readbody" $ do
        b <- body
        text $ decodeUtf8 b

    get "/header" $ do
        agent <- header "User-Agent"
        maybe (raise "User-Agent header not found!") text agent

    -- Make a request to this URI, then type a line in the terminal, which
    -- will be the response. Using ctrl-c will cause getLine to fail.
    -- This demonstrates that IO exceptions are lifted into ActionM exceptions.
    get "/iofail" $ do
        msg <- liftIO $ liftM fromString getLine
        text msg

{- If you don't want to use Warp as your webserver,
   you can use any WAI handler.

import Network.Wai.Handler.FastCGI (run)

main = do
    myApp <- scottyApp $ do
        get "/" $ text "hello world"

    run myApp
-}
