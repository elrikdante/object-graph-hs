{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S
import qualified Data.Aeson as Data.Aeson
import qualified Data.Text  as T
import qualified Data.Text.Lazy as T (fromStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map   as Data.Map
import Network.HTTP.Types (status404)
import GHC.Exts   (fromList)
import Data.Either (either)
import Data.Maybe  (fromMaybe)
import Data.Aeson ((.:))
import Turtle
import Prelude hiding (FilePath)
fromEither = either undefined id

data RubyClass = RubyClass T.Text [(T.Text, Int)] deriving Show
               
data Success   =  Success RubyClass [(T.Text, T.Text, T.Text)] deriving Show
data Failure   = Failure deriving Show

type Analysis  = Either (Failure) Success

analyse :: FilePath -> IO Analysis
analyse path = do
  let folder = fromEither (toText path)
  (code, out) <- shellStrict ("flog " `T.append` folder) empty
  let methodList = fmap (T.dropWhile (==' ') ) (drop 4 (T.lines out))
  case code of
    ExitSuccess -> return $ Right $ Success (RubyClass "balh" []) (parseMethodDetail (fmap tokenise methodList))
    _           -> return $ Left Failure
  where
    parseMethodDetail ((complexity:method:file:[]):rest)
      | "none" `T.isSuffixOf` method   = parseMethodDetail rest
      | otherwise                      = (complexity,method,file):parseMethodDetail rest
    parseMethodDetail _                = []
    tokenise = fmap (filter (not . T.null)) (T.splitOn " ") 

demoClasses :: Data.Map.Map T.Text RubyClass
demoClasses = Data.Map.fromList [
  ("ApiWebService",  RubyClass "ApiWebService" []),
  ("User"         ,  RubyClass "User"          [])
  ]

instance Data.Aeson.FromJSON RubyClass where
  parseJSON = Data.Aeson.withObject "RubyClass" $ \o ->
    RubyClass 
    <$> o .: "name"
    <*> fmap (fmap (\(k,v) -> (T.cons '*' k, v))) (o .: "methods")

instance Data.Aeson.ToJSON RubyClass where
  toJSON (RubyClass name methods) = object [
    "name"    .= name,
    "methods" .= methods
    ]

findOr404 :: T.Text -> (RubyClass -> S.ActionM ()) -> S.ActionM ()
findOr404 name f = do
  maybe 
    (S.status status404)
    f
    (Data.Map.lookup name demoClasses)

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.text "hello"

  S.get "/classes" $ do
    S.json demoClasses
  S.get "/class/:class_name" $ do
    class_name <- S.param "class_name"
    findOr404 class_name $ \(rbklass) -> S.json rbklass
  S.get "/methods/:class_name" $ do
    class_name <- S.param "class_name"
    findOr404 class_name $ \(RubyClass _ methods) -> S.json methods
  S.get "/names/:class_name" $ do
    class_name <- S.param "class_name"
    findOr404 class_name $ \(RubyClass name _) -> S.text (T.fromStrict name)

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8080 app'

