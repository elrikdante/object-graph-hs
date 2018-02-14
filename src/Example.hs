{-# LANGUAGE RankNTypes,
             OverloadedStrings,
             DeriveGeneric #-}
module Example (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)

import qualified Web.Scotty as Web.Scotty
import qualified Data.Aeson as Data.Aeson
import qualified Data.Text  as T
import qualified Data.Text.Lazy as T (fromStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map   as Data.Map

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status404)
import GHC.Exts   (fromList)
import GHC.Generics
import Data.Either (either)
import Data.Maybe  -- (fromMaybe)
import Data.Aeson -- ((.:))
import Data.Foldable (forM_)
import Data.Function -- ((&))
import Turtle
import Prelude hiding (FilePath,fail)
import Data.List (sortOn)
import qualified Data.ByteString.Lazy as LBS

data RubyClass    = RubyClass T.Text [(T.Text, Double)] deriving Show
data Failure msg  = Failure msg deriving Show
type Analysis     = Either (Failure T.Text) RubyClass

fromEither = either undefined id
emptyMap :: Data.Map.Map T.Text [(T.Text,Int)]
emptyMap   = Data.Map.empty

fail msg   = Failure msg
sink       = flip shellStrict empty
flogCmd    = sink . ("flog " <>)

type LineOffset = Int

data MethodSummary =
     MethodSummary {msName :: String,
                   msCost ::  Float}
    |MethodSummaryPath { msName :: String
                  ,msCost :: Float
                  ,msPath :: String
                  ,msloMin :: LineOffset
                  ,msloMax :: LineOffset}
     deriving (Show,Eq,Ord,Generic)

instance ToJSON MethodSummary

data Complexity    = Complexity String Int [MethodSummary]

summarise :: FilePath -> IO [MethodSummary]
summarise dir = do
  parts <- shellStrict ("flog " <> (folder dir)) empty & fmap snd & fmap T.lines
  pure (fmap (match summary) parts) & fmap (fmap extract) & fmap catMaybes
  where extract [] = Nothing
        extract (h:_) = Just h
        folder= fromEither . toText
        summary = do
                skip (crlf <|> spaces)
                score <- (many digit >>= \ord  ->
                          char '.'   >>
                          many digit >>= \mant ->
                          return (ord ++ "." ++ mant)
                          <* skip spaces)
                      <|> (many digit <* skip spaces)
                char ':'
                skip (crlf <|> spaces)
                description <- many (alphaNum <|> oneOf "#/.:-?!_")
                fileInfo    <- optional $ do
                                         path <- skip (spaces) *> many (alphaNum <|> oneOf "/._")
                                         (char ':')
                                         (min,max) <- ((,) <$> (many digit)
                                                           <*> (char '-' *> many digit))
                                         return (path,(min,max))
                _ <- many anyChar
                return $ maybe
                         (MethodSummary description (read score))
                         (\(path,(min,max)) ->
                                case null path of
                                True -> MethodSummary description (read score)
                                _    -> MethodSummaryPath description
                                                          (read score)
                                                          path
                                                          (read min)
                                                          (read max))
                         fileInfo


demoClasses :: Data.Map.Map T.Text RubyClass
demoClasses = Data.Map.fromList [
  ("ApiWebService",  RubyClass "ApiWebService" []),
  ("User"         ,  RubyClass "User"          [])
  ]

instance Data.Aeson.FromJSON RubyClass where
  parseJSON = Data.Aeson.withObject "RubyClass" $ \o ->
    RubyClass
    <$> o .: "name"
    <*> (<$>) ((<$>) (\(k,v) -> (T.cons '*' k, read $ T.unpack v))) (o .: "methods")

instance Data.Aeson.ToJSON RubyClass where
  toJSON (RubyClass name methods) = object [
    "name"    .= name,
    "methods" .= methods
    ]

findOr404 :: T.Text -> (RubyClass -> Web.Scotty.ActionM ()) -> Web.Scotty.ActionM ()
findOr404 name f = do
  maybe
    (Web.Scotty.status status404)
    f
    (Data.Map.lookup name demoClasses)

app' :: Web.Scotty.ScottyM ()
app' = do
  Web.Scotty.get "/" $ do
    Web.Scotty.text "hello"
  Web.Scotty.get "/classes" $ do
    Web.Scotty.json demoClasses
  Web.Scotty.get "/class/:class_name" $ do
    class_name <- Web.Scotty.param "class_name"
    findOr404 class_name $ \(rbklass) -> Web.Scotty.json rbklass
  Web.Scotty.get "/methods/:class_name" $ do
    class_name <- Web.Scotty.param "class_name"
    findOr404 class_name $ \(RubyClass _ methods) -> Web.Scotty.json methods
  Web.Scotty.get "/names/:class_name" $ do
    class_name <- Web.Scotty.param "class_name"
    findOr404 class_name $ \(RubyClass name _) -> Web.Scotty.json (T.fromStrict name)

app :: IO Application
app = Web.Scotty.scottyApp app'

runApp :: IO ()
runApp = summarise "examples/bunny/lib" >>= mapM_ LBS.putStrLn  . fmap encode . sortOn (msName)-- >>  Web.Scotty.scotty 8080 app'
