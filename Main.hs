{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-- An example of embedding a custom monad into
-- Scotty's transformer stack, using ReaderT to provide access
-- to a TVar containing global state.
--
-- Note: this example is somewhat simple, as our top level
-- is IO itself. The types of 'scottyT' and 'scottyAppT' are
-- general enough to allow a Scotty application to be
-- embedded into any MonadIO monad.
module Main (main) where

import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Default.Class
import Data.String
import Data.Text.Lazy.Encoding ( decodeUtf8 )
import Data.Text.Lazy (Text)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Network.Wai.Middleware.RequestLogger

import Prelude ()
import Prelude.Compat

import Web.Scotty.Trans

import User (User(..), Admin (..), Ordinary (..), readUser, newUser, updateUser)

newtype AppState = AppState { wordCounts :: Map Text Int }

instance Default AppState where
    def = AppState Map.empty

newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
    sync <- newTVarIO def
    let runActionToIO m = runReaderT (runWebM m) sync

    scottyT 9000 runActionToIO app

app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev
    get "/query" $ do
        key <- param "key" >>= \ x -> return (x `asTypeOf` (undefined :: Text))
        counts <- webM $ gets wordCounts
        text $ fromString $ show (Map.findWithDefault (0 :: Int) key counts)

    post "/input" $ do
        bd <- body >>= \x -> return (decodeUtf8 x)
        webM $ modify $ \ st -> 
            let wd = wordCounts st
            in st { wordCounts = Map.insertWith (\_ prev -> prev + 1) bd 1 wd }
        redirect "/"

    post "/example" $ do
      _ <- liftIO $ newUser Admin "green" (Just "justin@example.com")
      _ <- liftIO $ newUser Ordinary () (Just "justin@example.com")
      -- Below won't compile because "Ordinary" can't specify a favorite color
      -- _ <- liftIO $ newUser Ordinary "green" (Just "justin@example.com")

      let getColor :: Maybe (User id email color) -> color
          getColor = maybe (error "No such user") (\(User _ _ color) -> color)
      _ :: String <- liftIO (readUser Admin 1 >>= \x -> (return $ getColor x))
      _ :: () <- liftIO (readUser Ordinary 1 >>= \x -> (return $ getColor x))
      -- Below fails to compile because "Ordinary" role can't read favorite color
      -- _ :: String <- liftIO (readUser Ordinary 1 >>= \x -> (return $ getColor x))

      _ :: String <- liftIO (updateUser Admin (User 1 Nothing (Just "blue")) >>= \x -> (return $ getColor x))
      _ :: () <- liftIO (updateUser Ordinary (User 1 Nothing (Just ())) >>= \x -> (return $ getColor x))
      -- Below fails to compile because "Ordinary" role can't update favorite color
      -- _ :: String <- liftIO (updateUser Ordinary (User 1 Nothing (Just "xxx")) >>= \x -> (return $ getColor x))
      return ()
