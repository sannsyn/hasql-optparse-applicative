module Hasql.Options.Applicative where

import BasePrelude
import Options.Applicative
import qualified Hasql.Connection
import qualified Hasql.Pool


-- |
-- Given a prefix for long names produces a parser of @Hasql.Pool.'Hasql.Pool.Settings'@.
poolSettings :: Maybe String -> Parser Hasql.Pool.Settings
poolSettings prefix =
  (,,) <$> size <*> timeout <*> connectionSettings prefix
  where
    size =
      option auto $
        long (prefixed "pool-size") <>
        value 1 <>
        showDefault <>
        help "Amount of connections in the pool"
    timeout =
      fmap fromIntegral $
      option auto $
        long (prefixed "pool-timeout") <>
        value 10 <>
        showDefault <>
        help "Amount of seconds for which the unused connections are kept open"
    prefixed s = 
      maybe s (<> ("-" <> s)) prefix

-- |
-- Given a prefix for long names produces a parser of @Hasql.Connection.'Hasql.Connection.Settings'@.
connectionSettings :: Maybe String -> Parser Hasql.Connection.Settings
connectionSettings prefix =
  Hasql.Connection.settings <$> host <*> port <*> user <*> password <*> database
  where
    host =
      fmap fromString $ strOption $
        long (prefixed "host") <> 
        value "127.0.0.1" <>
        showDefault <>
        help "Server host"
    port =
      option auto $
        long (prefixed "port") <>
        value 5432 <>
        showDefault <>
        help "Server port"
    user =
      fmap fromString $ strOption $
        long (prefixed "user") <>
        value "postgres" <>
        showDefault <>
        help "Username"
    password =
      fmap fromString $ strOption $
        long (prefixed "password") <>
        value "" <>
        showDefault <>
        help "Password"
    database =
      fmap fromString $ strOption $
        long (prefixed "database") <>
        value "" <>
        showDefault <>
        help "Database name"
    prefixed s = 
      maybe s (<> ("-" <> s)) prefix
