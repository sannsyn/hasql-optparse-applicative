module Hasql.OptparseApplicative where

import BasePrelude
import Options.Applicative
import qualified Hasql.Connection as A
import qualified Hasql.Pool as B


-- |
-- Given a function, which updates the long names produces a parser of @B.'B.Settings'@.
-- You can use this function to prefix the name or you can just specify 'id', if you don't want it changed.
poolSettings :: (String -> String) -> Parser B.Settings
poolSettings updatedName =
  (,,) <$> size <*> timeout <*> connectionSettings updatedName
  where
    size =
      option auto $
        long (updatedName "pool-size") <>
        value 1 <>
        showDefault <>
        help "Amount of connections in the pool"
    timeout =
      fmap fromIntegral $
      option auto $
        long (updatedName "pool-timeout") <>
        value 10 <>
        showDefault <>
        help "Amount of seconds for which the unused connections are kept open"

-- |
-- Given a function, which updates the long names produces a parser of @A.'A.Settings'@.
-- You can use this function to prefix the name or you can just specify 'id', if you don't want it changed.
connectionSettings :: (String -> String) -> Parser A.Settings
connectionSettings updatedName =
  A.settings <$> host <*> port <*> user <*> password <*> database
  where
    host =
      fmap fromString $ strOption $
        long (updatedName "host") <> 
        value "127.0.0.1" <>
        showDefault <>
        help "Server host"
    port =
      option auto $
        long (updatedName "port") <>
        value 5432 <>
        showDefault <>
        help "Server port"
    user =
      fmap fromString $ strOption $
        long (updatedName "user") <>
        value "postgres" <>
        showDefault <>
        help "Username"
    password =
      fmap fromString $ strOption $
        long (updatedName "password") <>
        value "" <>
        showDefault <>
        help "Password"
    database =
      fmap fromString $ strOption $
        long (updatedName "database") <>
        help "Database name"
