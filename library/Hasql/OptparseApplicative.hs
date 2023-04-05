module Hasql.OptparseApplicative where

import qualified Attoparsec.Time.Text as C
import BasePrelude
import qualified Data.Attoparsec.Text as D
import qualified Hasql.Connection as A
import qualified Hasql.Pool as B
import Options.Applicative

-- | Given a function, which updates the long names, produces a parser of
-- the @Hasql.Pool.'acquire'@ operation.
--
-- You can use this function to prefix the name or you can just specify 'id',
-- if you don't want it changed.
poolSettings :: (String -> String) -> Parser (IO B.Pool)
poolSettings updatedName =
  B.acquire
    <$> size
    <*> acquisitionTimeout
    <*> connectionLifetime
    <*> connectionSettings updatedName
  where
    size =
      option auto . mconcat $
        [ long (updatedName "pool-size"),
          value 1,
          showDefault,
          help "Amount of connections in the pool"
        ]
    acquisitionTimeout =
      attoparsedOption C.diffTime . mconcat $
        [ long (updatedName "pool-acquisition-timeout"),
          value 10,
          showDefault,
          help "How long it takes until the attempt to connect is considered timed out"
        ]
    connectionLifetime =
      attoparsedOption C.diffTime . mconcat $
        [ long (updatedName "pool-connection-lifetime"),
          value (fromIntegral (24 * 60 * 60)),
          showDefault,
          help "Maximal lifetime for connections. Allows to periodically clean up the connection resources to avoid server-side leaks"
        ]

-- | Given a function, which updates the long names produces a parser
-- of @Hasql.Connection.'A.Settings'@.
--
-- You can use this function to prefix the name or you can just specify 'id',
-- if you don't want it changed.
connectionSettings :: (String -> String) -> Parser A.Settings
connectionSettings updatedName =
  A.settings <$> host <*> port <*> user <*> password <*> database
  where
    host =
      fmap fromString $
        strOption $
          long (updatedName "host")
            <> value "127.0.0.1"
            <> showDefault
            <> help "Server host"
    port =
      option auto $
        long (updatedName "port")
          <> value 5432
          <> showDefault
          <> help "Server port"
    user =
      fmap fromString $
        strOption $
          long (updatedName "user")
            <> value "postgres"
            <> showDefault
            <> help "Username"
    password =
      fmap fromString $
        strOption $
          long (updatedName "password")
            <> value ""
            <> showDefault
            <> help "Password"
    database =
      fmap fromString $
        strOption $
          long (updatedName "database")
            <> help "Database name"

-- * Helpers

-- timeout name def help =
--   attoparsedOption C.diffTime mconcat $
--     [ long name,
--       value def,
--       showDefault,
--       help "How long it takes until the attempt to connect is considered timed out. In seconds"
--     ]
--   where
--     reader = eitherReader $

attoparsedOption :: D.Parser a -> Mod OptionFields a -> Parser a
attoparsedOption parser =
  option $ eitherReader $ D.parseOnly (parser <* D.endOfInput) . fromString
