module Options
( Options (..)
, getOptions
) where

import Options.Applicative

data Options = Options
  { optionsHost :: String
  , optionsPort :: Int
  , optionsAccountsPath :: String
  , optionsDatabasePath :: String
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> option str
      (long "host"
      <> metavar "HOST"
      <> help "Host to listen on"
      <> value "127.0.0.1"
      <> showDefault)
  <*> option auto
      (long "port"
      <> metavar "PORT"
      <> help "Port to listen on"
      <> value 7123
      <> showDefault)
  <*> option str
      (long "accounts"
      <> metavar "PATH"
      <> help "Account list path"
      <> value "localscrobble-accounts.txt"
      <> showDefault)
  <*> option str
      (long "database"
      <> metavar "PATH"
      <> help "Database path"
      <> value "localscrobble.db"
      <> showDefault)

getOptions :: IO Options
getOptions = execParser $ info (optionsParser <**> helper)
  (fullDesc <> header "localscrobble - local scrobbling server")
