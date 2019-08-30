{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad (guard, join, void, when)
import Control.Exception (bracket, bracketOnError)
import Safe

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Network.HTTP.Types

import qualified Crypto.Hash.MD5 as MD5
import System.Entropy (getEntropy)

import Data.Monoid ((<>))
import qualified Data.Traversable as Tr
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Int (Int64)
import Data.List (find)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Base16 as B16

import Data.Time.Clock.POSIX

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite3 as RawSQL

import Options (Options (..), getOptions)

main :: IO ()
main = do
  Options
    { optionsHost = host
    , optionsPort = port
    , optionsAccountsPath = accountsPath
    , optionsDatabasePath = dbPath
    } <- getOptions

  accounts <- parseAccounts <$> T.readFile accountsPath
  sessions <- newMVar []
  withDB dbPath setupDB

  let formatHost host = if ':' `elem` host then "[" <> host <> "]" else host
      baseURL = "http://" <> LB.pack (formatHost host) <> ":" <> LB.pack (show port)
      settings =
        Warp.setPort port
        $ Warp.setHost (fromString host)
        $ Warp.defaultSettings
  withStdoutLogger $ \logger ->
    Warp.runSettings settings (app dbPath baseURL logger accounts sessions)

app :: FilePath -> LB.ByteString -> ApacheLogger -> Accounts -> MVar Sessions -> Application
app dbPath baseURL logger accounts sessions req respond = do
  let path = rawPathInfo req
  body <- strictRequestBody req
  let urlQuery = queryString req
      wantsHandshake = ("hs", Just "true") `elem` urlQuery
      isGET = requestMethod req == methodGet
      isPOST = requestMethod req == methodPost

      q = parseQuery $ LB.toStrict body
      mSessionID = queryGet q "s"

      ok = (status200, "OK\n")
      missingParameters = (status400, "FAILED Missing Parameters")
      failAuth reason = (status403, LB.pack (show reason) <> "\n")

  mAuthenticatedUser <- case mSessionID of
    Just sessionID | isPOST && not wantsHandshake ->
      findSessionUser sessions sessionID
    Nothing -> pure Nothing

  (st, msg) <- case mAuthenticatedUser of
    Just username | isPOST && path == submissionsPath -> do
      withDB dbPath $ \db -> withTransaction db $
        mapM_ (insertScrobble db username) . catMaybes . takeWhile isJust $
          map (\idx -> lookupScrobble (<> "[" <> B.pack (show idx) <> "]") q)
          [(0 :: Int)..]
      pure ok
    Just username | isPOST && path == nowPlayingPath -> do
      let t = fromJustNote "read track" $ lookupTrack id q
      withDB dbPath $ \db -> withTransaction db $
        insertNowPlaying db username t
      pure ok
    Nothing | isPOST -> pure $ failAuth BADAUTH
    Nothing | isGET && wantsHandshake -> do
      let user = fromJustNote "username" $ queryGetText urlQuery "u"
      case queryHandshake urlQuery of
        Nothing -> pure missingParameters
        Just handshake -> do
          mSessionID <- login accounts sessions handshake
          pure $ case mSessionID of
            Right sessionID -> (status200, handshakeResponse baseURL sessionID)
            Left err -> failAuth err
    _ ->
      pure (status404, "Not Found\n")

  logger req st (Just . fromIntegral $ LB.length msg)
  respond $ responseLBS st [] msg

-- Authentication

type Accounts = [(Username, B.ByteString)]

parseAccounts :: Text -> Accounts
parseAccounts = map parseLine . T.lines
  where parseLine line = let (u, p) = T.breakOn ":" line
                         in (u, T.encodeUtf8 (T.tail p))

type SessionID = B.ByteString
type Sessions = [(SessionID, Username)]

getSession :: MVar Sessions -> Username -> IO SessionID
getSession sv user = do
  modifyMVar sv $ \sessions ->
    case find ((== user) . snd) sessions of
      Just (sessionID, _) -> pure (sessions, sessionID)
      Nothing -> do
        sessionID <- B16.encode <$> getEntropy 16
        pure ((sessionID, user) : sessions, sessionID)

findSessionUser :: MVar Sessions -> SessionID -> IO (Maybe Username)
findSessionUser sv sessionID =
  fmap snd . find ((== sessionID) . fst) <$> readMVar sv

data LoginError = BADAUTH | BADTIME
  deriving (Show)

data Handshake = Handshake
  { hsUser :: Text
  , hsToken :: B.ByteString
  , hsTimestamp :: Int64
  , hsTimestampString :: B.ByteString
  }

queryHandshake :: Query -> Maybe Handshake
queryHandshake q = do
  guard $ ("hs", Just "true") `elem` q
  u <- queryGetText q "u"
  a <- queryGet q "a"
  ts <- queryGet q "t"
  ti <- readMay $ B.unpack ts

  pure Handshake
    { hsUser = u
    , hsToken = a
    , hsTimestampString = ts
    , hsTimestamp = ti
    }

checkTime :: Handshake -> IO Bool
checkTime hs = do
  currentTime <- getPOSIXTime
  let maxClockDifference = 10
      timestamp = hsTimestamp hs
      currentSeconds :: Int64
      currentSeconds = round currentTime
  pure $ abs (timestamp - currentSeconds) < maxClockDifference

checkToken :: Accounts -> Handshake -> Maybe Username
checkToken accounts hs = do
  let user = hsUser hs
  password <- lookup user accounts
  let expectedToken = md5 (md5 password <> hsTimestampString hs)
  if expectedToken == hsToken hs then Just user else Nothing

-- | MD5 with output in lowercase hexadecimal as specified in the
-- scrobbling submission protocol.
md5 :: B.ByteString -> B.ByteString
md5 = B16.encode . MD5.hash

note :: a -> Maybe b -> Either a b
note x Nothing = Left x
note _ (Just x) = Right x

login :: Accounts -> MVar Sessions -> Handshake -> IO (Either LoginError SessionID)
login accounts sessions hs = do
  timeOk <- checkTime hs
  if timeOk then
    sequence . note BADAUTH $ getSession sessions <$> checkToken accounts hs
  else
    pure $ Left BADTIME

-- Scrobble type

lookupTrack :: (B.ByteString -> B.ByteString) -> Query -> Maybe Track
lookupTrack accessor query = do
  let getRead = queryGetRead query . accessor
      getText = queryGetText query . accessor

  a <- getText "a"
  t <- getText "t"
  let l = getRead "l"
      b = getText "b"
      n = getText "n"
      m = getText "m"

  return Track
    { trackTitle = t
    , trackArtist = a
    , trackAlbum = b
    , trackLength = l
    , trackNumber = n
    , trackMusicBrainzID = m
    }

lookupScrobble :: (B.ByteString -> B.ByteString) -> Query -> Maybe Scrobble
lookupScrobble accessor query = do
  let get = queryGet query . accessor
      getRead = queryGetRead query . accessor

  track <- lookupTrack accessor query
  i <- getRead "i"
  o <- parseScrobbleSource . B.unpack =<< get "o"
  let r = parseScrobbleRating . B.unpack =<< get "r"

  return Scrobble
    { scrobbleTrack = track
    , scrobbleSource = o
    , scrobbleRating = r
    , scrobbleTime = i
    }

queryGet :: Query -> B.ByteString -> Maybe B.ByteString
queryGet q x = nonempty =<< join (lookup x q)

queryGetRead :: Read a => Query -> B.ByteString -> Maybe a
queryGetRead q x = readMay . B.unpack =<< queryGet q x

queryGetText :: Query -> B.ByteString -> Maybe Text
queryGetText q x = T.decodeUtf8 <$> queryGet q x

data Track = Track
  { trackTitle :: Text
  , trackArtist :: Text
  , trackAlbum :: Maybe Text
  , trackLength :: Maybe Int
  , trackNumber :: Maybe Text
  , trackMusicBrainzID :: Maybe Text
  }
  deriving (Eq, Show)

type EpochTime = Int64

data Scrobble = Scrobble
  { scrobbleTrack :: Track
  , scrobbleSource :: ScrobbleSource
  , scrobbleRating :: Maybe ScrobbleRating
  , scrobbleTime :: EpochTime
  }
  deriving (Eq, Show)

data ScrobbleSource
  = SourceP
  | SourceR
  | SourceE
  | SourceL
  deriving (Eq, Show)

data ScrobbleRating
  = RatingLove
  | RatingBan
  | RatingSkip
  deriving (Eq, Show)

parseScrobbleSource :: String -> Maybe ScrobbleSource
parseScrobbleSource s = case s of
  "P" -> Just SourceP
  "R" -> Just SourceR
  "E" -> Just SourceE
  'L' : _ -> Just SourceL
  _ -> Nothing

scrobbleSourceLetter :: IsString s => ScrobbleSource -> s
scrobbleSourceLetter s = case s of
  SourceP -> "P"
  SourceR -> "R"
  SourceE -> "E"
  SourceL -> "L"

parseScrobbleRating :: String -> Maybe ScrobbleRating
parseScrobbleRating s = case s of
  "L" -> Just RatingLove
  "B" -> Just RatingBan
  "S" -> Just RatingSkip
  _ -> Nothing

scrobbleRatingLetter :: IsString s => ScrobbleRating -> s
scrobbleRatingLetter s = case s of
  RatingLove -> "L"
  RatingBan -> "B"
  RatingSkip -> "S"

nonempty :: (IsString s, Eq s) => s -> Maybe s
nonempty "" = Nothing
nonempty x = Just x

-- Routing

nowPlayingPath :: IsString s => s
nowPlayingPath = "/nowplaying/"

submissionsPath :: IsString s => s
submissionsPath = "/submissions/"

handshakeResponse :: LB.ByteString -> SessionID -> LB.ByteString
handshakeResponse baseURL sessionID =
  "OK\n"
  <> LB.fromStrict sessionID
  <> "\n"
  <> baseURL <> nowPlayingPath <> "\n"
  <> baseURL <> submissionsPath <> "\n"

-- Database

type RowID = Int64
type Username = Text

insertNowPlaying :: SQL.Connection -> Username -> Track -> IO RowID
insertNowPlaying db user tr = do
  ui <- insertName db "user" user
  tri <- insertTrack db tr
  now <- realToFrac <$> getPOSIXTime :: IO Double
  let nowInt = round now :: Int64
  SQL.execute db "DELETE FROM now_playing WHERE user_id = ?" (SQL.Only ui)
  insertValues db "now_playing" ["user_id", "track_id", "timestamp"] (ui, tri, nowInt)

insertScrobble :: SQL.Connection -> Username -> Scrobble -> IO RowID
insertScrobble db user s = do
  let so = scrobbleSourceLetter (scrobbleSource s) :: Text
      ra = scrobbleRatingLetter <$> scrobbleRating s :: Maybe Text
      ts = scrobbleTime s
  ui <- insertName db "user" user
  tri <- insertTrack db $ scrobbleTrack s
  insertValues db "scrobble" ["user_id", "track_id", "source", "rating", "timestamp"] (ui, tri, so, ra, ts)

insertTrack :: SQL.Connection -> Track -> IO RowID
insertTrack db t = do
  ti <- insertName db "title" $ trackTitle t
  ai <- insertName db "artist" $ trackArtist t
  bi <- Tr.sequence $ insertName db "album" <$> trackAlbum t
  mi <- Tr.sequence $ insertName db "musicbrainz" <$> trackMusicBrainzID t
  selectOrInsertValues db "track" ["title_id", "artist_id", "album_id", "musicbrainz_id", "length", "number"] (ti, ai, bi, mi, trackLength t, trackNumber t)

newtype TableName = TableName { unTableName :: Text }
  deriving (Show)

type ColumnName = TableName

instance IsString TableName where
  fromString = TableName . fromString

insertName :: SQL.Connection -> TableName -> Text -> IO RowID
insertName db t n = selectOrInsertValues db t ["name"] (SQL.Only n)

insertValues :: SQL.ToRow a => SQL.Connection -> TableName -> [ColumnName] -> a -> IO RowID
insertValues db (TableName table) cols' x = do
  SQL.execute db insertQuery x
  SQL.lastInsertRowId db
  where
    cols = map unTableName cols'
    substTuple = tuple $ map (const "?") cols
    insertQuery = SQL.Query $
      "INSERT INTO " <> table
      <> tuple cols <> " values " <> substTuple

selectValues :: SQL.ToRow a => SQL.Connection -> TableName -> [ColumnName] -> a -> IO (Maybe RowID)
selectValues db (TableName table) cols' x =
    fmap SQL.fromOnly . listToMaybe <$> SQL.query db selectQuery x
  where
    cols = map unTableName cols'
    ands = T.intercalate " AND " $ map (<> " IS ?") cols
    selectQuery = SQL.Query $
      "SELECT id FROM " <> table <> " WHERE " <> ands

selectOrInsertValues :: SQL.ToRow a => SQL.Connection -> TableName -> [ColumnName] -> a -> IO RowID
selectOrInsertValues db t cols' x = do
  mi <- selectValues db t cols' x
  case mi of
    Nothing -> insertValues db t cols' x
    Just i -> return i

openDB :: FilePath -> IO SQL.Connection
openDB path = do
  db <- SQL.open path
  when False $
    SQL.setTrace db . Just $ \s -> T.putStrLn $ "SQL: " <> s
  SQL.execute_ db "PRAGMA foreign_keys = ON"
  return db

withDB :: FilePath -> (SQL.Connection -> IO a) -> IO a
withDB dbPath = bracket (openDB dbPath) SQL.close

setupDB :: SQL.Connection -> IO ()
setupDB db = RawSQL.exec (SQL.connectionHandle db) schema

withTransaction :: SQL.Connection -> IO a -> IO a
withTransaction db f =
  bracketOnError
    (void $ SQL.execute_ db "BEGIN TRANSACTION")
    (const $ SQL.execute_ db "ROLLBACK")
    (const $ f <* SQL.execute_ db "COMMIT TRANSACTION")

commas :: [Text] -> Text
commas = T.intercalate ","

tuple :: [Text] -> Text
tuple xs = "(" <> commas xs <> ")"

schema :: Text
schema = T.unlines
  [ nameTable "title"
  , nameTable "artist"
  , nameTable "album"
  , nameTable "musicbrainz"
  , nameTable "user"
  , "CREATE TABLE IF NOT EXISTS track"
  , "( id INTEGER PRIMARY KEY"
  , ", " <> foreignKey "title" True
  , ", " <> foreignKey "artist" True
  , ", " <> foreignKey "album" False
  , ", " <> foreignKey "musicbrainz" False
  , ", length INTEGER"
  , ", number TEXT"
  , ");"
  , "CREATE TABLE IF NOT EXISTS scrobble"
  , "( id INTEGER PRIMARY KEY"
  , ", " <> foreignKey "user" True
  , ", " <> foreignKey "track" True
  , ", source TEXT NOT NULL"
  , ", rating TEXT"
  , ", timestamp INTEGER NOT NULL"
  , ");"
  , "CREATE TABLE IF NOT EXISTS now_playing"
  , "( id INTEGER PRIMARY KEY"
  , ", " <> foreignKey "user" True
  , ", " <> foreignKey "track" True
  , ", timestamp INTEGER NOT NULL"
  , ");"
  ]
  where nameTable name = T.unlines
          [ "CREATE TABLE IF NOT EXISTS " <> name
          , "( id INTEGER PRIMARY KEY"
          , ", name TEXT NOT NULL UNIQUE"
          , ");"
          ]
        foreignKey name required =
          name <> "_id INTEGER"
          <> (if required then " NOT NULL" else "")
          <> " REFERENCES " <> name
