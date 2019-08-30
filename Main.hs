{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Control.Monad (join, void, when)
import Control.Exception (bracket, bracketOnError)
import Safe

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Network.HTTP.Types

import Data.Monoid ((<>))
import qualified Data.Traversable as Tr
import Data.Maybe (isJust, catMaybes, listToMaybe)
import Data.Int (Int64)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

import Data.Time.Clock.POSIX

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite3 as RawSQL

import Options (Options (..), getOptions)

main :: IO ()
main = do
  Options
    { optionsHost = host
    , optionsPort = port
    , optionsDatabasePath = dbPath }
      <- getOptions
  withDB dbPath setupDB

  let formatHost host = if ':' `elem` host then "[" <> host <> "]" else host
      baseURL = "http://" <> LB.pack (formatHost host) <> ":" <> LB.pack (show port)
      settings =
        Warp.setPort port
        $ Warp.setHost (fromString host)
        $ Warp.defaultSettings
  withStdoutLogger $ \logger ->
    Warp.runSettings settings (app dbPath baseURL logger)

app :: FilePath -> LB.ByteString -> ApacheLogger -> Application
app dbPath baseURL logger req respond = do
  let path = rawPathInfo req
  body <- strictRequestBody req
  let urlQuery = queryString req
      wantsHandshake = ("hs", Just "true") `elem` urlQuery
      isGET = requestMethod req == methodGet
      isPOST = requestMethod req == methodPost

  let q = parseQuery $ LB.toStrict body
      sessionID = fromJustNote "session id" $ queryGetText q "s"
      username = sessionID

  (st, msg) <-
    if isPOST && path == submissionsPath then do
      withDB dbPath $ \db -> withTransaction db $
        mapM_ (insertScrobble db username) . catMaybes . takeWhile isJust $
          map (\idx -> lookupScrobble (<> "[" <> B.pack (show idx) <> "]") q)
          [(0 :: Int)..]
      return (status200, "OK\n")
    else if isPOST && path == nowPlayingPath then do
      let t = fromJustNote "read track" $ lookupTrack id q
      withDB dbPath $ \db -> withTransaction db $
        insertNowPlaying db username t
      return (status200, "OK\n")
    else return $ if isGET && wantsHandshake then
      let user = fromJustNote "username" $ queryGetText urlQuery "u" in
      (status200, handshakeResponse baseURL user)
    else
      (status404, "Not Found\n")

  logger req st (Just . fromIntegral $ LB.length msg)
  respond $ responseLBS st [] msg

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
queryGetRead q x = readNote ("queryGetRead: " <> B.unpack x) . B.unpack <$> queryGet q x

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

handshakeResponse :: LB.ByteString -> Username -> LB.ByteString
handshakeResponse baseURL user =
  "OK\n"
  <> LB.fromStrict (T.encodeUtf8 user)
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
