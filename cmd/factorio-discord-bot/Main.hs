module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Char (isAlphaNum)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, getCurrentTime, parseTimeM)
import Data.Time.Clock (UTCTime (..))
import Network.HTTP.Req (
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  Scheme (Https),
  Url,
  defaultHttpConfig,
  ignoreResponse,
  req,
  runReq,
  useHttpsURI,
 )
import Options.Applicative.Builder (
  ReadM,
  fullDesc,
  help,
  info,
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
  strOption,
  switch,
 )
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Types qualified as Args (Parser, ParserInfo)
import Relude
import System.Directory (doesFileExist)
import Text.Megaparsec (
  Parsec,
  chunk,
  errorBundlePretty,
  runParser,
  single,
  takeP,
  takeWhile1P,
  try,
 )
import Text.URI (mkURI)

data Options = Options
  { factorioStdoutFile :: FilePath
  , discordWebhookURL :: Url 'Https
  , debug :: Bool
  }
  deriving (Show)

optionsParser :: Args.Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "factorio-stdout-file"
          <> metavar "FILE"
          <> help "path to file where Factorio stdout is being streamed"
      )
    <*> option
      parseURLFlag
      ( long "discord-webhook-url"
          <> metavar "URL"
          <> help "Discord webhook URL"
      )
    <*> switch (long "debug" <> help "Output debug information")

parseURLFlag :: ReadM (Url 'Https)
parseURLFlag = maybeReader $ \s -> do
  uri <- mkURI $ toText s
  fst <$> useHttpsURI uri

argparser :: Args.ParserInfo Options
argparser =
  info
    (optionsParser <**> helper)
    (fullDesc <> progDesc "Send a Discord message when someone joins or leaves your Factorio server")

-- Poll the file every second. If a new join or leave notification has occurred,
-- send a Discord webhook message.
main :: IO ()
main = do
  opts@Options{factorioStdoutFile, debug} <- execParser argparser
  exists <- doesFileExist factorioStdoutFile
  unless exists $ die $ "Error: file at " <> show factorioStdoutFile <> " does not exist"

  when debug $ putStrLn $ "Command line parameters: " <> show opts

  now <- getCurrentTime
  void $ flip runStateT now $ forever $ poll opts

-- If this becomes a performance issue, we should tail the file. See, for
-- example:
-- - https://stackoverflow.com/questions/41230293/how-to-efficiently-follow-tail-a-file-with-haskell-including-detecting-file
-- - https://hackage.haskell.org/package/tailfile-hinotify-2.0.0.0/docs/System-IO-TailFile.html
poll :: (MonadIO m) => Options -> StateT UTCTime m ()
poll Options{factorioStdoutFile, discordWebhookURL, debug} = do
  when debug $ do
    now <- liftIO getCurrentTime
    putStrLn $ "Polling at: " <> show now

  -- Read and parse the file.
  (notifs, players) <- parseLog factorioStdoutFile

  -- Send any new notifications.
  lastNotificationTime <- get
  let notifs' = filter ((> lastNotificationTime) . time) notifs

  when debug $ do
    putStrLn $ "Last notification time: " <> show lastNotificationTime
    putStrLn $ "Players: " <> show players
    putStrLn $ "Notifications: " <> show notifs
    putStrLn $ "New notifications: " <> show notifs'

  unless (null notifs') $
    void $
      runReq defaultHttpConfig $
        req
          POST
          discordWebhookURL
          (ReqBodyJson $ Map.singleton @Text "content" $ makeMessage notifs' players)
          ignoreResponse
          mempty

  -- Update the last notification time.
  case nonEmpty notifs' of
    Nothing -> pure ()
    Just ns -> put $ time $ last ns

  -- Sleep for a bit.
  when debug $ putStrLn $ replicate 80 '-'
  liftIO $ threadDelay 1_000_000
 where
  renderN :: Notification -> Text
  renderN Notification{player, action} =
    "Player " <> player <> case action of
      Join -> " joined the game"
      Leave -> " left the game"

  renderP :: Set Player -> Text
  renderP players =
    "Current players: "
      <> if Set.null players then "none" else T.intercalate ", " $ Set.toList players

  makeMessage :: [Notification] -> Set Player -> Text
  makeMessage notifs players =
    T.intercalate "\n" (fmap renderN notifs)
      <> "\n\n"
      <> renderP players

type Parser = Parsec Void Text

type Player = Text

data Notification = Notification
  { player :: Player
  , time :: UTCTime
  , action :: Action
  }
  deriving (Show)

data Action = Join | Leave deriving (Show)

parseLog :: (MonadIO m) => FilePath -> m ([Notification], Set Player)
parseLog filepath = do
  contents <- readFileText filepath
  notifs <- case runParser logParser filepath contents of
    Left err -> die $ "Could not parse log file: " <> errorBundlePretty err
    Right r -> pure r
  let sortedNotifs = sortOn time notifs
  pure (sortedNotifs, foldl' foldPlayers mempty sortedNotifs)
 where
  foldPlayers :: Set Player -> Notification -> Set Player
  foldPlayers players Notification{player, action} = case action of
    Join -> Set.insert player players
    Leave -> Set.delete player players

-- Note that we do not require eof to handle an edge case: if we read the file
-- while it's being written, the last line will be garbage.
logParser :: Parser [Notification]
logParser = catMaybes <$> many lineP
 where
  -- Lines are either a notification, or not.
  lineP :: Parser (Maybe Notification)
  lineP =
    (Just <$> try notifP)
      <|> (takeWhile1P Nothing ('\n' /=) >> single '\n' >> pure Nothing)

  -- Notifications are a timestamp, action, player, and some filler.
  notifP :: Parser Notification
  notifP = do
    time <- timeP
    _ <- single ' '
    (action, player) <- eventP
    _ <- single '\n'
    pure Notification{player, time, action}

  -- FIXME: This will break in about 8000 years, when we have 5-digit years.
  timeP :: Parser UTCTime
  timeP = do
    t <- takeP (Just "timestamp") $ 4 + 1 + 2 + 1 + 2 + 1 + 2 + 1 + 2 + 1 + 2
    parseTimeM False defaultTimeLocale "%Y-%0m-%0d %0T" $ toString t

  eventP :: Parser (Action, Player)
  eventP = do
    _ <- single '['
    action <- (chunk "JOIN" >> pure Join) <|> (chunk "LEAVE" >> pure Leave)
    _ <- chunk "] "
    player <- takeWhile1P (Just "username") isAlphaNum
    _ <- chunk $ case action of
      Join -> " joined the game"
      Leave -> " left the game"
    pure (action, player)
