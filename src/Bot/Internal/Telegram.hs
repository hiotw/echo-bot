{-# LANGUAGE OverloadedStrings #-}

module Bot.Internal.Telegram 
( Config(..)
, Handle(..)
, new
, ping
, empty
, update
, echo
) where


import qualified Data.Aeson                 as A
import qualified System.IO                  as IO
import           Data.Maybe                    ( fromMaybe )
import           Control.Applicative           ( (<|>) )
import           Control.Exception             ( bracket )
import           Data.ByteString.Lazy.Char8    ( pack, ByteString )
import           Network.HTTP.Client           ( responseBody )
import           Network.HTTP.Simple           ( httpLBS
                                               , parseRequest
                                               , getResponseStatusCode 
                                               )


data Config = Config
            { cPath :: FilePath 
            } deriving Show


data Handle = Handle
            { hType :: String
            , hToken :: String
            } deriving ( Show, Eq )


data Message = Message
             { mChatId :: Int
             , mId     :: Int
             , mOffset :: Int
             } deriving Show


empty :: Handle
empty = Handle
      { hType = mempty
      , hToken = mempty
      }


instance A.FromJSON Handle where
  parseJSON = A.withObject "Handle" $ \ o ->
    Handle <$> o A..: "type" <*> o A..: "token"


instance A.FromJSON Message where
  parseJSON = A.withObject "Message" $ \ o -> Message
    <$> ( ( ( firstResult o ) >>= ( A..: "chat" ) ) >>= ( A..: "id" ) )
    <*> ( ( firstResult o ) >>= ( A..: "message_id" ) )
    <*> ( ( o A..: "result" ) >>= ( \ (el:_) -> el A..: "update_id" )  )
    where
      firstResult o = do
        (result:_) <- o A..: "result"
        result A..: "message"
      

new :: Config -> IO Handle
new config = bracket
  ( IO.openFile path IO.ReadMode )
  ( IO.hClose )
  ( \ file -> do
      content <- IO.hGetContents file 

      case ( A.decode ( pack content ) :: Maybe Handle ) of
        Nothing -> do
          IO.hPutStrLn IO.stderr $ "Failed to parse " ++ path
          return empty

        Just h  -> do
          IO.hPutStrLn IO.stderr $ "Parsed successfuly: " ++ path
          return h
  )
  where
    path = cPath config


ping :: Handle -> IO ()
ping handle = do
  request <- parseRequest url
  response <- httpLBS request
  IO.hPutStrLn IO.stderr $ "Ping result: "
                           ++ ( show $ getResponseStatusCode response )
  where
    url = "https://api.telegram.org/bot" <> ( hToken handle ) <> "/getMe"


update :: Handle -> Int -> IO ByteString
update handle offset = do
  request <- parseRequest url
  response <- httpLBS request
  IO.hPutStrLn IO.stderr $ "Update result: "
                           ++ ( show $ getResponseStatusCode response )
  IO.hPutStrLn IO.stderr $ "Response body: "
                           ++ ( show $ responseBody response )

  return $ responseBody response

  where
    url = "https://api.telegram.org/bot" 
          <> ( hToken handle ) 
          <> "/getUpdates?allowed_updates=poll&limit=1&offset="
          <> ( show offset )


echo :: Handle -> IO ()
echo handle = do
  message <- update handle 0
  processMessages handle ( A.decode message :: Maybe Message )


processMessages :: Handle -> Maybe Message -> IO ()
processMessages handle Nothing          = IO.hPutStrLn IO.stderr "No updates"
processMessages handle ( Just message ) = do
  IO.hPutStrLn IO.stderr $ "Message: " <> ( show $ message )

  request <- parseRequest $ url message
  response <- httpLBS request

  IO.hPutStrLn IO.stderr $ "Echo result: "
                         <> ( show $ getResponseStatusCode response )
  IO.hPutStrLn IO.stderr $ "Response body: "
                         <> ( show $ responseBody response )

  nextMessage <- update handle $ mOffset message + 1
  processMessages handle ( A.decode nextMessage :: Maybe Message )
  where
    url m = "https://api.telegram.org/bot" 
          <> ( hToken handle ) 
          <> "/copyMessage?chat_id=" <> ( show $ mChatId m )
          <> "&from_chat_id=" <> ( show $ mChatId m )
          <> "&message_id=" <> ( show $ mId m )
