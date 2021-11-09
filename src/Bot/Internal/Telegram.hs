{-# LANGUAGE OverloadedStrings #-}

module Bot.Internal.Telegram 
( Config(..)
, Handle(..)
, new
, ping
, empty
, update
) where


import qualified Data.Aeson                 as A
import qualified System.IO                  as IO
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


empty :: Handle
empty = Handle
      { hType = mempty
      , hToken = mempty
      }


instance A.FromJSON Handle where
  parseJSON = A.withObject "Handle" $ \ o ->
    Handle <$> o A..: "type" <*> o A..: "token"


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


update :: Handle -> IO ByteString
update handle = do
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
          <> "/getUpdates?allowed_updates=poll"
