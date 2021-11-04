{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram 
( Config(..)
, Handle(..)
, withHandle
) where


import qualified Data.Aeson                 as A
import qualified System.IO                  as IO
import           Control.Exception             ( bracket )
import           Data.ByteString.Lazy.Char8    ( pack )


data Config = Config
            { cPath :: FilePath 
            } deriving Show


data Handle = Handle
            { hType :: String
            , hToken :: String 
            } deriving Show


instance A.FromJSON Handle where
  parseJSON = A.withObject "Handle" $ \ o ->
    Handle <$> o A..: "type" <*> o A..: "token"


withHandle :: Config -> (a -> b) -> IO ()
withHandle config _ = bracket
  ( IO.openFile path IO.ReadMode )
  ( IO.hClose )
  ( \ file -> do
      content <- IO.hGetContents file 

      case ( A.decode ( pack content ) :: Maybe Handle ) of
        Nothing -> IO.hPutStrLn IO.stderr $ "Failed to parse " ++ path
        Just h  -> IO.hPutStrLn IO.stderr $ "Handle: " ++ show h
  )
  where
    path = cPath config
