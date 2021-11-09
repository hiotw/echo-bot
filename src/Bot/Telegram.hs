{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram 
( Config(..)
, Handle
, new
, ping
, update
) where

import Bot.Internal.Telegram
