{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram 
( Config(..)
, Handle
, new
, ping
, update
, echo
) where

import Bot.Internal.Telegram
