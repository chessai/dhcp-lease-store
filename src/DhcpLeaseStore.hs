{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module DhcpLeaseStore
  (
  ) where

import Control.Applicative ((<|>))
import Data.ByteString.Streaming.Char8
import Data.Map.Strict (Map)
import Net.Types (IPv4,Mac)
import qualified Net.IPv4 as I4
import qualified Net.Mac as Mac
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map.Strict as M
import qualified Data.Attoparsec.ByteString.Char8 as A

data BindingState
  = BindingStateFree
  | BindingStateActive
  | BindingStateAbandoned

isActive :: BindingState -> Bool
isActive BindingStateActive = True
isActive _ = False

data Pertinent
  = PAddr !IPv4
  | PMac !Mac
  -- | PBindingState !BindingState

datum :: BC8.ByteString -> A.Parser ()
datum n = A.skipSpace >> A.string n >> A.skipSpace

parseAddr :: A.Parser IPv4
parseAddr = datum "lease" *> I4.parserUtf8

parseMac :: A.Parser Mac
parseMac = datum "hardware ethernet" *> Mac.parserUtf8

parseBindingState :: A.Parser BindingState
parseBindingState = do
  datum "binding state"
  (BindingStateFree <$ A.string "free")
  <|> (BindingStateActive <$ A.string "active")
  <|> (BindingStateAbandoned <$ A.string "abandoned")

parsePertinent :: A.Parser Pertinent
parsePertinent = do
  (PAddr <$> parseAddr) <|> (PMac <$> parseMac) -- <|> (PBindingState <$> parseBindingState)
 
pertinent :: BC8.ByteString -> Maybe Pertinent
pertinent = A.maybeResult . A.parse parsePertinent

-- | Return the first IPv4 associated with the given mac address.
--
--   Note: This does not account for lease times.
--   Note: This does not account for binding states.
findIp :: Monad m => Mac -> ByteString m () -> m (Maybe IPv4)
findIp mac stream = let
  go !mip strm = nextChunk strm >>= \case
    Left () -> pure mip
    Right (x,rest) -> do
      case pertinent x of
        Nothing -> go mip rest
        Just p -> case p of  
          PAddr addr -> go (Just addr) rest
          PMac macFound -> if mac == macFound
            then case mip of
              Just ip -> pure (Just ip)
              _ -> go mip rest
            else go mip rest
  in go Nothing stream
