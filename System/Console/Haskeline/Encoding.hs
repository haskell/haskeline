module System.Console.Haskeline.Encoding (encode, decode) where

import System.Console.Haskeline.InputT
import System.Console.Haskeline.Term
import System.Console.Haskeline.Monads
import Data.ByteString (ByteString)

-- TODO: Docs
-- Mention that this is very terminal-dependent and probably not suited for storing
-- data in files.

encode :: MonadIO m => String -> InputT m ByteString
encode str = do
    encoder <- asks encodeForTerm
    liftIO $ encoder str

decode :: MonadIO m => ByteString -> InputT m String
decode str = do
    decoder <- asks decodeForTerm
    liftIO $ decoder str
