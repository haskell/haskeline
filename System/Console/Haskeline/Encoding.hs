{- |

This module exposes the console Unicode API which is used by the functions
in "System.Console.Haskeline".  On POSIX systems, it uses @iconv@ plus the
console\'s locale; on Windows it uses the console's current code page.

Characters or bytes which cannot be encoded/decoded (for example, not belonging
to the output range) will be ignored.
-}

module System.Console.Haskeline.Encoding (
                    encode,
                    encodeToString,
                    decode,
                    decodeFromString) where

import System.Console.Haskeline.InputT
import System.Console.Haskeline.Term
import System.Console.Haskeline.Monads
import Data.ByteString.Char8 (ByteString, pack, unpack)

-- | Encode a Unicode 'String' into a 'ByteString' suitable for the current
-- console.
encode :: MonadIO m => String -> InputT m ByteString
encode str = do
    encoder <- asks encodeForTerm
    liftIO $ encoder str

-- | Convert a 'ByteString' from the console's encoding into a Unicode 'String'.
decode :: MonadIO m => ByteString -> InputT m String
decode str = do
    decoder <- asks decodeForTerm
    liftIO $ decoder str

-- | Convert a 'String' from Unicode to the console's encoding.
encodeToString :: MonadIO m => String -> InputT m String
encodeToString = fmap unpack . encode

-- | Convert a 'String' from the console's encoding to unicode.
decodeFromString :: MonadIO m => String -> InputT m String
decodeFromString = decode . pack
