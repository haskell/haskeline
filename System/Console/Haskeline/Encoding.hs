{- |

This module exposes the console Unicode API which is used by the functions
in "System.Console.Haskeline".  On POSIX systems, it uses @iconv@ plus the
console\'s locale; on Windows it uses the console's current code page.

Characters or bytes which cannot be encoded/decoded (for example, not belonging
to the output range) will be ignored.
-}

module System.Console.Haskeline.Encoding (
                                    encode,
                                    decode,
                                    getEncoder,
                                    getDecoder
                                    -- $convFncs
                                    ) where

import System.Console.Haskeline.InputT
import System.Console.Haskeline.Term
import System.Console.Haskeline.Monads
import Data.ByteString (ByteString)

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

getEncoder :: Monad m => InputT m (String -> IO ByteString)
getEncoder = asks encodeForTerm

getDecoder :: Monad m => InputT m (ByteString -> IO String)
getDecoder = asks decodeForTerm
