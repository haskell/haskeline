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

{- $convFncs

The functions returned from 'getEncoder' and 'getDecoder' should be run within an
'InputT' action in order to use the console's locale.

For example, the following might not use the correct encoding:

> encodeStrings :: String -> String -> IO (ByteString,ByteString)
> encodeStrings str1 str2 = do
>    encoder <- runInputT defaultSettings getEncoder
>    estr1 <- encoder str1
>    estr2 <- encoder str2
>    return (estr1, estr2)

Instead, that function could be written as:

> encodeStrings str1 str2 = runInputT defaultSettings $ do
>    encoder <- getEncoder
>    estr1 <- liftIO $ encoder str1
>    estr2 <- liftIO $ encoder str2
>    return (estr1, estr2)

Better still, of course, would be:

> encodeStrings str1 str2 = runInputT defaultSettings $ do
>    estr1 <- encode str1
>    estr2 <- encode str2
>    return (estr1, estr2)

But it may not always be feasible to use 'encode' or 'decode' directly.
(For example, some event handler APIs require callbacks to be IO actions.)

-}

getEncoder :: Monad m => InputT m (String -> IO ByteString)
getEncoder = asks encodeForTerm

getDecoder :: Monad m => InputT m (ByteString -> IO String)
getDecoder = asks decodeForTerm
