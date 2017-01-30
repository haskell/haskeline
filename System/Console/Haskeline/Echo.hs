{-# LANGUAGE CPP #-}

module System.Console.Haskeline.Echo (withoutInputEcho) where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import System.Console.Haskeline.MonadException (MonadException, bracket)
import System.Exit (ExitCode(..))
import System.IO (Handle, hGetContents, hGetEcho, hSetEcho)
import System.Process (StdStream(..), createProcess, shell,
                       std_in, std_out, waitForProcess)

#if defined(MINGW)
import Graphics.Win32.Misc (getStdHandle, sTD_INPUT_HANDLE)

# if MIN_VERSION_Win32(2,5,0)
import System.Win32.MinTTY (isMinTTYHandle)
# else
import System.Console.Haskeline.Backend.Win32.MinTTY (isMinTTYHandle)
# endif
import System.IO.Unsafe (unsafePerformIO)
#endif

-- | Return the handle's current input 'EchoState'.
getInputEchoState :: Handle -> IO EchoState
getInputEchoState input = if minTTY
                             then fmap MinTTY (getInputEchoSTTY input)
                             else fmap DefaultTTY $ hGetEcho input

-- | Return all of @stty@'s current settings in a non-human-readable format.
--
-- This function is not very useful on its own. Its greater purpose is to
-- provide a compact 'STTYSettings' that can be fed back into
-- 'setInputEchoState'.
getInputEchoSTTY :: Handle -> IO STTYSettings
getInputEchoSTTY input = sttyRaw input "-g"

-- | Set the handle's input 'EchoState'.
setInputEchoState :: Handle -> EchoState -> IO ()
setInputEchoState input (MinTTY settings) = setInputEchoSTTY input settings
setInputEchoState input (DefaultTTY echo) = hSetEcho input echo

-- | Create an @stty@ process and wait for it to complete. This is useful for
-- changing @stty@'s settings, after which @stty@ does not output anything.
--
-- @
-- setInputEchoSTTY input = 'void' . 'sttyRaw' input
-- @
setInputEchoSTTY :: Handle -> STTYSettings -> IO ()
setInputEchoSTTY input = void . sttyRaw input

-- | Save the handle's current input 'EchoState', perform a computation,
-- restore the saved 'EchoState', and then return the result of the
-- computation.
--
-- @
-- bracketInputEcho input action =
--  'bracket' ('liftIO' $ 'getInputEchoState' input)
--            ('liftIO' . 'setInputEchoState' input)
--            (const action)
-- @
bracketInputEcho :: MonadException m => Handle -> m a -> m a
bracketInputEcho input action =
  bracket (liftIO $ getInputEchoState input)
          (liftIO . setInputEchoState input)
          (const action)

-- | Perform a computation with the handle's input echoing disabled. Before
-- running the computation, the handle's input 'EchoState' is saved, and the
-- saved 'EchoState' is restored after the computation finishes.
--
-- @
-- withoutInputEcho input action =
--   'bracketInputEcho' input ('liftIO' ('setInputEchoState' input 'echoOff') >> action)
-- @
withoutInputEcho :: MonadException m => Handle -> m a -> m a
withoutInputEcho input action =
  bracketInputEcho input (liftIO (setInputEchoState input echoOff) >> action)

-- | Create an @stty@ process, wait for it to complete, and return its output.
sttyRaw :: Handle -> String -> IO STTYSettings
sttyRaw input arg = do
  let stty = (shell $ "stty " ++ arg) {
        std_in  = UseHandle input
      , std_out = CreatePipe
      }
  (_, mbStdout, _, rStty) <- createProcess stty
  exStty <- waitForProcess rStty
  case exStty of
    e@ExitFailure{} -> throw e
    ExitSuccess     -> maybe (return "") hGetContents mbStdout

-- | A representation of the terminal input's current echoing state. Example
-- values include 'echoOff' and 'echoOn'.
data EchoState
  = MinTTY STTYSettings
    -- ^ The argument to (or value returned from) an invocation of the @stty@
    -- command-line utility. Most POSIX-like shells have @stty@, including
    -- MinTTY on Windows. Since neither 'hGetEcho' nor 'hSetEcho' work on
    -- MinTTY, when 'getInputEchoState' runs on MinTTY, it returns a value
    -- built with this constructor.
    --
    -- However, native Windows consoles like @cmd.exe@ or PowerShell do not
    -- have @stty@, so if you construct an 'EchoState' with this constructor
    -- manually, take care not to use it with a native Windows console.
  | DefaultTTY Bool
    -- ^ A simple on ('True') or off ('False') toggle. This is returned by
    -- 'hGetEcho' and given as an argument to 'hSetEcho', which work on most
    -- consoles, with the notable exception of MinTTY on Windows. If you
    -- construct an 'EchoState' with this constructor manually, take care not
    -- to use it with MinTTY.
  deriving (Eq, Ord, Show)

-- | Indicates that the terminal's input echoing is (or should be) off.
echoOff :: EchoState
echoOff = if minTTY then MinTTY "-echo" else DefaultTTY False

-- | Settings used to configure the @stty@ command-line utility.
type STTYSettings = String

-- | Is the current process attached to a MinTTY console (e.g., Cygwin or MSYS)?
minTTY :: Bool
#if defined(MINGW)
minTTY = unsafePerformIO $ do
  h <- getStdHandle sTD_INPUT_HANDLE
  isMinTTYHandle h
{-# NOINLINE minTTY #-}
#else
minTTY = False
#endif
