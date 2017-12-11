# Haskeline

The Haskeline library provides a user interface for line input in command-line programs.
This library is similar in purpose to readline, but since it is written in Haskell it is (hopefully)
more easily used in other Haskell programs.

## Links
The latest release, as well as the API documentation, may be obtained from [Hackage](http://hackage.haskell.org/package/haskeline).

The most recent development source code can be downloaded with:

    git clone https://github.com/judah/haskeline

Further documentation is also available at
[https://github.com/judah/haskeline/wiki](https://github.com/judah/haskeline/wiki)


## Features:

 - Provides a [rich line editing interface](https://github.com/judah/haskeline/wiki/KeyBindings).
 - A `~/.haskeline` file allows customization of [preferences](https://github.com/judah/haskeline/wiki/UserPreferences) and [custom key bindings](https://github.com/judah/haskeline/wiki/CustomKeyBindings).
 - Runs on POSIX-compatible systems, using the [terminfo](http://github.com/judah/terminfo) library to support non-ANSI terminals.
 - Runs on Windows using MinGW.
 - [Supports Unicode](https://github.com/judah/haskeline/wiki/UnicodeSupport) cross-platform.
 - History recall and incremental search.
 - Custom tab completion functions which may run in an arbitrary monad.
