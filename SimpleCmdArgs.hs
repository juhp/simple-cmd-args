module SimpleCmdArgs
  (optionMods,
   optionalMods,
   simpleCmdArgs,
   simpleCmdArgs',
   strArg,
   Subcommand(..),
   subcommands,
   switchMods
  )
where

import Control.Monad (join)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Data.Monoid (mconcat)
#endif
import Data.Semigroup ((<>))
import Data.Version
import Options.Applicative

-- | Parser executor (allows interspersed args and options)
simpleCmdArgs ::
  Maybe Version
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser (IO ())
  -- ^ commands
  -> IO ()
simpleCmdArgs mversion h pd =
  simpleCmdArgsWithMods mods mversion
  where
    mods = fullDesc <> header h <> progDesc pd

-- | Parser executor without interspersing options and args
simpleCmdArgs'
  :: Maybe Version
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Parser (IO ())
  -- ^ commands
  -> IO ()
simpleCmdArgs' mversion h pd =
  simpleCmdArgsWithMods mods mversion
  where
    mods = fullDesc <> header h <> progDesc pd <> noIntersperse

-- | Generic parser executor with explicit info modifiers
simpleCmdArgsWithMods ::
  InfoMod (IO ()) ->
  -- ^ modifiers
  Maybe Version
  -- ^ version string
  -> Parser (IO ())
  -- ^ commands
  -> IO ()
simpleCmdArgsWithMods mods mversion cmdsParser = join $
  customExecParser (prefs showHelpOnEmpty)
  (case mversion of
    (Just version) -> info (helper <*> versionOption version <*> cmdsParser) mods
    Nothing -> info (helper <*> cmdsParser) mods)
  where 
    versionOption ver =
      infoOption (showVersion ver) (long "version" <> help "Show version")

data Subcommand = Subcommand String String (Parser (IO ()))

subcommands :: [Subcommand] -> Parser (IO ())
subcommands = subparser . mconcat . map cmdToParse
  where
    cmdToParse (Subcommand name cmddesc cmdparse) =
      command name (info cmdparse (progDesc cmddesc))

strArg :: String -> Parser String
strArg var = strArgument (metavar var)

switchMods :: HasName f =>
  Char -> String -> String -> Mod f a
switchMods s l h =
  short s <> long l <> help h

optionMods :: (HasMetavar f, HasName f) =>
  Char -> String -> String -> String -> Mod f a
optionMods s l meta h =
  short s <> long l <> metavar meta <> help h

optionalMods :: (HasMetavar f, HasName f, HasValue f) =>
  Char -> String -> String -> String -> a -> Mod f a
optionalMods s l meta h d =
  short s <> long l <> metavar meta <> help h <> value d
