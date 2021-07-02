{-|
This library provides a thin layer on optparse-applicative
argument and option parsing, using @Parser (IO ())@,
applying commands directly to their argument parsing.

A few option Mod functions are also provided.
-}

module SimpleCmdArgs
  (simpleCmdArgs,
   simpleCmdArgs',
   simpleCmdArgsWithMods,
   -- * Subcommands
   Subcommand(..),
   subcommands,
   -- * Option and arg helpers
   strArg,
   switchWith,
   flagWith,
   flagWith',
   switchMods,
   strOptionWith,
   optionWith,
   optionMods,
   strOptionalWith,
   optionalWith,
   optionalMods,
   argumentWith,
   -- * Re-exports from optparse-applicative
   Parser,
   ReadM,
   auto,
   many,
   eitherReader,
   maybeReader,
   optional,
   some,
   str,
   (<|>),
#if !MIN_VERSION_base(4,8,0)
   (<$>), (<*>)
#endif
  )
where

#if !MIN_VERSION_base(4,13,0)
import Control.Applicative ((<|>),
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>)
#endif
                           )
#endif
import Control.Monad (join)
import Data.List (nub)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif
import Data.String
import Data.Version
import Debug.Trace (trace)
import Options.Applicative

-- | Parser executor (allows interspersed args and options)
--
-- > simpleCmdArgs (Just version) "summary" "program description" $ myCommand <$> myOptParser <*> myargsParser
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
  simpleCmdArgsWithMods mversion mods
  where
    mods = fullDesc <> header h <> progDesc pd

-- | Parser executor without interspersing options and args
--
-- > simpleCmdArgs' Nothing "summary" "program description" $ myCommand <$> myOptParser <*> myargsParser
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
  simpleCmdArgsWithMods mversion mods
  where
    mods = fullDesc <> header h <> progDesc pd <> noIntersperse

-- | Generic parser executor with explicit info modifiers
--
-- @since 0.1.1
simpleCmdArgsWithMods ::
  Maybe Version -- ^ version string
  -> InfoMod (IO ()) -- ^ modifiers
  -> Parser (IO ()) -- ^ commands
  -> IO ()
simpleCmdArgsWithMods mversion mods cmdsParser = join $
  customExecParser (prefs showHelpOnEmpty)
  (case mversion of
    (Just version) -> info (helper <*> versionOption version <*> cmdsParser) mods
    Nothing -> info (helper <*> cmdsParser) mods)
  where
    versionOption ver =
      infoOption (showVersion ver) (long "version" <> help "Show version")

-- | > Subcommand "command" "help description text" $ myCommand <$> optParser
data Subcommand =
  Subcommand String String (Parser (IO ()))

subCmdName :: Subcommand -> String
subCmdName (Subcommand n _ _) = n

-- | equality by command name
--
-- @since 0.1.5
instance Eq Subcommand where
  c1 == c2 = subCmdName c1 == subCmdName c2

-- | comparison by command name
--
-- @since 0.1.5
instance Ord Subcommand where
  compare c1 c2 = compare (subCmdName c1) (subCmdName c2)

-- | Create a list of @Subcommand@ that can be run by @simpleCmdArgs@
subcommands :: [Subcommand] -> Parser (IO ())
subcommands = hsubparser . mconcat . map cmdToParse . warnIfDuplicates
  where
    cmdToParse (Subcommand name cmddesc cmdparse) =
      command name (info cmdparse (progDesc cmddesc))

    warnIfDuplicates :: [Subcommand] -> [Subcommand]
    warnIfDuplicates subcmds =
      if dups then trace "duplicate subcommand found" subcmds else subcmds
      where
        dups = nub subcmds /= subcmds

-- | A string arg parser with a METAVAR for help
strArg :: IsString s => String -> Parser s
strArg var = strArgument (metavar var)

-- | switch with Mods
--
-- > switchWith 'o' "option" "help description"
--
-- @since 0.1.1
switchWith :: Char -> String -> String -> Parser Bool
switchWith s l h =
  switch (switchMods s l h)

-- | flag with Mods
--
-- > flagWith offVal onVal 'f' "flag" "help description"
--
-- @since 0.1.2
flagWith :: a -> a -> Char -> String -> String -> Parser a
flagWith off on s l h =
  flag off on (switchMods s l h)

-- | flag' with Mods
--
-- > flagWith' val 'f' "flag" "help description"
--
-- @since 0.1.2
flagWith' :: a -> Char -> String -> String -> Parser a
flagWith' val s l h =
  flag' val (switchMods s l h)

-- | @Mod@s for a switch.
--
-- > switchMods 'o' "option" "help description"
switchMods :: HasName f =>
  Char -> String -> String -> Mod f a
switchMods s l h =
  short s <> long l <> help h

-- | strOption with Mods
--
-- > strOptionWith 'o' "option" "METAVAR" "help description"
--
-- @since 0.1.1
strOptionWith :: IsString s => Char -> String -> String -> String -> Parser s
strOptionWith s l meta h =
  strOption (optionMods s l meta h)

-- | option with Mods
--
-- > optionWith auto 'o' "option" "METAVAR" "help description"
--
-- @since 0.1.1
optionWith :: ReadM a -> Char -> String -> String -> String -> Parser a
optionWith r s l meta h =
  option r (optionMods s l meta h)

-- | @Mod@s for a mandatory option.
--
-- > optionMods 'o' "option" "METAVAR" "help description"
optionMods :: (HasMetavar f, HasName f) =>
  Char -> String -> String -> String -> Mod f a
optionMods s l meta h =
  short s <> long l <> metavar meta <> help h

-- | strOptional with Mods
--
-- > strOptionalWith 'o' "option" "METAVAR" "help description" default
--
-- @since 0.1.1
strOptionalWith :: (IsString s, Show s)
                => Char -> String -> String -> String -> s -> Parser s
strOptionalWith s l meta h d =
  strOption (optionalMods s l meta (h <> " [default: " <> show d <> "]") d)

-- | optional option with Mods, includes a default value.
--
-- > optionalWith auto 'o' "option" "METAVAR" "help description" default
--
-- @since 0.1.1
optionalWith :: ReadM a -> Char -> String -> String -> String -> a -> Parser a
optionalWith r s l meta h d =
  option r (optionalMods s l meta h d)

-- | @Mod@s for an optional option: includes a default value.
--
-- > optionalMods 'o' "option" "METAVAR" "help description" default
optionalMods :: (HasMetavar f, HasName f, HasValue f) =>
  Char -> String -> String -> String -> a -> Mod f a
optionalMods s l meta h d =
  short s <> long l <> metavar meta <> help h <> value d

-- | argument with METAVAR
--
-- > argumentWith auto "METAVAR"
--
-- @since 0.1.1
argumentWith :: ReadM a -> String -> Parser a
argumentWith r meta =
  argument r (metavar meta)
