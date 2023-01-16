module Main (main) where

import Prelude

import ArgParse.Basic as ArgParse
import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Node.FS.Stats as FS.Stats
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import PureScript.CST as CST
import PureScript.CST.Errors as CST.Errors
import PureScript.CST.Types as CST.Types

type Package a =
  { packageType :: PackageType
  , datum :: a
  }

type Dependency = String

type Module = String

data PackageType = Project | Dependency Dependency

derive instance Eq PackageType

type Args =
  { packages :: List (Package FilePath)
  , declaredDeps :: List Dependency
  }

parseArgs :: forall m. MonadEffect m => m Args
parseArgs = liftEffect do
  argv <- Array.drop 2 <$> Process.argv
  case
    ArgParse.parseArgs
      "exact-deps"
      "Find unused and undeclared dependencies in purs-nix."
      argsParser
      argv
    of
    Right args -> pure args
    Left e -> do
      Console.error $ ArgParse.printArgError e
      die
  where
  argsParser =
    ArgParse.flagHelp *> ArgParse.fromRecord
      { packages: ArgParse.many ado
          packageType <- ArgParse.choose "pkg-type"
            [ Project <$ ArgParse.flag [ "--project" ] "The project."
            , Dependency <$> ArgParse.argument [ "--dep" ] "Dependency name."
            ]
          dir <- ArgParse.argument
            [ "--dir" ]
            "Directory containing Purescript source files."
          in { packageType, datum: dir }
      , declaredDeps: ArgParse.many $
          ArgParse.argument [ "--declared" ] "Declared dependency."
      }

die :: forall m a. MonadEffect m => m a
die = liftEffect $ Process.exit 1

main :: Effect Unit
main = launchAff_ do
  args <- parseArgs
  packages <- parseModules args.packages
  let
    declaredDeps :: HashSet Dependency
    declaredDeps = HashSet.fromFoldable args.declaredDeps

    packageByModule :: HashMap Module PackageType
    packageByModule = HashMap.fromFoldable do
      { packageType, datum: modules } <- packages
      CST.Types.ModuleHeader header <- List.fromFoldable modules
      let moduleName = unwrap (unwrap header.name).name
      pure $ Tuple moduleName packageType

    projectModuleDependencies :: HashSet Module
    projectModuleDependencies = HashSet.fromFoldable do
      { packageType, datum: modules } <- packages
      guard $ packageType == Project
      CST.Types.ModuleHeader header <- List.fromFoldable modules
      List.fromFoldable header.imports
        <#> unwrap >>> _.module >>> unwrap >>> _.name >>> unwrap

    exactDeps :: HashSet Dependency
    exactDeps = HashSet.fromFoldable do
      mod <- List.fromFoldable projectModuleDependencies
      case HashMap.lookup mod packageByModule of
        Just (Dependency dep) -> pure dep
        _ -> mempty

    unusedDeps =
      HashSet.toArray $ HashSet.difference declaredDeps exactDeps
    undeclaredDeps =
      HashSet.toArray $ HashSet.difference exactDeps declaredDeps

  Console.log $ "Unused: " <> show unusedDeps
  Console.log $ "Undeclared: " <> show undeclaredDeps

  unless (null unusedDeps && null undeclaredDeps) die

parseModules :: List (Package FilePath) -> Aff (List (Package (Array (CST.Types.ModuleHeader Void))))
parseModules = traverse \pkg -> do
  files <- findPursFiles pkg.datum
  modules <- traverse parseModule files
  pure pkg { datum = modules }
  where
  findPursFiles :: FilePath -> Aff (Array FilePath)
  findPursFiles dir =
    do
      contents <- FS.readdir dir
      let inDir path = Path.concat [ dir, path ]
      Array.concat <$> for (inDir <$> contents) \path -> do
        stats <- FS.stat path
        case unit of
          _ | FS.Stats.isDirectory stats ->
            findPursFiles path
          _ | FS.Stats.isFile stats, Path.extname path == ".purs" ->
            pure (pure path)
          _ -> mempty

  parseModule :: FilePath -> Aff (CST.Types.ModuleHeader Void)
  parseModule path =
    FS.readTextFile Encoding.UTF8 path
      >>= CST.parsePartialModule >>> case _ of
        CST.ParseSucceeded (CST.PartialModule mod) -> pure mod.header
        CST.ParseSucceededWithErrors _ errors -> bail $ NEA.head errors
        CST.ParseFailed error -> bail error
    where
    bail error = do
      Console.error $ "Failed to parse " <> path
      Console.error $ CST.Errors.printParseError error.error
      die
