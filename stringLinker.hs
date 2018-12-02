{-# LANGUAGE TupleSections #-}

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import System.FilePath
import System.Process
import Distribution.System
import Data.Maybe
import Data.List
import Data.Char
import Data.Bool

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

sequencePair :: (IO a, IO b) -> IO (a, b)
sequencePair (xm, ym) = xm >>= ((<$> ym) . (,))

outputFormat :: Platform -> String
outputFormat (Platform arch os) = objF ++ bitSize
  where objF = case os of
                 Linux   -> "elf"
                 OSX     -> "macho"
                 Windows -> "win"
        bitSize = case arch of
                    I386   -> "32"
                    X86_64 -> "64"

readPlatform :: String -> Platform
readPlatform str = Platform (
    case arch of
      "32" -> I386
      "64" -> X86_64
  )(
    case os of
      "linux" -> Linux
      "osx"   -> OSX
      "macos" -> OSX
      "mac"   -> OSX
      "win"   -> Windows
  )
  where (os, arch) = splitAt (length str - 2) . map toLower $ str

transformIdent :: Platform -> String -> String
transformIdent (Platform _ Linux)   = id
transformIdent (Platform _ OSX)     = ("_" ++)
transformIdent (Platform _ Windows) = ("_" ++)

data MaybeOptions = MaybeOptions
  { mOptInput          :: [(String, IO String)]
  , mOptOutput         :: Maybe FilePath
  , mOptOutputF        :: Maybe String
  , mOptOutputPlatform :: Platform
  }

data Options = Options
  { optInput          :: [(String, IO String)]
  , optOutput         :: FilePath
  , optOutputF        :: String
  , optOutputPlatform :: Platform
  }

initialOptions :: MaybeOptions
initialOptions = MaybeOptions
  { mOptInput          = []
  , mOptOutput         = Nothing
  , mOptOutputF        = Nothing
  , mOptOutputPlatform = buildPlatform
  }

fillDefaultOptions :: MaybeOptions -> Options
fillDefaultOptions opts = Options
  { optInput          = mOptInput opts
  , optOutput         = fromMaybe (fst . head . mOptInput $ opts) (mOptOutput opts)
  , optOutputF        = fromMaybe (outputFormat . mOptOutputPlatform $ opts) (mOptOutput opts)
  , optOutputPlatform = mOptOutputPlatform opts
  }

inputOpt :: String -> MaybeOptions -> IO MaybeOptions
inputOpt arg opt = return opt { mOptInput = (takeBaseName arg, readFile arg) : mOptInput opt }

options :: [OptDescr (MaybeOptions -> IO MaybeOptions)]
options =
  [ Option
      "i"
      ["input"]
      (ReqArg
        inputOpt
        "FILE")
      "Input file"
  , Option
      "o"
      ["output"]
      (ReqArg
        (\arg opt -> return opt { mOptOutput = Just arg })
        "FILE")
      "Output file"
  , Option
      "f"
      ["format"]
      (ReqArg
        (\arg opt -> return opt { mOptOutputF = Just arg })
        "FORMAT")
      "Set output format"
  , Option
      "p"
      ["platform"]
      (ReqArg
        (\arg opt -> return opt { mOptOutputPlatform = readPlatform arg })
        "PLATFORM")
      "Set output platform"
  , Option
      "h"
      ["help"]
      (NoArg
        (\_ -> do
          prg <- getProgName
          hPutStrLn stderr (usageInfo prg options)
          exitWith ExitSuccess))
      "Show help"
  ]

getActions :: ([MaybeOptions -> IO MaybeOptions], [String], [String]) -> IO [MaybeOptions -> IO MaybeOptions]
getActions (actions, nonOptions, errors) = (sequence $ map errorWithoutStackTrace errors) >> return actions

assertInput :: MaybeOptions -> MaybeOptions
assertInput opts = bool opts (errorWithoutStackTrace "No input files given") . null . mOptInput $ opts

compileString :: (String, String) -> String
compileString (ident, str) = "global "
                          ++ ident
                          ++ "\n"
                          ++ ident
                          ++ " DB "
                          ++ (intercalate "," . map (show . fromEnum) $ str)
                          ++ ",0"

compile :: Platform -> [(String, String)] -> String
compile platform xs = intercalate "\n" . map ($ map (mapFst (transformIdent platform)) xs) $
  [ const "section .data"
  , intercalate "\n" . map (compileString $)
  ]

generateCDecl :: (String, String) -> String
generateCDecl (ident, str) = "extern const char "
                          ++ ident
                          ++ "[];"

generateHeader :: Platform -> [(String, String)] -> String
generateHeader _ xs = intercalate "\n" . map (generateCDecl $) $ xs

outputFiles :: [(String, Platform -> [(String, String)] -> String)]
outputFiles =
  [ ("asm", compile)
  , ("h", generateHeader)
  ]

chain :: IO a -> [a -> IO ()] -> IO ()
chain x = foldl (>>) (return ()) . map (x >>=)

generateFiles :: [(String, Platform -> [(String, String)] -> String)] -> Options -> IO ()
generateFiles files opts =
    chain (sequence . map (sequencePair . mapFst return) . optInput $ opts)
  . map
    (\(ext, gen) ->
        writeFile (optOutput opts ++ "." ++ ext)
      . gen (optOutputPlatform opts)
    )
  $ files

passthrough :: (a -> IO ()) -> a -> IO a
passthrough f x = f x >> return x

main = getArgs                                          -- Get the arguments
   >>= return . getOpt (ReturnInOrder inputOpt) options -- Parse options to get a list of actions and errors
   >>= getActions                                       -- Throw the errors to get the actions
   >>= foldl (>>=) (return initialOptions)              -- Here we thread defaultOptions through all supplied option actions
   >>= return . assertInput                             -- Verify that input is non-empty
   >>= return . fillDefaultOptions
   >>= passthrough (generateFiles outputFiles)
   >>= (\opts ->
         callProcess "nasm" ["-f", optOutputF opts, optOutput opts ++ ".asm"]
       )

