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

data Options = Options
  { optInput  :: [IO (String, String)]
  , optOutput :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { optInput  = []
  , optOutput = Nothing
  }

inputOpt :: String -> Options -> IO Options
inputOpt arg opt = return opt
  { optInput  = ((takeBaseName arg,) <$> readFile arg) : optInput opt
  , optOutput = Just (fromMaybe (takeBaseName arg) (optOutput opt))
  }

options :: [OptDescr (Options -> IO Options)]
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
        (\arg opt -> return opt { optOutput = Just arg })
        "FILE")
      "Output file"
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

compileFunction :: (String, String) -> String
compileFunction (ident, str) = "global _"
                            ++ ident
                            ++ "\n_"
                            ++ ident
                            ++ ":\nlea rax,[rel __"
                            ++ ident
                            ++ "]\nret"

compileString :: (String, String) -> String
compileString (ident, str) = "global __"
                          ++ ident
                          ++ "\n__"
                          ++ ident
                          ++ " DB "
                          ++ (intercalate "," . map (show . fromEnum) $ str)
                          ++ ",0"

compile :: [(String, String)] -> String
compile xs = intercalate "\n" . map ($ xs) $
  [ const "section .text"
  , intercalate "\n" . map (compileFunction $)
  , const "section .data"
  , intercalate "\n" . map (compileString $)
  ]

generateCDecl :: (String, String) -> String
generateCDecl (ident, str) = "const char* "
                          ++ ident
                          ++ "();"

generateHeader :: [(String, String)] -> String
generateHeader xs = intercalate "\n" . map ($ xs) $
  [ const "#ifdef __cplusplus\nextern \"C\" {\n#endif"
  , intercalate "\n" . map (generateCDecl $)
  , const "#ifdef __cplusplus\n}\n#endif"
  ]

asmOutputFormat :: String
asmOutputFormat = objF ++ bitSize
  where objF = case buildOS of
                 Linux   -> "elf"
                 OSX     -> "macho"
                 Windows -> "win"
        bitSize = case buildArch of
                    I386   -> "32"
                    X86_64 -> "64"

main = do
  args <- getArgs

  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt (ReturnInOrder inputOpt) options args

  -- Here we thread defaultOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions

  input <- sequence . optInput $ opts

  if null input then errorWithoutStackTrace "No input files given" else return ()

  let output = fromJust . optOutput $ opts

  writeFile (output ++ ".asm") . compile $ input
  writeFile (output ++ ".h") . generateHeader $ input

  callProcess "nasm" ["-f", asmOutputFormat, output ++ ".asm"]

