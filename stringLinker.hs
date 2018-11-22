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

nativeOutputFormat :: String
nativeOutputFormat = objF ++ bitSize
  where objF = case buildOS of
                 Linux   -> "elf"
                 OSX     -> "macho"
                 Windows -> "win"
        bitSize = case buildArch of
                    I386   -> "32"
                    X86_64 -> "64"

register :: Arch -> String
register I386   = "eax"
register X86_64 = "rax"

transformIdent :: String -> String
transformIdent = case buildOS of
                   Linux   -> id
                   OSX     -> ("_" ++)
                   Windows -> ("_" ++)

data Options = Options
  { optInput      :: [IO (String, String)]
  , optOutput     :: Maybe FilePath
  , optOutputF    :: String
  , optOutputArch :: Arch
  }

defaultOptions :: Options
defaultOptions = Options
  { optInput      = []
  , optOutput     = Nothing
  , optOutputF    = nativeOutputFormat
  , optOutputArch = buildArch
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
      "f"
      ["format"]
      (ReqArg
        (\arg opt -> return opt {
            optOutputF = arg,
            optOutputArch = case reverse . take 2 . reverse $ arg of
                              "32" -> I386
                              "64" -> X86_64
                              _    -> optOutputArch opt
          })
        "FORMAT")
      "Set output format"
  , Option
      "a"
      ["arch", "architecture"]
      (ReqArg
        (\arg opt -> return opt {
            optOutputArch = case map toLower arg of
                              "32"     -> I386
                              "i386"   -> I386
                              "x86"    -> I386
                              "64"     -> X86_64
                              "x64"    -> X86_64
                              "x86_64" -> X86_64
          })
        "ARCH")
      "Set output architecture"
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

getActions :: ([Options -> IO Options], [String], [String]) -> IO [Options -> IO Options]
getActions (actions, nonOptions, errors) = (sequence $ map errorWithoutStackTrace errors) >> return actions

assertInput :: Options -> Options
assertInput opts = bool opts (errorWithoutStackTrace "No input files given") . null . optInput $ opts

compileFunction :: Arch -> (String, String) -> String
compileFunction arch (ident, str) = "global "
                                   ++ ident
                                   ++ "\n"
                                   ++ ident
                                   ++ ":\nlea "
                                   ++ register arch
                                   ++ ",[rel _"
                                   ++ ident
                                   ++ "]\nret"

compileString :: (String, String) -> String
compileString (ident, str) = "global _"
                          ++ ident
                          ++ "\n_"
                          ++ ident
                          ++ " DB "
                          ++ (intercalate "," . map (show . fromEnum) $ str)
                          ++ ",0"

compile :: Arch -> [(String, String)] -> String
compile arch xs = intercalate "\n" . map ($ map (mapFst transformIdent) xs) $
  [ const "section .text"
  , intercalate "\n" . map (compileFunction arch $)
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

chain :: IO a -> [a -> IO ()] -> IO ()
chain x = foldl (>>) (return ()) . map (x >>=)

main = getArgs                                          -- Get the arguments
   >>= return . getOpt (ReturnInOrder inputOpt) options -- Parse options to get a list of actions and errors
   >>= getActions                                       -- Throw the errors to get the actions
   >>= foldl (>>=) (return defaultOptions)              -- Here we thread defaultOptions through all supplied option actions
   >>= return . assertInput                             -- Verify that input is non-empty
   >>= (\(Options input (Just output) outputF outputArch) ->
          chain (sequence input)
            [ writeFile (output ++ ".asm") . compile outputArch
            , writeFile (output ++ ".h") . generateHeader
            ]
       >> callProcess "nasm" ["-f", outputF, output ++ ".asm"]
       )

