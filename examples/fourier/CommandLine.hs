{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module CommandLine (parseCommandLine) where

import Control.Monad (unless)
import System.Console.GetOpt
import System.Environment (getProgName, getArgs)
import System.Exit

data Options = Options {
  optSQDB :: FilePath
}

defaultOptions :: Options
defaultOptions = Options {
  optSQDB = "."
}

parseCommandLine :: IO FilePath
parseCommandLine = do
    progName <- getProgName
    let header = "Usage: " ++ progName ++ " [option...]"
    args <- getArgs
    let (o, nonopts, errs) = getOpt Permute options args
    unless (null errs) $ do
      mapM_ putStr errs
      putStrLn ""
      putStr $ usageInfo header options
      exitFailure
    unless (null nonopts) $ do
      putStrLn $ "unrecognized argument `" ++ head nonopts ++ "\'"
      putStrLn ""
      putStr $ usageInfo header options
      exitFailure
    let opts = foldl (flip id) defaultOptions o
    return $ optSQDB opts
  where
    options = [
        Option ['d'] ["database"] (ReqArg (\arg opts -> opts { optSQDB = arg }) "DIR") "location of .qc database (default: .)"
      ]
