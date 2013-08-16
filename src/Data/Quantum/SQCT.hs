{-
 - Hacq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Hacq.
 - Hacq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

module Data.Quantum.SQCT (
    SQGate(..), SQSolution(..), insertSQSolution, chooseSolution, filterSolutionDatabase,
    parseSQCT, readSQCTDirectory) where

import Control.Monad (MonadPlus, mzero)
import Control.Monad.State (StateT(StateT), evalStateT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Char as Char
import Data.Hashable(Hashable(hashWithSalt))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ratio
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- Data structure

data SQGate = SQGateId | SQGateX | SQGateY | SQGateZ | SQGateH | SQGateS !Bool | SQGateT !Bool
  deriving Eq

instance Hashable SQGate where
  s `hashWithSalt` SQGateId = s `hashWithSalt` (0 :: Int)
  s `hashWithSalt` SQGateX = s `hashWithSalt` (1 :: Int)
  s `hashWithSalt` SQGateY = s `hashWithSalt` (2 :: Int)
  s `hashWithSalt` SQGateZ = s `hashWithSalt` (3 :: Int)
  s `hashWithSalt` SQGateH = s `hashWithSalt` (4 :: Int)
  s `hashWithSalt` SQGateS False = s `hashWithSalt` (5 :: Int)
  s `hashWithSalt` SQGateS True = s `hashWithSalt` (6 :: Int)
  s `hashWithSalt` SQGateT False = s `hashWithSalt` (7 :: Int)
  s `hashWithSalt` SQGateT True = s `hashWithSalt` (8 :: Int)

data SQSolution = SQSolution {
  solutionLabel :: String,
  solutionDistance :: Double, -- |Distance in the diamond norm.
  solutionGateList :: [SQGate],
  solutionTCount :: Int
}

sqSolution :: String -> Double -> [SQGate] -> SQSolution
sqSolution label distance gs =
    SQSolution label distance gs (length $ filter isGateT gs)
  where
    isGateT (SQGateT _) = True
    isGateT _ = False

insertSQSolution :: SQSolution -> [SQSolution] -> [SQSolution]
insertSQSolution sol (sol' : sols) | solutionTCount sol >= solutionTCount sol' =
    sol' : insertSQSolution sol sols
insertSQSolution sol sols =
    sol : sols

chooseSolution :: [SQSolution] -> Double -> Maybe SQSolution
chooseSolution sols eps =
    listToMaybe $ dropWhile ((> eps) . solutionDistance) sols

filterSolutionDatabase :: Double -> HashMap Rational [SQSolution] -> HashMap Rational SQSolution
filterSolutionDatabase eps db =
    HashMap.fromList $ mapMaybe (uncurry choose) $ HashMap.toList db
  where
    choose fraction sols =
        case chooseSolution sols eps of
          Nothing -> Nothing
          Just sol -> Just (fraction, sol)

-- Parser

data Line = LOther | LDistance !Double | LBegin (Either ByteString Rational) | LEnd | LGate !SQGate
  deriving Eq

table :: HashMap ByteString (ByteString -> Line)
table = HashMap.fromList [
    (ByteString.pack "BEGIN", parseBegin),
    (ByteString.pack "END", const LEnd),
    (ByteString.pack "Id", const $ LGate SQGateId),
    (ByteString.pack "X", const $ LGate SQGateX),
    (ByteString.pack "Y", const $ LGate SQGateY),
    (ByteString.pack "Z", const $ LGate SQGateZ),
    (ByteString.pack "H", const $ LGate SQGateH),
    (ByteString.pack "P", const $ LGate $ SQGateS False),
    (ByteString.pack "P*", const $ LGate $ SQGateS True),
    (ByteString.pack "T", const $ LGate $ SQGateT False),
    (ByteString.pack "T*", const $ LGate $ SQGateT True),
    (ByteString.pack "#", parseComment)
  ]

checkPrefix :: MonadPlus m => ByteString -> StateT ByteString m ()
checkPrefix p = StateT $ \s ->
    let (hd, tl) = ByteString.splitAt (ByteString.length p) s in
    if hd == p then
      return ((), tl)
    else
      mzero

parseSubcircuitName :: ByteString -> Maybe Rational
parseSubcircuitName = evalStateT $ do
    checkPrefix $ ByteString.pack "RZ"
    num <- StateT ByteString.readInteger
    checkPrefix $ ByteString.pack "d"
    den <- StateT ByteString.readInteger
    return (num % den)

parseBegin :: ByteString -> Line
parseBegin circuitName =
    LBegin $ maybe (Left circuitName) Right $ parseSubcircuitName circuitName

parseComment :: ByteString -> Line
parseComment l =
    if ByteString.isPrefixOf kw l then
      LDistance $ read $ ByteString.unpack $ ByteString.drop (ByteString.length kw) l
    else
      LOther
  where
    kw = ByteString.pack "Distance between unitary and approximation:"

parseLine :: ByteString -> Line
parseLine l =
    case HashMap.lookup w0 table of
      Nothing -> LOther
      Just f -> f $ ByteString.dropWhile Char.isSpace rest
  where
    (w0, rest) = ByteString.break Char.isSpace $ ByteString.dropWhile Char.isSpace l

findDistance :: StateT [(Line, ByteString)] (Either String) Double
findDistance = StateT f
  where
    f [] = Left "No distance line"
    f ((LDistance fowlerDistance, _) : ls) = Right (fowlerDistance, ls)
    f (_ : ls) = f ls

findBegin :: StateT [(Line, ByteString)] (Either String) Rational
findBegin = StateT f
  where
    f [] = Left "No BEGIN line"
    f ((LBegin (Right factor), _) : ls) = Right (factor, ls)
    f ((LBegin (Left circuitName), _) : _) = Left $ "Unrecognized circuit name: " ++ ByteString.unpack circuitName
    f (_ : ls) = f ls

parseGateLine :: StateT [(Line, ByteString)] (Either String) (Maybe SQGate)
parseGateLine = StateT f
  where
    f [] = Left "No END line"
    f ((LEnd, _) : rest) = Right (Nothing, rest)
    f ((LGate g, _) : rest) = Right (Just g, rest)
    f ((_, l) : _) = Left $ "Unrecognized line: " ++ ByteString.unpack l

unfoldrM :: Monad m => m (Maybe a) -> m [a]
unfoldrM m = do
    mx <- m
    case mx of
      Nothing ->
        return []
      Just x -> do
        xs <- unfoldrM m
        return (x : xs)

-- Returns (r, d, l)
-- where l is the list of gates in the order appearing in the file,
-- the goal unitary is diag[1, e^{2πri}],
-- and the diamond distance is d.
parseSQCT :: String -> ByteString -> Either String (Rational, SQSolution)
parseSQCT label file = flip evalStateT (map (\l -> (parseLine l, l)) $ ByteString.lines file) $ do
    fowlerDistance <- findDistance
    let diamondDistance = 2 * fowlerDistance * sqrt (2 - fowlerDistance * fowlerDistance)
    factor <- findBegin
    gateList <- unfoldrM parseGateLine
    return (factor, sqSolution label diamondDistance gateList)

readSQCTDirectory :: FilePath -> IO (HashMap Rational [SQSolution])
readSQCTDirectory topDir = do
    isDirectory <- doesDirectoryExist topDir
    if isDirectory then
      go HashMap.empty [topDir]
    else
      error $ topDir ++ ": Directory does not exist"
  where
    go db [] =
        return db
    go db (path : paths) = do
        isDirectory <- doesDirectoryExist path
        if isDirectory then do
          files <- getDirectoryContents path
          let nontrivialPaths = map (path </>) $ filter (`notElem` [".", ".."]) files
          go db (nontrivialPaths ++ paths)
        else if List.isSuffixOf ".qc" path then do
          content <- ByteString.readFile path
          case parseSQCT path content of
            Left err -> do
              hPutStrLn stderr $ path ++ ": " ++ err
              go db paths
            Right (factor, sol) -> do
              putStrLn $ "Read " ++ path ++ ":"
              putStrLn $ "  Rotation angle: 2pi * " ++ show factor
              putStrLn $ "  Diamond distance: " ++ show (solutionDistance sol)
              putStrLn $ "  T-count: " ++ show (solutionTCount sol)
              let db' = HashMap.insertWith (const $ insertSQSolution sol) factor [sol] db
              go db' paths
        else
          go db paths
