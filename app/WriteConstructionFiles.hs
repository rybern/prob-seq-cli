{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module WriteConstructionFiles where

import Sequence

import Data.Fix
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Semigroup ((<>))

import Control.Monad.State

writeSTBFile :: ProbSeq String -> FilePath -> IO [(MatSeq String, FilePath)]
writeSTBFile seq fp = do
  Text.writeFile fp . Text.unlines . V.toList $ lines
  return mats
  where linesM = cataM (buildSTBLines (nextMatrixFilePath fp)) seq
        (lines, mats) = runState linesM []

indent :: Vector Text -> Vector Text
indent = V.map ("  " <>) . updHead . updLast
  where updHead v = v V.// [(0, '(' `Text.cons` (V.head v))]
        updLast v = v V.// [(V.length v - 1, V.last v `Text.snoc` ')')]

matrixFilePath :: FilePath -> Int -> FilePath
matrixFilePath base ix = base ++ ".matrix_"++show ix

nextMatrixFilePath :: FilePath -> MatSeq String -> State [(MatSeq String, FilePath)] FilePath
nextMatrixFilePath base seq = do
  ls <- get
  let next = matrixFilePath base (length ls)
  put $ (seq, next) : ls
  return next

tshow :: (Show s) => s -> Text
tshow = Text.pack . show

buildSTBLines :: (Monad m)
              => (MatSeq String -> m FilePath)
              -> Constructor String (Vector Text)
              -> m (Vector Text)
buildSTBLines _ EmptySequence = return ["empty"]
buildSTBLines _ (State str) = return ["state " <> Text.pack str]
buildSTBLines _ (Skip i) = return ["skip " <> tshow i]
buildSTBLines next (MatrixForm m) = next m >>= \fp -> return ["fromFile " <> Text.pack fp]
buildSTBLines _ (EitherOr p s1 s2) = return $ ("either " <> tshow (fromRational p))
                                     `V.cons` indent s1
                                     <> indent s2
buildSTBLines _ (AndThen s1 s2) = return $ "then"
                                  `V.cons` indent s1
                                  <> indent s2
buildSTBLines _ (GeometricRepeat p s) = return $ ("geometricRepeat " <> tshow (fromRational p))
                                        `V.cons` indent s
buildSTBLines _ (ReverseSequence s) = return $ "reverseSequence"
                                      `V.cons` indent s
buildSTBLines _ (Collapse _ _ i s) = return $ ("collapse " <> tshow i)
                                     `V.cons` indent s
buildSTBLines _ (Possibly p s) = return $ ("possibly " <> tshow (fromRational p))
                                 `V.cons` indent s
buildSTBLines _ (UniformDistOver ss) = return $ "uniform-dist-over" `V.cons` mconcat (map indent ss)
buildSTBLines _ (FiniteDistOver pairs) = return $
                                         ("finite-dist-over [" <>
                                           Text.intercalate ", " (map (tshow . fromRational) ps) <> "] ")
                                         `V.cons` mconcat (map indent ss)
  where (ss, ps) = unzip pairs
buildSTBLines _ (FiniteDistRepeat ps s) = return $
                                          ("finite-dist-repeat [" <>
                                            Text.intercalate ", " (map (tshow . fromRational) ps) <> "] ")
                                          `V.cons` indent s
buildSTBLines _ (UniformDistRepeat i s) = return $
                                          ("uniform-dist-repeat " <> tshow i)
                                          `V.cons` indent s
buildSTBLines _ (Series ss) = return $ "series" `V.cons` mconcat (map indent ss)
buildSTBLines _ (Repeat i s) = return $ ("repeat " <> tshow i) `V.cons` indent s
