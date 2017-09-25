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
import System.IO

import Control.Monad.State

data ASTWriteSpec = ASTWriteSpec {
    delimText :: Text
  , indentText :: Text
  }

stbSpec = ASTWriteSpec " " ""
humanSpec = ASTWriteSpec "\n" "  "

writeSTBHandle :: ProbSeq String -> FilePath -> (Handle -> IO (), [(MatSeq String, FilePath)])
writeSTBHandle seq fp = let (content, mats) = writeAST stbSpec seq fp
                        in (\h -> Text.hPutStr h $ content, mats)

writeSTBFile :: ProbSeq String -> FilePath -> (IO (), [(MatSeq String, FilePath)])
writeSTBFile seq fp = let (write, mats) = writeSTBHandle seq fp
                      in (withFile fp WriteMode write, mats)

putAST :: ProbSeq String -> IO ()
putAST seq = Text.putStrLn . fst $ writeAST humanSpec seq "[base-file]"

writeAST :: ASTWriteSpec -> ProbSeq String -> FilePath -> (Text, [(MatSeq String, FilePath)])
writeAST spec seq fp = (Text.intercalate (delimText spec) . V.toList $ lines, mats)
  where linesM = cataM (buildSTBLines (nextMatrixFilePath fp) (indentWith (indentText spec))) seq
        (lines, mats) = runState linesM []

indentWith :: Text -> Vector Text -> Vector Text
indentWith indentString = V.map (indentString <>) . updHead . updLast
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
              -> (Vector Text -> Vector Text)
              -> Constructor String (Vector Text)
              -> m (Vector Text)
buildSTBLines _ _ EmptySequence = return ["empty"]
buildSTBLines _ _ (State str) = return ["state " <> Text.pack str]
buildSTBLines _ _ (Skip i) = return ["skip " <> tshow i]
buildSTBLines next _ (MatrixForm m) = next m >>= \fp -> return ["fromFile " <> Text.pack fp]
buildSTBLines _ indent (EitherOr p s1 s2) = return $ ("either " <> tshow (fromRational p))
                                     `V.cons` indent s1
                                     <> indent s2
buildSTBLines _ indent (AndThen s1 s2) = return $ "then"
                                  `V.cons` indent s1
                                  <> indent s2
buildSTBLines _ indent (GeometricRepeat p s) = return $ ("geometricRepeat " <> tshow (fromRational p))
                                        `V.cons` indent s
buildSTBLines _ indent (ReverseSequence s) = return $ "reverseSequence"
                                      `V.cons` indent s
buildSTBLines _ indent (Collapse _ _ i s) = return $ ("collapse " <> tshow i)
                                     `V.cons` indent s
buildSTBLines _ indent (Possibly p s) = return $ ("possibly " <> tshow (fromRational p))
                                 `V.cons` indent s
buildSTBLines _ indent (UniformDistOver ss) = return $ "uniform-dist-over" `V.cons` mconcat (map indent ss)
buildSTBLines _ indent (FiniteDistOver pairs) = return $
                                         ("finite-dist-over [" <>
                                           Text.intercalate ", " (map (tshow . fromRational) ps) <> "] ")
                                         `V.cons` mconcat (map indent ss)
  where (ss, ps) = unzip pairs
buildSTBLines _ indent (FiniteDistRepeat ps s) = return $
                                          ("finite-dist-repeat [" <>
                                            Text.intercalate ", " (map (tshow . fromRational) ps) <> "] ")
                                          `V.cons` indent s
buildSTBLines _ indent (UniformDistRepeat i s) = return $
                                          ("uniform-dist-repeat " <> tshow i)
                                          `V.cons` indent s
buildSTBLines _ indent (Series ss) = return $ "series" `V.cons` mconcat (map indent ss)
buildSTBLines _ indent (Repeat i s) = return $ ("repeat " <> tshow i) `V.cons` indent s
