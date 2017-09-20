{-# LANGUAGE OverloadedStrings #-}
module ConstructionFiles where

import Data.Text hiding (concat, map, zip)
import Data.Text as Text (unlines)
import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import System.FilePath.Posix
import System.Exit
import System.IO.Error

import BuildConstructor

import Sequence hiding (constructor)

readSeqFile :: FilePath ->
               IO (MatSeq String)
readSeqFile fp =
  case takeExtension fp of
    ".st" -> readSTFile fp >>= throwError
    ".stp" -> readSTPFile fp >>= throwError
    ".stb" -> runBuilderFile fp
  where throwError (Right r) = return r
        throwError (Left e) = ioError $ userError e

builderHelpString :: String
builderHelpString = Prelude.unlines . map unpack $ builderHelpLines spec

runBuilderFile :: FilePath -> IO (MatSeq String)
runBuilderFile fp = buildValueFromFileM spec fp >>= \res ->
  case res of
    Left e -> die (show e)
    Right v -> eitherT (die . show) (return . buildMatSeq) v

{-
runBuilderLines :: Text -> IO (MatSeq Char)
runBuilderLines txt =
  case buildValueM spec txt of
    Left e -> die (show e)
    Right v -> eitherT (die . show) (return . buildMatSeq) v
-}

spec :: TinyLangSpec IO (ProbSeq String)
spec = TinyLangSpec
  {
    valName = "SEQUENCE"
  , constructors =
      [
        TinyLangConstructor
        {
          sym = "seq"
        , constructor = SimpleString (C0 . deterministicSequence . V.fromList . map return . unpack)
        , constructorDescription = [
              "Deterministic sequence"
            ]
        }
      , TinyLangConstructor
        {
          sym = "collapse"
        , constructor = SimpleInt (\n -> Composer (C0 . collapse undefined undefined (fromIntegral n)))
        , constructorDescription = [ "Collapse each sliding window of n states into a single state"]
        }
      , TinyLangConstructor
        {
          sym = "series"
        , constructor = SimpleListOfValue (C0 . Prelude.foldl1 (\a b -> andThen a b))
        , constructorDescription = [ "List of sequences done in series"]
        }
        , TinyLangConstructor
        {
          sym = "series"
        , constructor = SimpleListOfValue (C0 . Prelude.foldl1 (\a b -> andThen a b))
        , constructorDescription = [ "List of sequences done in series"]
        }
      , TinyLangConstructor
        {
          sym = "then"
        , constructor = Composer (\seqA -> Composer (\seqB -> C0 $ andThen seqA seqB))
        , constructorDescription = ["Pair of sequences done in series"]
        }
      , TinyLangConstructor
        {
          sym = "either"
        , constructor = SimpleFloat (\p -> Composer (\seqA -> Composer (\seqB -> C0 $ eitherOr (toRational p) seqA seqB)))
        , constructorDescription = ["Do the first sequence with probability, otherwise do the second sequence"]
        }
      , TinyLangConstructor
        {
          sym = "empty"
        , constructor = C0 emptySequence
        , constructorDescription = ["MatSeq with no states"]
        }
      , TinyLangConstructor
        {
          sym = "reverse"
        , constructor = Composer (C0 . reverseSequence)
        , constructorDescription = ["Reverse the states and transition probabilities of a sequence"]
        }
      , TinyLangConstructor
        {
          sym = "possibly"
        , constructor = SimpleFloat (\p -> Composer (C0 . possibly (toRational p)))
        , constructorDescription = ["Do the sequence with probability, otherwise do nothing"]
        }
      , TinyLangConstructor
        {
          sym = "uniform-dist-over"
        , constructor = SimpleListOfValue (C0 . uniformDistOver)
        , constructorDescription = ["Do any of a list of sequences, with equal probability"]
        }
      , TinyLangConstructor
        {
          sym = "finite-dist-over"
        , constructor = SimpleListOfFloat (\ps ->
                                             SimpleListOfValue (\seqs ->
                                                                  C0 $ finiteDistOver
                                                                  (zip
                                                                    seqs
                                                                    (map toRational ps))))
        , constructorDescription = ["Do any of a list of sequences, with corresponding probability"]
        }
      --, TinyLangConstructor
        --{
          --sym = "skipdist"
        --, constructor = SimpleListOfFloat (\ps ->
                                             --Composer (\seq ->
                                                         --C0 . Fix $ SkipDist (map toRational ps) seq))
        --, constructorDescription = ["Each step of the result is the equivalent of some number of steps of the arguments,"
                                   --, "with the number of steps drawn from the step distribution"]
        --}
      , TinyLangConstructor
        {
          sym = "repeat"
        , constructor = SimpleInt (\n -> Composer (C0 . repeatSequence (fromIntegral n)))
        , constructorDescription = ["Repeat the sequence N times"]
        }
      , TinyLangConstructor
        {
          sym = "uniform-dist-repeat"
        , constructor = SimpleInt (\n -> Composer (C0 . uniformDistRepeat (fromIntegral n)))
        , constructorDescription = ["Repeat the sequence between 0 and N times, with 1/n+1 chance of each"]
        }
      , TinyLangConstructor
        {
          sym = "finite-dist-repeat"
        , constructor = SimpleListOfFloat (\ps ->
                                Composer (C0 . finiteDistRepeat (map toRational ps)))
        , constructorDescription = ["Repeat the sequence a number of times with corresponding probability, indexed from 0"]
        }
      , TinyLangConstructor
        {
          sym = "read-file"
        , constructor = SimpleString (\fp -> Monadic (C0 . matrixForm <$> readSeqFile (unpack fp)))
        , constructorDescription = ["Read a .st, .stp, or .stb file as a sequence"]
        }
      ]
  }

{-
testFile = Text.unlines
  [
    "seqA = deterministic \"abcd\""
  , "seqB = deterministic \"efg\""
  , "andThen seqA (eitherOr 0.5 emptyMatSeq seqB)"
  ]

built = buildValueM spec testFile
-}
