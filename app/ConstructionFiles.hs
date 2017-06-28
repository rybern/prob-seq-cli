{-# LANGUAGE OverloadedStrings #-}
module ConstructionFiles where

import Data.Text hiding (concat, map, zip)
import Data.Text as Text (unlines)
import qualified Math.LinearAlgebra.Sparse as M
import qualified Data.Vector as V
import System.FilePath.Posix
import System.Exit

import BuildConstructor

import Sequence
import Sequence.IO.TransMatrix

readSeqFile :: FilePath ->
               IO (Sequence Char)
readSeqFile fp =
  case takeExtension fp of
    ".st" -> readSTFile fp
    ".stp" -> readSTPFile fp
    ".stb" -> runBuilderFile fp

builderHelpString :: String
builderHelpString = Prelude.unlines . map unpack $ builderHelpLines spec

printHelp :: IO ()
printHelp = printBuilderHelp spec

runBuilderFile :: FilePath -> IO (Sequence Char)
runBuilderFile fp = buildValueFromFileM spec fp >>= \res ->
  case res of
    Left e -> die (show e)
    Right v -> eitherT (die . show) return v

runBuilderLines :: Text -> IO (Sequence Char)
runBuilderLines txt =
  case buildValueM spec txt of
    Left e -> die (show e)
    Right v -> eitherT (die . show) return v

spec :: TinyLangSpec IO (Sequence Char)
spec = TinyLangSpec
  {
    valName = "SEQUENCE"
  , constructors =
      [
        TinyLangConstructor
        {
          sym = "seq"
        , constructor = SimpleString (C0 . deterministicSequence . V.fromList . unpack)
        , constructorDescription = [
              "Deterministic sequence"
            ]
        }
      , TinyLangConstructor
        {
          sym = "series"
        , constructor = SimpleListOfValue (C0 . Prelude.foldl1 andThen)
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
        , constructor = SimpleFloat (\p -> Composer (\seqA -> Composer (\seqB -> C0 $ eitherOr p seqA seqB)))
        , constructorDescription = ["Do the first sequence with probability, otherwise do the second sequence"]
        }
      , TinyLangConstructor
        {
          sym = "empty"
        , constructor = C0 emptySequence
        , constructorDescription = ["Sequence with no states"]
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
        , constructor = SimpleFloat (\p -> Composer (\seq -> C0 $ possibly p seq))
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
                                                                  C0 $ finiteDistOver (zip seqs ps)))
        , constructorDescription = ["Do any of a list of sequences, with corresponding probability"]
        }
      , TinyLangConstructor
        {
          sym = "skipdist"
        , constructor = SimpleListOfFloat (\ps ->
                                             Composer (\seq ->
                                                         C0 $ skipDist (M.vecFromAssocList (zip [1..] ps)) seq))
        , constructorDescription = ["Each step of the result is the equivalent of some number of steps of the arguments,"
                                   , "with the number of steps drawn from the step distribution"]
        }
      , TinyLangConstructor
        {
          sym = "uniform-dist-repeat"
        , constructor = SimpleInt (\n -> Composer (\seq -> C0 $ uniformDistRepeat (fromIntegral n) seq))
        , constructorDescription = ["Repeat the sequence between 0 and N times, with 1/n+1 chance of each"]
        }
      , TinyLangConstructor
        {
          sym = "finite-dist-repeat"
        , constructor = SimpleListOfFloat (\ps ->
                                Composer (\seq ->
                                             C0 $ finiteDistRepeat ps seq))
        , constructorDescription = ["Repeat the sequence a number of times with corresponding probability"]
        }
      , TinyLangConstructor
        {
          sym = "read-file"
        , constructor = SimpleString (\fp -> Monadic (C0 <$> readSeqFile (unpack fp)))
        , constructorDescription = ["Read a .st, .stp, or .stb file as a sequence"]
        }
      ]
  }

{-
testFile = Text.unlines
  [
    "seqA = deterministic \"abcd\""
  , "seqB = deterministic \"efg\""
  , "andThen seqA (eitherOr 0.5 emptySequence seqB)"
  ]

built = buildValueM spec testFile
-}
