{-# LANGUAGE RecordWildCards #-}
module CLI where

import WriteConstructionFiles
import ReadConstructionFiles
import Sequence

import Control.Monad
import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options {
    inputFile :: FilePath
  , stOutput :: Maybe FilePath
  , stpOutput :: Maybe FilePath
  , sampling :: Maybe Int
  , stbOutput :: Maybe FilePath
  }

runCLI :: IO ()
runCLI = execParser opts >>= runOptions
  where opts = info ((builderHelpOption <*> cliParser) <**> helper)
          (fullDesc
           <> progDesc "Build transition matrices by combining probabilistic sequences")

runOptions :: Options -> IO ()
runOptions (Options {..}) = do
  when (  stOutput == Nothing
       && stpOutput == Nothing
       && sampling == Nothing
       && stbOutput == Nothing
       ) $ die "No output or actions - exiting"

  probSeq <- readSeqFile (inputFile)
  let matSeq = buildMatSeq probSeq

  forM_ stOutput $ \fp -> do
    writeSTFile matSeq fp

  forM_ stpOutput $ \fp -> do
    writeSTPFile matSeq fp

  forM_ sampling $ \n -> replicateM_ n $ do
    (states, _) <- randToIO $ sampleSeq vecDist matSeq
    print states

  forM_ stbOutput $ \fp -> do
    mats <- writeSTBFile probSeq fp
    mapM_ (uncurry writeSTPFile) mats

maybeOption nilval option settings = (\v -> if v == nilval then Nothing else Just v) <$> option settings

autoOption = option auto

builderHelpOption =
  infoOption builderHelpString
  ( long "builder-help"
    <> help "Print help for writing .stb (transition builder) files, including available constructors.")

cliParser :: Parser Options
cliParser = Options
            <$> strOption
            ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Input file describing a probabilistic sequence. Accepts sparse matrix format (.st or .stp extensions) or sequence builder file (.stb extension).")
            <*> maybeOption "" strOption
            ( long "st-output"
              <> metavar "ST_FILE"
              <> value ""
              <> help "Output file describing a sparse transition distribution. This format may lose information, but should be interpretable to HMM programs.")
            <*> maybeOption "" strOption
            ( long "stp-output"
              <> metavar "STP_FILE"
              <> value ""
              <> help "Output file describing a sparse transition distribution. This format should keep all information, but may not be interpretable to HMM programs.")
            <*> maybeOption 0 autoOption
            ( long "sample"
              <> short 's'
              <> metavar "N_SAMPLES"
              <> value 0
              <> help "Samples from the input sequence N_SAMPLES number of times. Prints each sample in a new line to stdout.")
            <*> maybeOption "" strOption
            ( long "stb-output"
              <> metavar "STB_FILE"
              <> value ""
              <> help "Output file containing a simplified, standardized form of the input AST. If the input was a matrix, the matrix is wrapped in a \"matrix\" constructor.")
