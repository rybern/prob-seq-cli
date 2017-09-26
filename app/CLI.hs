{-# LANGUAGE RecordWildCards #-}
module CLI where

import WriteConstructionFiles
import ReadConstructionFiles
import Sequence

import Control.Monad
import System.IO
import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))
import Data.List
import qualified Data.Vector as V

data Channel = File FilePath
             | Std
             deriving (Eq)

data ChannelDirection = Input
                      | Output

openChannel :: Channel -> ChannelDirection -> IO Handle
openChannel Std Input = return stdin
openChannel Std Output = return stdout
openChannel (File f) Input = openFile f ReadMode
openChannel (File f) Output = openFile f WriteMode

withChannel :: Channel -> ChannelDirection -> (Handle -> IO r) -> IO r
withChannel c d f = do
  handle <- openChannel c d
  r <- f handle
  hClose handle
  return r

data Options = Options {
    input :: Channel
  , stOutput :: Maybe Channel
  , stpOutput :: Maybe Channel
  , sampling :: Maybe Int
  , stbOutput :: Maybe Channel
  , printAST :: Bool
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
       && printAST == False
       ) $ die "No output or actions - exiting"

  when ((> 1) . length . filter id $
        [ stOutput == Just Std
        , stpOutput == Just Std
        , stbOutput == Just Std
        , printAST
        ]) $ die "More than one action printing to stdout - exiting"

  probSeq <- readSeqChan input
  let matSeq = buildMatSeq probSeq

  forM_ stOutput $ \stChan -> withChannel stChan Output (writeSTHandle matSeq)

  forM_ stpOutput $ \stpChan -> withChannel stpChan Output (writeSTPHandle matSeq)

  forM_ sampling $ \n -> replicateM_ n $ do
    (states, _) <- randToIO $ sampleSeq vecDist matSeq
    putStrLn . intercalate ", " . V.toList $ states

  when printAST $ putAST probSeq

  forM_ stbOutput $ \stbChan -> do
    let fp Std = "[base-file]"
        fp (File f) = f
        (hDoWrite, mats) = writeSTBHandle probSeq (fp stbChan)
    mapM_ (uncurry writeSTPFile) mats
    withChannel stbChan Output hDoWrite

readSeqChan :: Channel -> IO (ProbSeq String)
readSeqChan Std = withChannel Std Input runBuilderHandle
readSeqChan (File f) = readSeqFile f

maybeOption nilval option settings = (\v -> if v == nilval then Nothing else Just v) <$> option settings

channelOption settings = (\v -> if v == "_" then Std else File v) <$> strOption settings

autoOption = option auto

builderHelpOption =
  infoOption builderHelpString
  ( long "builder-help"
    <> help "Print help for writing .stb (transition builder) files, including available constructors.")

cliParser :: Parser Options
cliParser = Options
            <$> channelOption
            ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Input file describing a probabilistic sequence. Accepts sparse matrix format (.st or .stp extensions) or sequence builder file (.stb extension).")
            <*> maybeOption (File "") channelOption
            ( long "st-output"
              <> metavar "ST_FILE"
              <> value ""
              <> help "Output file describing a sparse transition distribution. This format may lose information, but should be interpretable to HMM programs.")
            <*> maybeOption (File "") channelOption
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
            <*> maybeOption (File "") channelOption
            ( long "stb-output"
              <> metavar "STB_FILE"
              <> value ""
              <> help "Output file containing a simplified, standardized form of the input AST. If the input was a matrix, the matrix is wrapped in a \"matrix\" constructor.")
            <*> flag False True
            ( long "print-ast"
              <> help "If set, prints the AST to stdout in a human-readable format.")
