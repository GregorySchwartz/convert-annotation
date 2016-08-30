{- convert-annotation
Gregory W. Schwartz

Converts an unknown annotation to Ensembl's annotation, or other annotation.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Standard
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad
import GHC.Generics

-- Cabal
import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Control.Lens as L
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import Pipes.Csv
import Options.Generic

-- Local
import Types
import EnsemblConvert
import UniProtConvert

-- | Command line arguments
data Options = Info { delimiter        :: Maybe String
                                      <?> "([,] | CHAR) The delimiter of the CSV file."
                    , database         :: String
                                      <?> "(Ensembl | UniProt) Which database to convert with."
                    , descriptionField :: String
                                      <?> "(Other TEXT | Description | Synonyms) The info to retrieve about the identifier. Description provides information about the identifier while synonyms provides alternate identifiers for the same entity. Returns a list of information (delimited by '/') for each match to Ensembl's cross references. For UniProt, enter a valid column (http://www.uniprot.org/help/programmatic_access)."
                    , column           :: T.Text
                                      <?> "(COLUMN) The column containing the identifier. Must be a valid id for info."
                    , remove           :: Bool
                                      <?> "Whether to remove empty results (no matches to the database)."
                    }
             | Annotation { delimiter :: Maybe String
                                     <?> "([,] | CHAR) The delimiter of the CSV file."
                          , database  :: String
                                     <?> "(Ensembl | UniProt) Which database to convert with."
                          , column    :: T.Text
                                     <?> "(COLUMN) The column containing the identifier. Must be a valid id for info."
                          , remove    :: Bool
                                     <?> "Whether to remove empty results (no matches to the database)."
                          }
               deriving (Generic)

instance ParseRecord Options

-- | Map the header column to the rest of the file for converting that
-- column.
pipeConvert :: Options -> Pipe [T.Text] [T.Text] IO ()
pipeConvert opts = do
    h <- await

    let c = col opts h

    yield h

    forever $ do
        x    <- await
        newX <- lift . convert opts . (!! c) $ x
        unless
            ((unHelpful . remove $ opts) && T.null newX)
            (yield . L.set (L.ix c) newX $ x)
        return ()

    return ()

-- | Get the index of the column.
col :: Options -> [T.Text] -> Int
col opts =
    fromMaybe (error "Column not found.") . elemIndex (unHelpful $ column opts)

-- | The conversion process.
convert :: Options -> T.Text -> IO T.Text
convert opts@(Info { descriptionField = df }) =
    fmap (fromMaybe "" . fmap unDesc)
        . whichDesc (read . unHelpful . database $ opts)
        . UnknownAnn
  where
    whichDesc Ensembl = toEnsemblDesc (read . unHelpful $ df)
    whichDesc UniProt = toUniProtDesc (read . unHelpful $ df)
convert opts@(Annotation {})                  =
    fmap (fromMaybe "" . fmap unAnn)
        . whichDesc (read . unHelpful . database $ opts)
        . UnknownAnn
  where
    whichDesc Ensembl = toEnsemblAnn
    whichDesc UniProt = toUniProtAnn

main :: IO ()
main = do
    opts <- getRecord "convert-annotation, Gregory W. Schwartz.\
                      \ Converts an unknown annotation to some other\
                      \ annotation."

    let delim = case unHelpful . delimiter $ opts of
                    Nothing         -> ','
                    (Just "\\t")    -> '\t'
                    (Just [x])      -> x
                    (Just [])       -> error "No delimiter set"
                    _         -> error "Delimiter is one character"
        csvOpts = CSV.defaultDecodeOptions
                    { CSV.decDelimiter = fromIntegral (ord delim) }

    runEffect $ decodeWith csvOpts NoHeader PB.stdin
            >-> P.concat
            >-> (pipeConvert opts)
            >-> encode
            >-> PB.stdout

    return ()
