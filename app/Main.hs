{- convert-annotation
Gregory W. Schwartz

Converts an unknown annotation to Ensembl's annotation, or other annotation.
-}

{-# LANGUAGE BangPatterns      #-}
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
import Data.Semigroup

-- Cabal
import qualified Data.Text as T
import qualified Data.Csv as CSV
import qualified Control.Lens as L
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import Pipes.Csv
import Options.Generic

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import Language.R.Literal as R

-- Local
import Types
import EnsemblConvert
import HUGOConvert
import UniProtConvert
import RGeneConvert
import MSigDBRDataConvert

-- | Command line arguments
data Options = Info { delimiter        :: Maybe String
                                      <?> "([,] | CHAR) The delimiter of the CSV file."
                    , database         :: String
                                      <?> "(Ensembl | HUGO TYPE | UniProt | RGene (TYPE, TYPE) | MSigDBRdata (FILE, RDATA, TYPE)) Which database to convert with. TYPE is the type of the original gene symbol. The compatible list for TYPE with HUGO is in http://www.genenames.org/help/rest-web-service-help. HUGO is only supported for Annotation. RGene (Annotation only) takes in a type of (FROM, TO) for the gene symbol origin and destination. MSigDBRdata (Info only) takes an rdata file (tested with http://bioinf.wehi.edu.au/software/MSigDB/), the name of the rdata object containing the named list, and the TYPE of symbol (compatable list at http://bioconductor.org/packages/release/bioc/manuals/biomaRt/man/biomaRt.pdf in getGene) which returns pathways separated by \"/\"."
                    , descriptionField :: Maybe String
                                      <?> "(Other TEXT | Description | Synonyms) The info to retrieve about the identifier. Description provides information about the identifier while synonyms provides alternate identifiers for the same entity. Returns a list of information (delimited by '/') for each match to Ensembl's cross references. For UniProt, enter a valid column (http://www.uniprot.org/help/programmatic_access)."
                    , column           :: T.Text
                                      <?> "(COLUMN) The column containing the identifier. Must be a valid id for info."
                    , newColumn        :: Maybe T.Text
                                      <?> "([Nothing] | COLUMN) The new column to put the results into. If unspecified, replaces the original column."
                    , remove           :: Bool
                                      <?> "Whether to remove empty results (no matches to the database)."
                    }
             | Annotation { delimiter :: Maybe String
                                     <?> "([,] | CHAR) The delimiter of the CSV file."
                          , database  :: String
                                     <?> "(Ensembl | HUGO TYPE | UniProt | RGene (TYPE, TYPE) | MSigDBRdata (FILE, RDATA, TYPE)) Which database to convert with. TYPE is the type of the original gene symbol. The compatible list for TYPE with HUGO is in http://www.genenames.org/help/rest-web-service-help. HUGO is only supported for Annotation. RGene (Annotation only) takes in a type of (FROM, TO) for the gene symbol origin and destination. MSigDBRdata (Info only) takes an rdata file (tested with http://bioinf.wehi.edu.au/software/MSigDB/), the name of the rdata object containing the named list, and the TYPE of symbol (compatable list at http://bioconductor.org/packages/release/bioc/manuals/biomaRt/man/biomaRt.pdf in getGene) which returns pathways separated by \"/\"."
                          , column    :: T.Text
                                     <?> "(COLUMN) The column containing the identifier. Must be a valid id for info."
                          , newColumn :: Maybe T.Text
                                     <?> "([Nothing] | COLUMN) The new column to put the results into. If unspecified, replaces the original column."
                          , remove    :: Bool
                                     <?> "Whether to remove empty results (no matches to the database)."
                          }
               deriving (Generic)

instance ParseRecord Options

-- | Map the header column to the rest of the file for converting that
-- column.
pipeConvert :: Options
            -> Maybe (RMart s)
            -> Maybe (RData s)
            -> Pipe [T.Text] [T.Text] IO ()
pipeConvert opts rMart rData = do
    h <- await

    let c      = col opts h
        newCol = unHelpful . newColumn $ opts

    yield . maybe h (\x -> h <> [x]) $ newCol

    forever $ do
        x    <- await
        newX <- lift . convert opts rMart rData . (!! c) $ x
        unless ((unHelpful . remove $ opts) && T.null newX)
            . maybe (yield . L.set (L.ix c) newX $ x)
                    (const (yield (x <> [newX])))
            $ newCol
        return ()

    return ()

-- | Get the index of the column.
col :: Options -> [T.Text] -> Int
col opts =
    fromMaybe (error "Column not found.") . elemIndex (unHelpful $ column opts)

-- | The conversion process.
convert :: Options -> Maybe (RMart s) -> Maybe (RData s) -> T.Text -> IO T.Text
convert opts@(Info { descriptionField = df }) rMart rData =
    fmap (fromMaybe "" . fmap unDesc)
        . whichDesc (read . unHelpful . database $ opts)
        . UnknownAnn
  where
    whichDesc Ensembl  = toEnsemblDesc ( read
                                       . fromMaybe (error "Needs description field.")
                                       . unHelpful
                                       $ df
                                       )
    whichDesc (HUGO _) = error "HUGO description not yet supported."
    whichDesc UniProt  = toUniProtDesc ( read
                                       . fromMaybe (error "Needs description field.")
                                       . unHelpful
                                       $ df
                                       )
    whichDesc (RGene _) = error "RGene description not yet supported."
    whichDesc (MSigDBRData queryType) =
        toMSigDBPathways
            (fromJust rData)
            (fromJust rMart)
            (MSigDBType queryType)
convert opts@(Annotation {}) rMart rData                  =
    fmap (fromMaybe "" . fmap unAnn)
        . whichAnn (read . unHelpful . database $ opts)
        . UnknownAnn
  where
    whichAnn Ensembl           = toEnsemblAnn
    whichAnn (HUGO queryType)  = toHUGOAnn . HUGOType $ queryType
    whichAnn UniProt           = toUniProtAnn
    whichAnn (RGene queryType) =
        toRGeneAnn (fromJust rMart) (RType queryType)
    whichAnn (MSigDBRData _)   =
        error "MSigDBRData annotation not yet supported."

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

    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        rMart <- case read . unHelpful . database $ opts of
                    (RGene _)       -> fmap Just getRMart
                    (MSigDBRData _) -> fmap Just getRMart
                    _               -> return Nothing
        rData <- case read . unHelpful . database $ opts of
                    (MSigDBRData (!file, !object, _)) ->
                        fmap Just . getRData (File file) $ object
                    _                                 -> return Nothing

        liftIO $ runEffect $ decodeWith csvOpts NoHeader PB.stdin
            >-> P.concat
            >-> (pipeConvert opts rMart rData)
            >-> encode
            >-> PB.stdout

        return ()
