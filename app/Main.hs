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
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B
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
                    , database  :: String
                                     <?> "(Ensembl | HUGO TYPE | UniProt | RGene (SPECIES, TYPE, TYPE) | MSigDBRdata (FILE, RDATA, TYPE)) Which database to convert with. TYPE is the type of the original gene symbol. The compatible list for TYPE with HUGO is in http://www.genenames.org/help/rest-web-service-help. HUGO is only supported for Annotation. RGene (Annotation only) takes in a type of (SPECIES, FROM, TO) for the gene symbol origin and destination, where species is generally \"hsapiens_gene_ensembl\" or \"mmusculus_gene_ensembl\". MSigDBRdata (Info only) takes an rdata file (tested with http://bioinf.wehi.edu.au/software/MSigDB/), the name of the rdata object containing the named list, and the TYPE of symbol (compatible list at http://bioconductor.org/packages/release/bioc/manuals/biomaRt/man/biomaRt.pdf in getGene) which returns pathways separated by \"/\"."
                    , descriptionField :: Maybe String
                                      <?> "(Other TEXT | Description | Synonyms) The info to retrieve about the identifier. Description provides information about the identifier while synonyms provides alternate identifiers for the same entity. Returns a list of information (delimited by '/') for each match to Ensembl's cross references. For UniProt, enter a valid column (http://www.uniprot.org/help/programmatic_access)."
                    , column           :: T.Text
                                      <?> "(COLUMN) The column containing the identifier. Must be a valid id for info."
                    , newColumn        :: Maybe T.Text
                                      <?> "([Nothing] | COLUMN) The new column to put the results into. If unspecified, replaces the original column."
                    , remove           :: Bool
                                      <?> "Whether to remove empty results (no matches to the database)."
                    , strict           :: Bool
                                      <?> "Whether to load everything in memory, no streaming. Useful for the R conversions only."
                    }
             | Annotation { delimiter :: Maybe String
                                     <?> "([,] | CHAR) The delimiter of the CSV file."
                          , database  :: String
                                     <?> "(Ensembl | HUGO TYPE | UniProt | RGene (SPECIES, TYPE, TYPE) | MSigDBRdata (FILE, RDATA, TYPE)) Which database to convert with. TYPE is the type of the original gene symbol. The compatible list for TYPE with HUGO is in http://www.genenames.org/help/rest-web-service-help. HUGO is only supported for Annotation. RGene (Annotation only) takes in a type of (SPECIES, FROM, TO) for the gene symbol origin and destination, where species is generally \"hsapiens_gene_ensembl\" or \"mmusculus_gene_ensembl\". MSigDBRdata (Info only) takes an rdata file (tested with http://bioinf.wehi.edu.au/software/MSigDB/), the name of the rdata object containing the named list, and the TYPE of symbol (compatible list at http://bioconductor.org/packages/release/bioc/manuals/biomaRt/man/biomaRt.pdf in getGene) which returns pathways separated by \"/\"."
                          , column    :: T.Text
                                     <?> "(COLUMN) The column containing the identifier. Must be a valid id for info."
                          , newColumn :: Maybe T.Text
                                     <?> "([Nothing] | COLUMN) The new column to put the results into. If unspecified, replaces the original column."
                          , remove    :: Bool
                                     <?> "Whether to remove empty results (no matches to the database)."
                          , strict    :: Bool
                                     <?> "Whether to load everything in memory, no streaming. Useful for the R conversions only."
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
        newX <- lift . convertSingle opts rMart rData . (!! c) $ x
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

-- | Convert the entire file at once, no streaming.
strictConvert :: Options
              -> Maybe (RMart s)
              -> Maybe (RData s)
              -> [[T.Text]]
              -> IO ()
strictConvert _ _ _ []                  = error "Empty file."
strictConvert _ _ _ (_:[])              = error "Empty file."
strictConvert opts rMart rData (h:body) = do
    let c      = col opts h
        newCol = unHelpful . newColumn $ opts
        newH   = maybe h (\x -> h <> [x]) $ newCol
        xs     = fmap (!! c) body

    newXS <- convertMultiple opts rMart rData $ xs

    let addToRow newX row =
            if (unHelpful . remove $ opts) && (T.null newX)
                then Nothing
                else Just
                   . maybe (L.set (L.ix c) newX row) (\x -> row <> [newX])
                   $ newCol
        newBody          = catMaybes . zipWith addToRow newXS $ body

    B.putStrLn . CSV.encode . (:) newH $ newBody

-- | The conversion process for streaming.
convertSingle :: Options
              -> Maybe (RMart s)
              -> Maybe (RData s)
              -> T.Text
              -> IO T.Text
convertSingle opts@(Info { descriptionField = df }) rMart rData =
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
convertSingle opts@(Annotation {}) rMart rData                  =
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

-- | The conversion process for all in memory.
convertMultiple :: Options
                -> Maybe (RMart s)
                -> Maybe (RData s)
                -> [T.Text]
                -> IO [T.Text]
convertMultiple opts@(Info { descriptionField = df }) rMart rData =
    fmap (fmap (fromMaybe "" . fmap unDesc))
        . whichDesc (read . unHelpful . database $ opts)
        . fmap UnknownAnn
  where
    whichDesc Ensembl  =
        mapM ( toEnsemblDesc ( read
                             . fromMaybe (error "Needs description field.")
                             . unHelpful
                             $ df
                             )
             )
    whichDesc (HUGO _) = error "HUGO description not yet supported."
    whichDesc UniProt  =
        mapM (toUniProtDesc ( read
                            . fromMaybe (error "Needs description field.")
                            . unHelpful
                            $ df
                            )
             )
    whichDesc (RGene _) = error "RGene description not yet supported."
    whichDesc (MSigDBRData queryType) =
        toMSigDBPathwaysMultiple
            (fromJust rData)
            (fromJust rMart)
            (MSigDBType queryType)
convertMultiple opts@(Annotation {}) rMart rData                  =
    fmap (fmap (fromMaybe "" . fmap unAnn))
        . whichAnn (read . unHelpful . database $ opts)
        . fmap UnknownAnn
  where
    whichAnn Ensembl           = mapM toEnsemblAnn
    whichAnn (HUGO queryType)  = mapM (toHUGOAnn . HUGOType $ queryType)
    whichAnn UniProt           = mapM toUniProtAnn
    whichAnn (RGene queryType) =
        toRGeneAnnMultiple (fromJust rMart) (RType queryType)
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
                    (RGene (species, _, _)) -> fmap Just $ getRMart species
                    (MSigDBRData _) -> fmap Just $ getRMart "hsapiens_gene_ensembl"
                    _               -> return Nothing
        rData <- case read . unHelpful . database $ opts of
                    (MSigDBRData (!file, !object, _)) ->
                        fmap Just . getRData (File file) $ object
                    _                                 -> return Nothing

        if unHelpful . strict $ opts
            then do
                contents <- liftIO B.getContents
                liftIO
                    . strictConvert opts rMart rData
                    . V.toList
                    . either error id
                    $ ( CSV.decode NoHeader contents
                     :: Either String (V.Vector [T.Text])
                      )
            else
                liftIO $ runEffect $ decodeWith csvOpts NoHeader PB.stdin
                    >-> P.concat
                    >-> (pipeConvert opts rMart rData)
                    >-> encode
                    >-> PB.stdout

        return ()
