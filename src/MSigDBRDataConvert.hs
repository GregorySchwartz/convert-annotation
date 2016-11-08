{- MSigDBRDataConvert
Gregory W. Schwartz

Collections the functions pertaining to converting certain annotations into
pathways using the MSigDB rdata files (tested with
http://bioinf.wehi.edu.au/software/MSigDB/).
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MSigDBRDataConvert
    ( getRData
    , toMSigDBPathways
    , toMSigDBPathwaysMultiple
    ) where

-- Standard

-- Cabal
import qualified Data.Text as T

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import Language.R.Literal as R
import H.Prelude

-- Local
import Types
import RGeneConvert

-- | Get the RData object.
getRData :: File -> String -> R s (RData s)
getRData (File file) object = fmap RData
                            $ [r| load(file_hs)
                                  res = get(object_hs)
                                  res
                              |]


-- | Get the pathways that contain this entrez gene id.
getPathway :: RData s -> Ann -> IO (Maybe Desc)
getPathway (RData object) (Ann entrezText) = R.runRegion $ do
    let entrez = T.unpack entrezText

    res <- [r| pathNames = names(object_hs[unlist(lapply(object_hs, function(x) (entrez_hs %in% unlist(x))))]) |]

    let pathNames = R.fromSomeSEXP res :: [String]

    if null . drop 1 $ pathNames
        then return Nothing
        else return . Just . Desc . T.intercalate "/" . fmap T.pack $ pathNames

-- | Get the R mapping of a gene to its pathways.
toMSigDBPathways
    :: RData s
    -> RMart s
    -> MSigDBType
    -> UnknownAnn
    -> IO (Maybe Desc)
toMSigDBPathways _ _ _ (UnknownAnn "")            = return Nothing
toMSigDBPathways rData rMart (MSigDBType (_, _, !from)) query = R.runRegion $ do
    entrez <- io . toRGeneAnn rMart (RType (from, "entrezgene")) $ query
    maybe (return Nothing) (io . getPathway rData) $ entrez

-- | Get the R mapping of multiple genes to pathways.
toMSigDBPathwaysMultiple
    :: RData s
    -> RMart s
    -> MSigDBType
    -> [UnknownAnn]
    -> IO [Maybe Desc]
toMSigDBPathwaysMultiple rData rMart (MSigDBType (_, _, !from)) queries = do
      entrez <- toRGeneAnnMultiple rMart (RType (from, "entrezgene")) $ queries
      mapM (maybe (return Nothing) (getPathway rData)) entrez
