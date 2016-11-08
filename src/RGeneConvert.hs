{- RGeneConvert
Gregory W. Schwartz

Collections the functions pertaining to converting certain annotations into
other annotations using biomart from R
(http://bioconductor.org/packages/release/bioc/html/biomaRt.html).
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RGeneConvert
    ( getRMart
    , toRGeneAnn
    , toRGeneAnnMultiple
    ) where

-- Standard
import qualified Data.Map.Strict as Map

-- Cabal
import qualified Data.Text as T

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import Language.R.Literal as R

-- Local
import Types

-- | Get the R mapping of gene to gene.
getRMart :: R s (RMart s)
getRMart = fmap RMart
         $ [r| library(biomaRt)
               mart = useMart("ensembl", dataset="hsapiens_gene_ensembl")
           |]

-- | Get the R mapping of gene to gene.
toRGeneAnn :: RMart s -> RType -> UnknownAnn -> IO (Maybe Ann)
toRGeneAnn _ _ (UnknownAnn "")            = return Nothing
toRGeneAnn rMart (RType (!from, !to)) (UnknownAnn textQuery) =
    (fmap . fmap) Ann $ R.runRegion $ do
      let query = T.unpack textQuery
          mart  = unRMart rMart
      res <- [r| map = getBM( attributes = c(from_hs, to_hs)
                            , filters = from_hs
                            , values = c(query_hs)
                            , mart = mart_hs
                            , uniqueRows=FALSE
                            )
                as.character(map[1,2])
            |]

      let naCheck "NA" = Nothing
          naCheck x    = Just x
      return . fmap T.pack . naCheck $ (R.fromSomeSEXP res :: String)

-- | Get the R mapping of a list of genes to genes.
toRGeneAnnMultiple :: RMart s -> RType -> [UnknownAnn] -> IO [Maybe Ann]
toRGeneAnnMultiple rMart (RType (!from, !to)) textQueries = R.runRegion $ do
    let queries = fmap (T.unpack . unUnknownAnn) textQueries
        mart    = unRMart rMart
    res <- [r| getBM( attributes = c(from_hs, to_hs)
                    , filters = from_hs
                    , values = queries_hs
                    , mart = mart_hs
                    , uniqueRows = TRUE
                    )
          |]

    origR <- [r| as.character(res_hs[,1]) |]
    destR <- [r| as.character(res_hs[,2]) |]

    let orig = R.fromSomeSEXP origR :: [String]
        dest = R.fromSomeSEXP destR :: [String]

    let annMap = Map.fromList . zip orig $ dest
    return . fmap (fmap (Ann . T.pack) . flip Map.lookup annMap) $ queries
