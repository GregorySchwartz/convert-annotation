{- UniProtConvert
Gregory W. Schwartz

Collections the functions pertaining to converting certain annotations into
UniProt annotations.
-}

{-# LANGUAGE OverloadedStrings #-}

module UniProtConvert
    ( toUniProtAnn
    , toUniProtDesc
    ) where

-- Standard
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import Data.Monoid

-- Cabal
import qualified Data.Text as T
import Network.HTTP

-- Local
import Types

-- | Get UniProt annotation.
toUniProtAnn :: UnknownAnn -> IO (Maybe Ann)
toUniProtAnn (UnknownAnn "") = return Nothing
toUniProtAnn query           = do
    uniProtDesc <- toUniProtDesc (UniProtOther "genes") $ query
    return . fmap (Ann . unDesc) $ uniProtDesc

-- | Get UniProt description.
toUniProtDesc :: DescFields -> UnknownAnn -> IO (Maybe Desc)
toUniProtDesc _ (UnknownAnn "")        = return Nothing
toUniProtDesc field (UnknownAnn query) = do
    let base     = "http://www.uniprot.org/uniprot/"
        q        = "?query="
        opts     = "&sort=score&columns="
                <> T.unpack ((\(UniProtOther x) -> x) field)
                <> "&format=tab"
        req      = base <> q <> T.unpack query <> opts

    rsp <- simpleHTTP (getRequest req) >>= getResponseBody

    let uniProtDesc = case rsp of
                        "" -> Nothing
                        x  -> Just . Desc . T.pack . head . drop 1 . lines $ x

    return uniProtDesc
