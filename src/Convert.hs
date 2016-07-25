{- Convert
Gregory W. Schwartz

Collections the functions pertaining to converting certain annotations into
other annotations.
-}

{-# LANGUAGE OverloadedStrings #-}

module Convert
    ( toEnsemblAnn
    , toEnsemblDesc
    ) where

-- Standard
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import Data.Monoid

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP
import Safe

-- Local
import Types

-- | Null the null value of an object. If the object is Null, return "".
nullNull :: Value -> Parser T.Text
nullNull (String x) = return $ x
nullNull _          = return $ ""

-- | Decode a Ensembl id query as an Ensembl identifier.
decodeEnsemblAnn :: CL.ByteString -> Maybe EnsemblAnn
decodeEnsemblAnn query = getENSG (eitherDecode query :: Either String ([Map.Map T.Text T.Text]))
  where
    getENSG = fmap EnsemblAnn
            . headMay
            . filter (T.isPrefixOf "ENSG")
            . concatMap Map.elems
            . either error id

-- | Decode a Ensembl id query as a description of the id.
decodeEnsemblDesc :: EnsemblDescFields -> CL.ByteString -> EnsemblDesc
decodeEnsemblDesc field query =
    EnsemblDesc
        . T.intercalate "/"
        . filter (not . T.null)
        . fmap (parse field)
        . either error id
        $ (eitherDecode query :: Either String [Object])
  where
    parse :: EnsemblDescFields -> Object -> T.Text
    parse Synonyms    = T.intercalate "/"
                      . filter (not . T.null)
                      . either error id
                      . parseEither (flip (.:) "synonyms")
    parse Description = either error id
                      . parseEither ((=<<) nullNull . flip (.:) "description")
    getDesc = EnsemblDesc
            . T.intercalate "/"
            . filter (not . T.null)
            . fmap (parse field)

-- | Get Ensembl annotation.
toEnsemblAnn :: UnknownAnn -> IO (Maybe EnsemblAnn)
toEnsemblAnn (UnknownAnn query) = do
    let base     = "http://rest.ensembl.org/"
        xrefs    = "xrefs/symbol/homo_sapiens/"
        xrefOpts = "?content-type=application/json;all_levels=1"
        xrefReq  = base <> xrefs <> T.unpack query <> xrefOpts

    xrefRsp   <- simpleHTTP (getRequest xrefReq) >>= getResponseBody

    let ensemblAnn = case xrefRsp of
                        "[]" -> Nothing
                        x    -> decodeEnsemblAnn . CL.pack $ x

    return ensemblAnn

-- | Get Ensembl description.
toEnsemblDesc :: EnsemblDescFields -> UnknownAnn -> IO EnsemblDesc
toEnsemblDesc field (UnknownAnn query) = do
    let base     = "http://rest.ensembl.org/"
        xrefs    = "xrefs/id/"
        xrefOpts = "?content-type=application/json;all_levels=1"
        xrefReq  = base <> xrefs <> T.unpack query <> xrefOpts

    xrefRsp   <- simpleHTTP (getRequest xrefReq) >>= getResponseBody

    let ensemblDesc = decodeEnsemblDesc field . CL.pack $ xrefRsp

    return ensemblDesc
