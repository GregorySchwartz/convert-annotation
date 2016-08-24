{- convert-annotation
Gregory W. Schwartz

Converts an unknown annotation to Ensembl's annotation.
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
import Convert

-- | Command line arguments
data Options = EnsemblInfo { delimiter        :: Maybe String
                                             <?> "([,] | CHAR) The delimiter of the CSV file."
                           , descriptionField :: Maybe String
                                             <?> "([Description] | Synonyms) The info to retrieve about the identifier. Description provides information about the identifier while synonyms provides alternate identifiers for the same entity. Returns a list of information (delimited by '/') for each match to Ensembl's cross references."
                           , column           :: T.Text
                                             <?> "(COLUMN) The column containing the identifier. Must be an Ensembl id for info."
                           , remove           :: Bool
                                             <?> "Whether to remove empty results (no matches to the database)."
                           }
             | EnsemblAnnotation { delimiter :: Maybe String
                                            <?> "([,] | CHAR) The delimiter of the CSV file."
                                 , column    :: T.Text
                                            <?> "(COLUMN) The column containing the identifier. Must be an Ensembl id for info."
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
convert (EnsemblInfo { descriptionField = df }) =
    fmap (fromMaybe "" . fmap unEnsemblDesc)
        . toEnsemblDesc ( maybe Description read
                        . unHelpful
                        $ df
                        )
        . UnknownAnn
convert (EnsemblAnnotation {})                  =
    fmap (fromMaybe "" . fmap unEnsemblAnn) . toEnsemblAnn . UnknownAnn

main :: IO ()
main = do
    opts <- getRecord "convert-annotation, Gregory W. Schwartz.\
                      \ Converts an unknown annotation to Ensembl's annotation."

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
