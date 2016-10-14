{- HUGOConvert
Gregory W. Schwartz

Collections the functions pertaining to converting certain annotations into
other annotations using the HUGO database.
-}

{-# LANGUAGE OverloadedStrings #-}

module HUGOConvert
    ( toHUGOAnn
    ) where

-- Standard
import Data.Monoid

-- Cabal
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Network.Wreq
import Safe

-- Local
import Types

-- | Get the HUGO annotation.
toHUGOAnn :: HUGOType -> UnknownAnn -> IO (Maybe Ann)
toHUGOAnn _ (UnknownAnn "") = return Nothing
toHUGOAnn (HUGOType queryType) (UnknownAnn query) = do
    let opts = set (header "Accept") ["application/json"] defaults

    r <- getWith opts
       $ "http://rest.genenames.org/search/"
      <> T.unpack queryType
      <> "/"
      <> T.unpack query

    let getSymbols = responseBody
                   . key "response"
                   . key "docs"
                   . _Array
                   . traverse
                   . key "symbol"
                   . _String

    return . fmap Ann . headMay . toListOf getSymbols $ r
