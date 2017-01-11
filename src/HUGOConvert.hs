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
import Control.Exception (throwIO)

-- Cabal
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Network.HTTP.Req
import Safe

-- Local
import Types

instance MonadHttp IO where
    handleHttpException = throwIO

-- | Get the HUGO annotation.
toHUGOAnn :: HUGOType -> UnknownAnn -> IO (Maybe Ann)
toHUGOAnn _ (UnknownAnn "") = return Nothing
toHUGOAnn (HUGOType queryType) (UnknownAnn query) = do
    let opts = header "Accept" "application/json"

    r <- req GET
            (http "rest.genenames.org" /: "search" /: queryType /: query)
            NoReqBody
            jsonResponse
            opts

    let getSymbols = key "response"
                   . key "docs"
                   . _Array
                   . traverse
                   . key "symbol"
                   . _String

    return . fmap Ann . headMay . toListOf getSymbols $ (responseBody r :: Value)
