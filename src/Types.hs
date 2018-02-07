{- Types
Gregory W. Schwartz

Collections the types used in the program.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

-- Standard
import GHC.Generics
import Control.DeepSeq (NFData)

-- Cabal
import qualified Data.Text as T
import Data.Aeson

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

-- Local


-- Algebraic
data Database
    = Ensembl
    | HUGO T.Text
    | UniProt
    | RGene (String, String, String)
    | MSigDBRData (String, String, String)
    deriving (Read,Show)
data DescFields = UniProtOther T.Text
                | Synonyms
                | Description
                  deriving (Read,Show)

-- Basic
newtype File        = File String
newtype UnknownAnn  = UnknownAnn { unUnknownAnn :: T.Text }
newtype Ann         = Ann { unAnn :: T.Text }
                      deriving (NFData)
newtype Desc        = Desc { unDesc :: T.Text }
                      deriving (NFData)
newtype HUGOType    = HUGOType { unHUGOType :: T.Text }
newtype RType       = RType { unRType :: (String, String, String) }
newtype MSigDBType  = MSigDBType { unMSigDBType :: (String, String, String) }
newtype RMart s     = RMart { unRMart :: (R.SomeSEXP s) }
newtype RData s     = RData { unRData :: (R.SomeSEXP s) }

-- Advanced
