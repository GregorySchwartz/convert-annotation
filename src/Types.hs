{- Types
Gregory W. Schwartz

Collections the types used in the program.
-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

-- Standard
import GHC.Generics

-- Cabal
import qualified Data.Text as T
import Data.Aeson

-- Local


-- Algebraic
data Database = Ensembl | UniProt deriving (Read, Show)
data DescFields = UniProtOther T.Text
                | Synonyms
                | Description
                  deriving (Read,Show)

-- Basic
newtype UnknownAnn  = UnknownAnn { unUnknownAnn :: T.Text }
newtype Ann         = Ann { unAnn :: T.Text }
newtype Desc        = Desc { unDesc :: T.Text }

-- Advanced
