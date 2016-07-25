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
data EnsemblDescFields = Synonyms | Description deriving (Read, Show)

-- Basic
newtype UnknownAnn  = UnknownAnn { unUnknownAnn :: T.Text }
newtype EnsemblAnn  = EnsemblAnn { unEnsemblAnn :: T.Text }
newtype EnsemblDesc = EnsemblDesc { unEnsemblDesc :: T.Text }

-- Advanced
