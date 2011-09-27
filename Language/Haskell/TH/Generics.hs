{-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls #-}

module Language.Haskell.TH.Generics
  (module Generics.Deriving, module Generics.Deriving.TH) where

import Language.Haskell.TH.Syntax
import Generics.Deriving.TH
import Generics.Deriving

deriveAll ''Exp
deriveAll ''Match
deriveAll ''Clause
deriveAll ''Pat
deriveAll ''Type
deriveAll ''Dec
deriveAll ''Name
deriveAll ''FunDep
deriveAll ''Pred
deriveAll ''TyVarBndr
deriveAll ''Kind
deriveAll ''Con
deriveAll ''Pragma
deriveAll ''Foreign
deriveAll ''FamFlavour
deriveAll ''Range
deriveAll ''Stmt
deriveAll ''Body
deriveAll ''ClassInstance
deriveAll ''Info
deriveAll ''Guard
deriveAll ''(,)
deriveAll ''(,,)
deriveAll ''Strict
