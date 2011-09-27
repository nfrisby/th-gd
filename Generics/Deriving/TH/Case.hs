{-# LANGUAGE ViewPatterns, TypeOperators, TemplateHaskell #-}

module Generics.Deriving.TH.Case (gd_case, varE) where

import Language.Haskell.TH

import Generics.Deriving hiding (conName)
import Generics.Deriving.TH

import qualified Data.Map as Map
import Data.Traversable (forM)






-- | The first argument is a list of case branches, represented as
-- single-parameter lambdas with constructor patterns. The second is the
-- default function to use as the other branch. The third is an injection
-- function (i.e. @return@) applied to the argument and result of the second
-- function.
gd_case :: [Q Exp] -> (Q Exp -> Q Exp) -> Q Exp
gd_case cases defalt = do
  x <- sequence cases
  gd_case' x defalt

gd_case' (mapM open -> Just cases@((con1, _) : _)) defalt = do
  DataConI _ _ ty _ <- reify con1
  TyConI (DataD _ _ tvbs cons _) <- reify ty
  case cons of
    [] -> [| error $(stringE ("gd_case of consructorless datatype " ++ show ty)) |]
    cons ->
      [| $(foldr (\x y inj -> [| $(each x [| $inj . L1 |]) ||| $(y [| $inj . R1 |]) |])
                   (each (last cons))
                   (init cons) [| id |])
       . unM1 . $(varE 'from) . $(return ascribe) |]
      where
        ascribe = SigE (VarE 'id) qty where
            qty = ForallT tvbs [] $ (ArrowT `AppT` ty' `AppT` ty')
            ty' = foldl AppT (ConT ty) $ map tvb_var tvbs
  where
    given_cons = Map.fromList cases
    each con inj = case Map.lookup (conName con) given_cons of
      Just ([], e) -> [| \_ -> $(return e) |]
      Just (ps, e) ->
        [| $(lamE [foldr (\p pat -> conP '(:*:) [eachP p, pat]) (eachP (last ps)) (init ps)]
                    (return e)) . unM1 |]
        where eachP p = return $ ConP 'M1 [ConP 'K1 [p]]
      Nothing -> defalt [| $(varE 'to) . M1 . $inj |]


{-D1 T_ (C1 T_C_ (S1 NoSelector (Rec0 Int))
       :+:
       C1 T_D_ (:*: (S1 NoSelector (Rec0 Int)) (:*: (S1 NoSelector (Rec0 Bool)) (S1 NoSelector (Rec0 Char))))
       :+:
       C1 T_B_ (S1 NoSelector U1))-}


tvb_var (PlainTV n) = VarT n
tvb_var (KindedTV n _) = VarT n



open (LamE [ConP con ps] e) = Just (con, (ps, e)) where
open _ = Nothing



conName (NormalC n _) = n
conName (RecC n _) = n
conName (InfixC _ n _) = n
conName (ForallC _ _ con) = conName con


unM (M1 x) = x

infixr 2 |||

(|||) :: (l p -> x) -> (r p -> x) ->
         (l :+: r) p -> x
(|||) f g x = case x of
  L1 x -> f x
  R1 x -> g x
