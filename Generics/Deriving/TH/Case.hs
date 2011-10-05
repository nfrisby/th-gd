{-# LANGUAGE ViewPatterns, TypeOperators, TemplateHaskell #-}

module Generics.Deriving.TH.Case (conCase, varE) where

import Language.Haskell.TH

import Generics.Deriving hiding (conName)

import qualified Data.Map as Map

import qualified Control.Arrow as Arrow





-- | The first argument is a list of case branches, represented as
-- single-parameter lambdas with constructor patterns. The second is the
-- default function to use as the other branch. The third is an injection
-- function (i.e. @return@) applied to the argument and result of the second
-- function.
conCase :: Q Exp -> (Q Exp -> Q Exp) -> Q Exp
conCase cas defalt = do
  x <- cas
  conCase' x defalt

conCase' (CaseE disc (mapM open -> Just cases@((con1, _) : _))) defalt = do
  DataConI _ _ ty _ <- reify con1
  TyConI (DataD _ _ tvbs cons _) <- reify ty
  (`AppE` disc) `fmap` case cons of
    [] -> [| error $(stringE ("conCase of consructorless datatype " ++ show ty)) |]
    cons ->
      [| $(foldr (\x y inj -> [| $(each ascribe x [| $inj . L1 |]) ||| $(y [| $inj . R1 |]) |])
                   (each ascribe (last cons))
                   (init cons) [| id |])
       . unM1 . $(varE 'from) . $ascribe |]
      where
        ascribe = return $ SigE (VarE 'id) qty where
            qty = ForallT tvbs [] $ (ArrowT `AppT` ty' `AppT` ty')
            ty' = foldl AppT (ConT ty) $ map tvb_var tvbs
  where
    given_cons = Map.fromList cases
    each ascribe con inj = case Map.lookup (conName con) given_cons of
      Just (Branch ps wrap, e)
        | null ps -> return $ LamE [wrap WildP] e
        | otherwise -> newName "x" >>= \n ->
        lamE [AsP n `fmap` conP 'M1 [foldr (\p pat -> conP '(:*:) [eachP p, pat]) (eachP (last ps)) (init ps)]]
               (letE [valD (wrap `fmap` wildP) (normalB [| $ascribe $ $(varE 'to) $ M1 $ $inj $(varE n) |]) []]
                     (return e))
        where eachP p = return $ ConP 'M1 [ConP 'K1 [p]]
      Nothing -> defalt [| $(varE 'to) . M1 . $inj |]
conCase' _ _ = fail "the first argument must be a case where each branch is a simple constructor pattern (no guards, no where clauses)"



tvb_var (PlainTV n) = VarT n
tvb_var (KindedTV n _) = VarT n



data Branch = Branch [Pat] (Pat -> Pat)

open (Match pat (NormalB e) []) = (\(con, bra) -> (con, (bra, e))) `fmap` w pat where
  w (ConP con ps) = Just (con, Branch ps id)
  w (InfixP p1 con p2) = Just (con, Branch [p1, p2] id)
--  w (RecP n fps) = (con, FieldPats fps)
--  w (BangP pat) = under BangP $ w pat
  w (SigP pat ty) = under (flip SigP ty) $ w pat
  w (AsP n pat) = under (AsP n) $ w pat
  w _ = Nothing

  under f = fmap (Arrow.second $ \(Branch ps g) -> Branch ps (f . g))
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
