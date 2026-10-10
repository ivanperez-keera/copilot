{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module FieldProps where

import Language.Haskell.TH
import Test.QuickCheck

-- | Generate QuickCheck property touching all fields of a datatype
deriveRecordFieldProps :: Name -> Q [Dec]
deriveRecordFieldProps typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      let funName = mkName $ "prop_fields_" ++ nameBase typeName
      x <- newName "x"
      matches <- mapM mkMatch constructors
      let body = LamE [VarP x] (CaseE (VarE x) matches)
      return
        [ SigD funName (AppT (AppT ArrowT (ConT typeName)) (ConT ''Bool))
        , FunD funName [Clause [] (NormalB body) []]
        ]
    _ -> fail "deriveRecordFieldProps: expected a data type"

  where
    mkMatch (RecC conName vars) = do
      -- use fresh variables, don't reuse qualified field names
      patVars <- mapM (\_ -> newName "f") vars
      let uses = map (\v -> InfixE (Just (VarE v)) (VarE '(==)) (Just (VarE v))) patVars
          body = foldr1 (\a b -> InfixE (Just a) (VarE '(&&)) (Just b)) uses
          fields = zipWith (\(fname,_,_) v -> (fname, VarP v)) vars patVars
      return $ Match (RecP conName fields) (NormalB body) []

    mkMatch (NormalC conName types) = do
      patVars <- mapM (\_ -> newName "f") types
      let uses = map (\v -> InfixE (Just (VarE v)) (VarE '(==)) (Just (VarE v))) patVars
          body = foldr1 (\a b -> InfixE (Just a) (VarE '(&&)) (Just b)) uses
      return $ Match (ConP conName (map VarP patVars)) (NormalB body) []

    mkMatch (InfixC t1 conName t2) =
      mkMatch (NormalC conName [t1,t2])
    mkMatch (ForallC _ _ c) =
      mkMatch c
    mkMatch _ =
      fail "Unsupported constructor type"

