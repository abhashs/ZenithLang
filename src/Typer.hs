{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Typer where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, pack, concat)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.Error

import qualified Text.PrettyPrint as Pp
import AstTypes (Type(..), Literal(..), Expr(..))


data Scheme = Scheme [Text] Type

class Types a where
    freeTypeVariables :: a -> Set.Set Text
    apply             :: Subst -> a -> a

instance Types Type where
    freeTypeVariables (TVar n)          = Set.singleton n
    freeTypeVariables StringT           = Set.empty
    freeTypeVariables IntT              = Set.empty
    freeTypeVariables BoolT             = Set.empty
    freeTypeVariables (CustomType _ _)  = Set.empty
    freeTypeVariables (t1 :-> t2)       =
        freeTypeVariables t1 `Set.union` freeTypeVariables t2

    apply s (TVar n)    = case Map.lookup n s of
                            Nothing -> TVar n
                            Just t  -> t
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2
    apply s t           = t

instance Types Scheme where
    freeTypeVariables (Scheme vars t) = freeTypeVariables t `Set.difference` Set.fromList vars
    apply s (Scheme vars t)           = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    freeTypeVariables l = foldr (Set.union . freeTypeVariables) Set.empty l
    apply s             = map (apply s)

--------------------------------------------------------------------------------
-- Substitutions
--------------------------------------------------------------------------------
type Subst = Map.Map Text Type

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

--------------------------------------------------------------------------------
-- Type Environment
--------------------------------------------------------------------------------
newtype TypeEnv = TypeEnv (Map.Map Text Scheme)

remove :: TypeEnv -> Text -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
    freeTypeVariables (TypeEnv env) = freeTypeVariables (Map.elems env)
    apply s (TypeEnv env)           = TypeEnv (Map.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList (freeTypeVariables t `Set.difference` freeTypeVariables env)

--------------------------------------------------------------------------------
data TIEnv = TIEnv{}
data TIState = TIState{ tiSupply :: Int }

type TI a = ExceptT Text (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either Text a, TIState)
runTI t =
  do
    (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    return (res, st)
  where
    initTIEnv = TIEnv
    initTIState = TIState { tiSupply = 0 }


newTyVar :: Text -> TI Type
newTyVar prefix =
  do
    s <- get
    put s{ tiSupply = tiSupply s + 1 }
    return (TVar (prefix <> pack (show (tiSupply s))))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) =
  do
    nvars <- mapM (\ _ -> newTyVar "a") vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

varBind :: Text -> Type -> TI Subst
varBind u t | t == TVar u        = return nullSubst
            | u `Set.member` freeTypeVariables t = throwError $
                "occurs check fails: " <> u <> " vs. " <> pack (show t)
            | otherwise                          = return (Map.singleton u t)

mgu :: Type -> Type -> TI Subst
mgu (l :-> r) (l' :-> r') =
  do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
mgu (TVar u) t                        = varBind u t
mgu t (TVar u)                        = varBind u t
mgu StringT StringT                   = return nullSubst
mgu IntT IntT                         = return nullSubst
mgu BoolT BoolT                       = return nullSubst
mgu (CustomType _ _) (CustomType _ _) = return nullSubst
mgu t1 t2                             = throwError $ "types do not unify: "
                                                   <> pack (show t1)
                                                   <>" vs. "
                                                   <> pack (show t2)


inferLiteral :: Literal -> TI (Subst, Type)
inferLiteral (IntLiteral _)  = return (nullSubst, IntT)
inferLiteral (StrLiteral _)  = return (nullSubst, StringT)
inferLiteral (BoolLiteral _) = return (nullSubst, BoolT)

-- foldExpressions :: [Expr] -> Subst -> (Subst, Type)
-- foldExpressions es headSub = foldr foldExpression 

typeInfer :: TypeEnv -> Expr -> TI(Subst, Type)
typeInfer (TypeEnv env) (IdentifierExpr n) =
    case Map.lookup n env of
      Nothing    -> throwError $ "unbound variable: " <> n
      Just sigma -> do t <- instantiate sigma
                       return (nullSubst, t)
typeInfer _ (LitExpr l) = inferLiteral l
-- TODO: EAbs?

typeInfer env exp@(ApplyExpr e1 e2) =
  do
    tv <- newTyVar "a"
    (s1, t1) <- typeInfer env e1
    (s2, t2) <- typeInfer (apply s1 env) (head e2)
    s3       <- mgu (apply s2 t1) (t2 :-> tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
  `catchError`
  (\e -> throwError $ e <> "\n in " <> pack (show exp))

typeInfer env exp@(BinExpr op e1 e2) =
  do
    tv <- newTyVar "a"
    (s1, t1) <- typeInfer env e1
    (s2, t2) <- typeInfer env e2
    s3   <- mgu t1 t2
    return (s3, t1)
  `catchError`
  (\e -> throwError $ e <> "\n in " <> pack (show exp))

-- typeInfer env exp@(LambdaExpr args e) =
--   do
--     (s1, t1) <- typeInfer env e
--     let TypeEnv env' = remove env x
--         t' = generalize (apply s1 env) t1
--         env'' = TypeEnv (Map.insert 

-- typeInfer env exp@(IfExpr b c1 c2) =
--   do
--     tv <- newTyVar "a"
--     (s1, t1) <- typeInfer env b



typeInference :: Map.Map Text Scheme -> Expr -> TI Type
typeInference env e = 
  do
    (s, t) <- typeInfer (TypeEnv env) e
    return (apply s t)

testTI :: Expr -> IO (Either Text Type)
testTI t = do (res, _) <- runTI (typeInference Map.empty t)
              return res

testTIWithEnv :: Map.Map Text Scheme -> Expr -> IO (Either Text Type)
testTIWithEnv env t = do (res, _) <- runTI (typeInference env t)
                         return res


