{-# LANGUAGE OverloadedStrings #-}
module TypeChecker where

import Data.Text (Text, pack)
import AstTypes hiding (Type, TVar, TCon, (:->))
import Data.List (union, nub, intersect)


--------------------------------------------------------------------------------
-- Kinds
--------------------------------------------------------------------------------

data Kind = Star | Kfn Kind Kind
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
infixr 2 :->
data Type
    = TVar TypeVar
    | TCon TypeConstructor
    | Type :-> Type
    | TGen Int
    deriving (Eq, Show)

data TypeVar = TypeVar Text Kind
    deriving (Eq, Show)

data TypeConstructor = TypeConstructor Text Kind
    deriving (Eq, Show)

tUnit   = TCon (TypeConstructor "()" Star)
tString = TCon (TypeConstructor "String" Star)
tNum    = TCon (TypeConstructor "Number" Star)

tList   = TCon (TypeConstructor "[]" (Kfn Star Star))
tArrow  = TCon (TypeConstructor "(->)" (Kfn Star (Kfn Star Star)))


class HasKind t where
    kind :: t -> Kind
instance HasKind TypeVar where
    kind (TypeVar v k) = k
instance HasKind TypeConstructor where
    kind (TypeConstructor v k) = k
instance HasKind Type where
    kind (TCon tc) = kind tc
    kind (TVar u) = kind u
    kind (t :-> _) = case kind t of
        (Kfn _ k) -> k
        Star -> Star
    kind (TGen _) = Star

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------
type Substitution = [(TypeVar, Type)]

nullSub :: Substitution
nullSub = []

(|->) :: TypeVar -> Type -> Substitution
u |-> t = [(u, t)]

class Types t where
    apply    :: Substitution -> t -> t
    typeVars :: t -> [TypeVar]

instance Types Type where
    apply s (TVar u)  = case lookup u s of
                          Just t -> t
                          Nothing -> TVar u
    apply s (l :-> r) = apply s l :-> apply s r
    apply s t         = t

    typeVars (TVar u)  = [u]
    typeVars (l :-> r) = typeVars l `union` typeVars r
    typeVars t         = []

instance Types a => Types [a] where
    apply s = map (apply s)
    typeVars = nub . concatMap typeVars

infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: (Monad m, MonadFail m) => Substitution -> Substitution -> m Substitution
merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
  where
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                (map fst s1 `intersect` map fst s2)


--------------------------------------------------------------------------------
-- Unify
--------------------------------------------------------------------------------
varBind :: (Monad m, MonadFail m) => TypeVar -> Type -> m Substitution
varBind u t | t == TVar u           = return nullSub
            | u `elem` typeVars t   = fail "occurs check fails" 
            | kind u /= kind t      = fail "kinds do not match"
            | otherwise             = return (u |-> t)


mgu :: (Monad m, MonadFail m) => Type -> Type -> m Substitution
mgu (l :-> r) (l' :-> r') = do
    s1 <- mgu l' r'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 @@ s1)
mgu (TVar u) t            = varBind u t
mgu t (TVar u)            = varBind u t
mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2                       = return nullSub
mgu t1 t2                 = fail "types do not unify"

match :: (Monad m, MonadFail m) => Type -> Type -> m Substitution
match (l :-> r) (l' :-> r')         = do sl <- match l l'
                                         sr <- match r r'
                                         merge sl sr
match (TVar u) t | kind u == kind t = return (u |-> t)
match (TCon tc1) (TCon tc2)
                 | tc1 == tc2       = return nullSub
match t1 t2                         = fail "types do not match"


--------------------------------------------------------------------------------
-- Type Schemes
--------------------------------------------------------------------------------



