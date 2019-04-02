--{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
module Ex09 where
--deriving instance Show (LessEq a b)
data Nat = Z | S Nat 

data SNat :: Nat -> * where
  SZero :: SNat Z
  SSucc :: SNat n -> SNat (S n)

data LessEq :: Nat -> Nat -> * where
  ZLEN :: LessEq Z n
  SLES :: LessEq n m -> LessEq (S n) (S m)

reflexivity :: SNat n -> LessEq n n
reflexivity SZero = ZLEN
reflexivity (SSucc n) = SLES (reflexivity n)
--reflexivity _
--          |_ == SZero = ZLEN _
  --        |otherwise = reflexivity (SSucc n) 

--reflexivity _ = error "'reflexivity' unimplemented"

transitivity :: LessEq a b -> LessEq b c -> LessEq a c
transitivity ZLEN ZLEN = ZLEN
transitivity ZLEN (SLES n) = ZLEN
transitivity (SLES n) (SLES m) = SLES (transitivity n m)
--transivivity (SLES n) (SLES m) = error ""
--transitivity _ _ = error "'transitivity' unimplemented"


data Equal :: Nat -> Nat -> * where
  EqRefl :: Equal n n

antisymmetry :: LessEq a b -> LessEq b a -> Equal a b
antisymmetry ZLEN ZLEN = EqRefl
antisymmetry (SLES n) (SLES m) = 
        case antisymmetry n m of
            EqRefl -> EqRefl

--antisymmetry _ _ = error "'antisymmetry' unimplemented"

