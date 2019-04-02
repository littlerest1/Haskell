{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}
module Ex06 where

data Format (fmt :: [*])  where
  X :: Format '[]
  L :: String -> Format f -> Format f
  S :: Format f-> Format (String ': f)
  I :: Format f -> Format (Int ': f)

type family FormatArgsThen (fmt :: [*]) (ty :: *) :: *
type instance FormatArgsThen '[]       ty = ty
type instance FormatArgsThen (t ': ts) ty = t  -> FormatArgsThen ts ty  
     

printf :: Format fmt -> FormatArgsThen fmt String
printf fmt = printf' fmt ""
   where
      printf' :: Format fmt -> String -> FormatArgsThen fmt String
      printf' X str = str
      printf' (L s f) str = printf' f (str++s)
      printf' (S f) str = \string -> printf' f (str ++ string);
      printf' (I f) str = \int -> printf' f (str ++ (show int));
      -- add other cases


