{-# LANGUAGE GADTs, DataKinds, KindSignatures, RankNTypes, StandaloneDeriving #-}
module Ex08 where
import Data.List (delete)
import Data.Maybe

data City = Sydney | Shanghai | Seoul | Singapore | Sapporo deriving (Eq, Show, Enum)

allCities :: [City]
allCities = [Sydney ..]

data SCity :: City -> * where
  SSydney    :: SCity Sydney
  SShanghai  :: SCity Shanghai
  SSeoul     :: SCity Seoul
  SSingapore :: SCity Singapore
  SSapporo   :: SCity Sapporo

untype :: SCity s -> City
untype SSydney    = Sydney
untype SShanghai  = Shanghai
untype SSeoul     = Seoul
untype SSingapore = Singapore
untype SSapporo   = Sapporo

withTyped :: (forall c. SCity c -> b) -> City -> b
withTyped f Sydney    = f SSydney
withTyped f Shanghai  = f SShanghai
withTyped f Seoul     = f SSeoul
withTyped f Singapore = f SSingapore
withTyped f Sapporo   = f SSapporo

data Flight :: City -> City -> * where
  NH880 :: Flight Sydney Sapporo
  SQ222 :: Flight Sydney Singapore
  KE122 :: Flight Sydney Seoul
  KE121 :: Flight Seoul Sydney
  SQ825 :: Flight Shanghai Singapore
  SQ231 :: Flight Singapore Sydney
  KE893 :: Flight Seoul Shanghai

deriving instance Show (Flight a b)

data Journey :: City -> City -> * where
  Fly :: Flight a b -> Journey a b
  Connect :: Journey a b -> Journey b c -> Journey a c
--  Connect :: Journey a b -> Journey a b -> Journey a b

deriving instance Show (Journey a b)

flight :: SCity a -> SCity b -> Maybe (Flight a b)
flight SSydney SSapporo = Just NH880
flight SSydney SSingapore = Just SQ222
flight SSydney SSeoul = Just KE122
flight SSeoul SSydney = Just KE121
flight SShanghai SSingapore = Just SQ825
flight SSingapore SSydney = Just SQ231
flight SSeoul SShanghai = Just KE893
flight _ _ = Nothing
        
journeys :: [City] -> SCity a -> SCity b -> [Journey a b]
journeys cs a b =
        let direct = case flight a b of
                           Nothing -> []
                           Just x -> [Fly x]
            indirect = concatMap (\c -> withTyped(\sc -> [Connect f1 f2 | let cs' = delete c cs,f1 <- journeys cs' a sc,f2 <- journeys cs' sc b]) c) cs
            in direct ++ indirect

--[Fly (fromJust (flight a b))] ++ (withTyped (\s -> [Connect (Fly (fromJust (flight a s))) (Fly (fromJust (flight s b)))]) x) ++ (journeys xs a b)
--(withTyped (\s -> s) x)
--journeys cities a b 
--     |isNothing(flight a b) = []
--     |otherwise = [Fly (fromJust (flight a b))]

--journey (x:xs) a b =[Connect (Fly (fromJust (flight a x))) (Fly (fromJust (flight x b)))]

