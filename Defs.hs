{-# LANGUAGE TypeOperators #-}
module Defs where 
import qualified Data.HashSet as Set
import qualified Data.Set as TreeSet
import MiniSat
import Control.Applicative
import Options
import SupersetDS 
import Data.Hashable
import Foreign.Storable

type Name = String

data Formula
    = Atom Name
    | Formula :&: Formula
    | Formula :|: Formula
    | And [Formula]
    | Or [Formula]
    | Not Formula
    | Box Int Formula
    | Dia Int Formula
    | TRUE
    | FALSE
    | Lit
    deriving ( Eq, Ord)

instance Show Formula where 
    show (Atom a)    = a
    show (p :&: q)   = "(" ++ show p ++ " & " ++ show q ++ ")"
    show (p :|: q)   = "(" ++ show p ++ " | " ++ show q ++ ")"
    show (Not p)     = "~" ++ show p
    show (Box x p)   = "[]" ++ show x ++ " " ++ show p
    show (Dia x p)   = "<>" ++ show x ++ " " ++ show p
    show TRUE        = "$true"
    show FALSE       = "$false"
    show (Or fs)     = "(" ++ orHelper fs ++ ")"
    show (And fs)    = "(" ++ andHelper fs ++ ")"

orHelper [] = ""
orHelper [f] = show f
orHelper (f : fs) = show f ++ " | " ++ orHelper fs

andHelper [] = ""
andHelper [f] = show f
andHelper (f : fs) = show f ++ " & " ++ andHelper fs
        

data a :-> b = a :-> b
    deriving ( Eq, Ord)

instance (Show a, Show b) => Show (a :-> b) where
    show (x :-> y) = "(" ++ show x ++ ") => (" ++ show y ++ ")"

-- RENAME
data Rename = NormalRenaming | ReuseRenaming
rename = ReuseRenaming

reuseRenameType = case rename of
    ReuseRenaming -> True
    _ -> False

subsetOpt = True -- subset thing

-- CACHE

data Caching = ExactCache | SubsetCache
cacheType = ExactCache

exactCacheType = case cacheType of 
        ExactCache -> True
        _ -> False

data Assumps = SetAssumps (TreeSet.Set Lit) | ListAssumps [Lit]
    deriving (Show)
data Cache = SetCache (Set.HashSet [Lit]) | ListCache [Set.HashSet Lit] | TrieCache BinaryTree
    deriving (Show)

-- POLARITY
polarityHigh = True

data MainOptions = MainOptions
    { optReflexive :: Bool
    , optReflexiveTransitive :: Bool
    , optValid :: Bool
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "reflexive" False
            "Handle relations as reflexive"
        <*> simpleOption "reflexive-transitive" False
            "Handle relations as reflexive-transitive"
        <*> simpleOption "valid" False
            "Handle relations as transitive"
            
instance Hashable Lit where
    hashWithSalt s lit
        | minisat_sign lit = case minisat_var lit of
            MkVar x -> s - fromIntegral (toInteger x)
        | otherwise = case minisat_var lit of
            MkVar x -> s + fromIntegral (toInteger x)
