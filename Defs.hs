{-# LANGUAGE TypeOperators #-}
module Defs where 
import qualified Data.Set as Set
import MiniSat
import Control.Applicative
import Options

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

-- CACHE

data Caching = ExactCache | SubsetCache
cacheType = ExactCache

exactCacheType = case cacheType of 
        ExactCache -> True
        _ -> False

data Assumps = SetAssumps (Set.Set Lit) | ListAssumps [Lit]
    deriving (Show)
data Cache = SetCache (Set.Set [Lit]) | ListCache [Set.Set Lit]
    deriving (Show)

-- POLARITY
polarityHigh = True

data MainOptions = MainOptions
    { optReflexive :: Bool
    , optTransitive :: Bool
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "reflexive" False
            "Handle relations as reflexive"
        <*> simpleOption "transitive" False
            "Handle relations as transitive"
            