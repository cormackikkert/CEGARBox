import Defs
import Clausify
import System.IO
import System.Environment

import MiniSat hiding (simplify)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Prover
import Grammar
import Control.Monad
import Options
import Debug.Trace 

stmtToFormula :: Fml -> Formula
stmtToFormula PTrue = TRUE
stmtToFormula PFalse = FALSE
stmtToFormula (PNot x) = Not (stmtToFormula x)
stmtToFormula (PDia (Int x) y) = Dia x (stmtToFormula y)
stmtToFormula (PBox (Int x) y) = Box x (stmtToFormula y)
stmtToFormula (PAnd x y) = stmtToFormula x :&: stmtToFormula y
stmtToFormula (POr x y) = stmtToFormula x :|: stmtToFormula y
stmtToFormula (PImplies x y) = (Not $ stmtToFormula x) :|: stmtToFormula y
-- stmtToFormula (PIff x y) = (a :&: b) :|: (Not a :&: Not b) 
--     where 
--         a = stmtToFormula x
--         b = stmtToFormula y
stmtToFormula (PIff x y) = (Not a :|: b) :&: (a :|: Not b) 
    where 
        a = stmtToFormula x
        b = stmtToFormula y
stmtToFormula (PAtom (Var x)) = Atom x

main :: IO()
main = runCommand $ \opts args -> do
    as <- getArgs
    
    hap <- readFile (last as)

    let parsed = (calc . lexer) hap
    
    let formula
            | optValid opts = simplify $ negatedNormalForm $ Not (stmtToFormula parsed)
            | otherwise = simplify $ negatedNormalForm $ stmtToFormula parsed

    counter <- makeCounter
    let producer = uniqueLit counter

    let compressedFormula = boxFlatten $ getSimplification opts $ listFormula formula
    
    modal <- modalise 0 (getType opts) producer compressedFormula (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList []))
    
    res <- case rename of
                NormalRenaming -> return modal
                ReuseRenaming -> removeDupeClauses modal


    -- Do process twice for S4 case
    i1a <- reduceClauses (getType opts) producer res
    let i1b = getModel opts i1a
    i1c <- removeDupeClauses i1b

    i2a <- reduceClauses (getType opts) producer i1c
    let i2b = getModel opts i2a
    i2c <- removeDupeClauses i2b


    result <- getResult opts i2c


    putStrLn (case (result, optValid opts)  of
        (Satisfiable, False) -> "Satisfiable"
        (Unsatisfiable _, False) -> "Unsatisfiable"
        (Satisfiable, _) -> "Invalid"
        (Unsatisfiable _, _) -> "Valid"
        )

    where
        getType opts
            | optReflexiveTransitive opts = 2                       -- S4
            | optReflexive opts = 1                                 -- KT
            | otherwise = 0                                         -- K
        getModel opts res
            | optReflexiveTransitive opts = processS4 res             -- S4
            | optReflexive opts = backpropKT res                      -- KT
            | otherwise = res                                         -- K
        getResult opts res
            | optReflexiveTransitive opts = proveTheoremT res              -- S4
            | optReflexive opts = proveTheoremKT res                       -- KT
            | otherwise = proveTheoremK res                                -- K
        getSimplification opts res 
            | optReflexiveTransitive opts = simplifyS4 res
            | otherwise = res
