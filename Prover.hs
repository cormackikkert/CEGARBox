{-# LANGUAGE TypeOperators #-}
module Prover where

import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set
import qualified Data.Set as TreeSet
import Data.List
import Data.Foldable (toList)
import MiniSat
import Clausify
import Defs
import Data.Maybe
import System.IO
import Debug.Trace
import SupersetDS

-- TODO: addClause bool
-- no-luby
-- set polarity

data Result = Satisfiable | Unsatisfiable [Lit]
    deriving (Show)

data ResultT = SatisfiableT (Int, Cache) | UnsatisfiableT [Lit]
    deriving (Show)
    
getTerms :: [Name] -> Formula -> [Name]
getTerms cur (Atom x) = x : cur
getTerms cur (a:|:b) = getTerms (getTerms cur b) a
getTerms cur (Dia _ a) = getTerms cur a
getTerms cur (Box _ a) = getTerms cur a
getTerms cur (Not a) = getTerms cur a
getTerms cur (Or []) = cur
getTerms cur (Or (x:xs)) = getTerms (getTerms cur x) (Or xs) 
getTerms cur _ = cur

getAtoms :: [Formula] -> Formula -> [Formula]
getAtoms cur (Atom x) = Atom x : cur
getAtoms cur (Not (Atom x)) = Not (Atom x) : cur
getAtoms cur (Or []) = cur
getAtoms cur (Or (x:xs)) = getAtoms (getAtoms cur (Or xs)) x

extractAtoms :: Formula -> [Formula]
extractAtoms (Or fs) = fs
extractAtoms x = [x]

getName :: Formula -> Name
getName (Atom x) = x
getName (Not x) = getName x

data LevelInfo = LevelInfo Solver (Formula -> Lit)

prepareSat :: [Name] -> TrieForm -> IO TrieForm 
prepareSat extra (TrieForm d f1 f2 f3 Nothing _ _ _ trie) = do
    sat <- newSolver
    eliminate sat True -- idk what this does

    let allNames = concatMap (getTerms []) (f1 ++ [l | (_, l :-> _) <- f2] ++ [l | (_, l :-> _) <- f3])

    let names = Set.toList $! Set.fromList ("false":(extra ++ allNames))
    univ <- sequence [newLit sat | _ <- names]
    
    -- sequence_ 
    --     [ 
    --         minisat_setPolarity sat (minisat_var lit) 1 | lit <- names
    --     ]

    let lits = Map.fromList (names `zip` univ)
        lit x = fromJust (Map.lookup x lits)
        flit (Atom x) = lit x
        flit (Not (Atom x)) = neg $ lit x
        allFs = [Atom x | x <- names] ++ [Not (Atom x) | x <- names]
        flitInverses = Map.fromList (map flit allFs `zip` allFs) 
        flitInverse x = fromJust (Map.lookup x flitInverses)
        --- other
        -- lits2 = Map.fromList (univ `zip` univ2)
        -- antimap x = fromJust (Map.lookup x lits2)
    -- sequence_ 
    --     [ print ("Level: " ++ show level ++ show (map flit x))
    --         | x <- a1 `Seq.index` level
    --     ]
    -- print(names `zip` univ)

    sequence_ 
        [ addClause sat (map flit x)
            | x <- map extractAtoms f1
        ]

    addClause sat [neg $ flit (Atom "false")]

    ntf@(TrieForm nd nf1 nf2 nf3 nf _ _  _ nt) <- helper (IntMap.keys trie) (TrieForm d f1 f2 f3 (Just ((sat, initCache), flit, map lit extra, flit, flitInverse)) [] [] [] trie) 

    

    return $! TrieForm nd nf1 nf2 nf3 nf (map (litify ntf) nf2) (map (litify ntf) nf3) (map (litifyDC ntf) nf3) nt

    where
        helper :: [Int] -> TrieForm -> IO TrieForm
        helper [] t = return t
        helper (x:xs) (TrieForm d f1 f2 f3 f _ _ _ t) = do
            ps <- prepareSat (Set.toList $ Set.fromList $ concatMap (getTerms []) ([r | (x, l :-> r) <- f2] ++ [r | (x, l :-> r) <- f3])) (fromJust $ IntMap.lookup x t)
            let nt = IntMap.insert x ps t
            helper xs (TrieForm d f1 f2 f3 f [] [] [] nt)     

        litify :: TrieForm -> (Int, Formula :-> Formula) -> (Int, Lit :-> Lit) 
        litify (TrieForm _ _ _ _ (Just (_, flit1, _, _, _)) _ _ _ trie) (n, f1 :-> f2) = (n, flit1 f1 :-> flit2 f2)
            where 
                TrieForm _ _ _ _ (Just (_, flit2, _, _, _)) _ _ _ _ = fromJust $ IntMap.lookup n trie

        -- for A -> <> B can be satisfied by A -> B
        litifyDC :: TrieForm -> (Int, Formula :-> Formula) -> (Int, Lit :-> Lit) 
        litifyDC (TrieForm _ _ _ _ (Just (_, flit1, _, _, _)) _ _ _ trie) (n, f1 :-> f2) = (n, flit1 f1 :-> flit1 f2)
        -- combineBox :: IO String -> (Formula, Formula :-> Formula, Formula :-> Formula) -> [((Int, Formula), Formula)] -> (Formula, Formula :-> Formula, Formula :-> Formula)
        -- combineBox (f1, f2, f3) [] = (f1, f2, f3)
        -- combineBox (f1, f2, f3) ((n, r), [l]) = return (f1, (n, l :-> r):f2, f3)
        -- combineBox (f1, f2, f3) ((n, r), ls) = do 
        --     nl <- newLit 
        --     return ([negatedNormalForm (Not a) :|: nl | a <- as] ++ nf1, (n, nl :-> r):f2, f3)

initCache = initCache' cacheType
initCache' SubsetCache = TrieCache Leaf
initCache' ExactCache = SetCache (Set.fromList [])

initAssumps = initAssumps' cacheType
initAssumps' SubsetCache = SetAssumps (TreeSet.fromList [])
initAssumps' ExactCache = ListAssumps []

createAssumps = createAssumps' cacheType
createAssumps' SubsetCache x = SetAssumps (TreeSet.fromList x)
createAssumps' ExactCache x = ListAssumps x

assumpsToLit (SetAssumps x) = TreeSet.toList x
assumpsToLit (ListAssumps x) = x

proveTheoremK :: TrieForm -> IO Result
proveTheoremK d = do
    tf <- prepareSat [] d
    (res, tf) <- proverS False 0 initAssumps tf (Map.fromList [])
    -- printCacheData 0 tf
    return res

proveTheoremKT :: TrieForm -> IO Result
proveTheoremKT d = do
    tf <- prepareSat [] d
    (res, ntf) <- proverS True 0 initAssumps tf (Map.fromList [])
    -- printCacheData 0 ntf
    return res

-- modelReuse set_assumps memo
--     | exactCacheType == True = 
--     | otherwise = any (Set.isSubsetOf set_assumps) memo


-- S for Standard
proverS :: Bool -> Int -> Assumps -> TrieForm -> Map.Map [Formula] Int -> IO (Result, (TrieForm, Map.Map [Formula]  Int))
proverS isKT level assumps tf@(TrieForm _ _ _ _ (Just ((sat, memo), _, univ, _, _)) boxes dias _ trie) g = do
    -- sat memo is bogus, use a set instead
    -- optimizations:
        -- Keep checking even after finding offending clauses
        -- Simplify when renaming
        -- Try to reduce the number of clauses  
    -- memo part
    -- insane bug: changing clauses whilst doing recursion messes things up
    -- print(level, assumps, g)
    -- print("EH", assumps)
    -- print("MODEL REUSE", univ, assumps, memo)
    -- print(univ)
    if quickCheck assumps && (if' isKT (modelReuseGlobal level tf assumps g) (modelReuse univ assumps memo)) then  do  --not (IntMap.null trie) && 
        -- print ("Found", assumps, memo)
        return $! (Satisfiable, (tf, g))
    else do 
        -- if not (IntMap.null trie) then 
        --     print "Not found"
        -- else return ()
        -- print ("WA", level, assumpsToLit assumps)

        res <- solve sat (assumpsToLit assumps) -- is satisfiable

        if res then do
            valsB <- sequence [modelValueBool sat l | (n, l :-> r) <- boxes]
            valsD <- sequence [modelValueBool sat l | (n, l :-> r) <- dias]
            
            -- let allNums = [1] -- (optimization) Set.toList $ Set.fromList $ [n | (n, _) <- boxes] ++ [n | (n, _) <- dias]
            
            let trigBox = [(n, l :-> r) | ((n, l :-> r), True) <- boxes `zip` valsB]
            let trigDia = [(n, l :-> r) | ((n, l :-> r), True) <- dias `zip` valsD]
            

            
            if subsetOpt then do
                let tdias = Set.fromList [r | (_, _ :-> r) <- trigDia]
                let tboxs = Set.fromList [r | (_, _ :-> r) <- trigBox]

                if (tdias `Set.isSubsetOf` tboxs) && not (Set.null tdias) then do
                    -- print("1 world")
                    -- create 1 world that satisfies everything     
                    let trigDia' = head trigDia -- fromJust $ Set.lookupMin trigDia
                    let (_, _ :-> actualDia) = trigDia'
                    let trigBox' = filter (\(n, l :-> r) -> r /= actualDia) trigBox

                    let allDia = [eval && (x == trigDia') | (x, eval) <- dias `zip` valsD]

                    -- print(dias, valsD, allDia)

                    let trigBox'' = squashFast trigBox' -- squash allNums (Set.toList trigBox')
                    let trigDia'' = squashFast [trigDia'] -- squash allNums [trigDia']

                    let dict = [Map.fromList [(r, l) | (l :-> r) <- a ++ b] | ((_, a), (_, b)) <- trigDia'' `zip` trigBox'']

                    let compressbox = processBoxes [(n, [r | l :-> r <- lst]) | (n,lst) <- trigBox'']
                    let compressdia = [(n, [r | l :-> r <- lst]) | (n,lst) <- trigDia'']
                    -- print("REE1", dias `zip` allDia)
                    proverHelperS isKT level (createAssumps [r | (n, l :-> r) <- trigBox']) (dias `zip` allDia) dict tf assumps g []
                else do
                    -- print("many worlds", tdias)
                    -- do typical approach
                    -- let diff = trigDia `Set.difference` trigBox
                    let allDia = [eval && (not (Set.member r tboxs)) | ((n, l :-> r), eval) <- dias `zip` valsD]

                    let trigDia' = filter (\(n, l :-> r) -> not (Set.member r tboxs)) trigDia 
                    let trigBox'' = squashFast trigBox -- squash allNums (Set.toList trigBox)  --[(n, l :-> r) | ((n, l :-> r), True) <- boxes `zip` valsB]
                    let trigDia'' = squashFast trigDia' --squash allNums (Set.toList trigDia') --[(n, l :-> r) | ((n, l :-> r), True) <- dias `zip` valsD]

                    let dict = [Map.fromList [(r, l) | (l :-> r) <- a ++ b] | ((_, a), (_, b)) <- trigDia'' `zip` trigBox'']

                    -- let compressbox = processBoxes [(n, [r | l :-> r <- lst]) | (n,lst) <- trigBox'']
                    let compressdia = [(n, [r | l :-> r <- lst]) | (n,lst) <- trigDia'']
                    -- print("REE2", dias `zip` allDia)
                    -- print(trigBox, createAssumps [r | (n, l :-> r) <- trigBox])
                    proverHelperS isKT level (createAssumps [r | (n, l :-> r) <- trigBox]) (dias `zip` allDia) dict tf assumps g []
            else do
                -- print("many worlds", tdias)
                -- do typical approach
                -- let diff = trigDia `Set.difference` trigBox
                let allDia = [eval | ((n, l :-> r), eval) <- dias `zip` valsD]

                -- let trigDia' = filter (\(n, l :-> r) -> not (Set.member r tboxs)) trigDia 
                let trigBox'' = squashFast trigBox -- squash allNums (Set.toList trigBox)  --[(n, l :-> r) | ((n, l :-> r), True) <- boxes `zip` valsB]
                let trigDia'' = squashFast trigDia --squash allNums (Set.toList trigDia') --[(n, l :-> r) | ((n, l :-> r), True) <- dias `zip` valsD]

                let dict = [Map.fromList [(r, l) | (l :-> r) <- a ++ b] | ((_, a), (_, b)) <- trigDia'' `zip` trigBox'']

                -- let compressbox = processBoxes [(n, [r | l :-> r <- lst]) | (n,lst) <- trigBox'']
                let compressdia = [(n, [r | l :-> r <- lst]) | (n,lst) <- trigDia'']
                -- print("REE2", dias `zip` allDia)
                -- print(trigBox, createAssumps [r | (n, l :-> r) <- trigBox])
                proverHelperS isKT level (createAssumps [r | (n, l :-> r) <- trigBox]) (dias `zip` allDia) dict tf assumps g []   
        else do 
            aPrime <- map neg `fmap` conflict sat
            
            return $! (Unsatisfiable aPrime, (tf, g))
    where
        
        
        processBoxes = processBoxes' cacheType
        processBoxes' SubsetCache lst = [(n, SetAssumps (TreeSet.fromList d)) | (n, d) <- lst] 
        processBoxes' ExactCache lst = [(n, ListAssumps d) | (n, d) <- lst]

-- boxes can be in set to make things speedy
proverHelperS :: Bool -> Int -> Assumps -> [((Int, Lit :-> Lit), Bool)] -> [Map.Map Lit Lit] -> TrieForm -> Assumps ->  Map.Map [Formula] Int -> [(Int, Lit :-> Lit)] -> IO (Result, (TrieForm, Map.Map [Formula] Int)) 
proverHelperS False _ _ [] dict (TrieForm u0 u1 u2 u3 (Just ((sat, SetCache memo), u4, univ, np, ni)) u5 u6 u7 trie) (ListAssumps assumps) g nds = do
    {-# SCC "insert" #-} return $! (Satisfiable, (TrieForm u0 u1 u2 u3 (Just ((sat, SetCache $ Set.insert assumps memo), u4, univ, np, ni)) u5 u6 u7 trie, g))

proverHelperS False _ _ [] dict (TrieForm u0 u1 u2 u3 (Just ((sat, TrieCache memo), u4, univ, np, ni)) u5 u6 u7 trie) (SetAssumps assumps) g nds = do
    -- SetCache (K)
    -- print("INSERT 2")
    big_assumps_bool <- sequence [modelValueBool sat v | v <- univ]
    let big_assumps = [if isTrue then v else neg v | (v, isTrue) <- univ `zip` big_assumps_bool]
    -- print("INSERT", big_assumps, univ)
    return $! (Satisfiable, (TrieForm u0 u1 u2 u3 (Just ((sat, TrieCache (memo `trieSetInsert` big_assumps)), u4, univ, np, ni)) u5 u6 u7 trie, g))

-- proverHelperS True _ [] [] dict tf@(TrieForm u0 u1 u2 u3 (Just ((sat, SetCache memo), u4, univ, np)) u5 u6 trie) (ListAssumps assumps) g = do
--     -- ExactCache (K)
--     return (Satisfiable, (repeatInsert assumps tf, g))
--     where
--         repeatInsert :: [Lit] -> TrieForm -> TrieForm
--         repeatInsert assumps (TrieForm u0 u1 u2 u3 (Just ((sat, SetCache memo), u4, univ, np)) u5 u6 trie)
--             | IntMap.null trie = TrieForm u0 u1 u2 u3 (Just ((sat, SetCache $ Set.insert assumps memo), u4, univ, np)) u5 u6 trie
--             | otherwise = TrieForm u0 u1 u2 u3 (Just ((sat, SetCache $ Set.insert assumps memo), u4, univ, np)) u5 u6 (IntMap.insert 1 (repeatInsert assumps (fromJust $! IntMap.lookup 1 trie)) trie)
proverHelperS True level _ [] dict tf@(TrieForm u0 u1 u2 u3 (Just ((sat, SetCache memo), u4, univ, np, ni)) u5 u6 u7 trie) (ListAssumps assumps) g nds = do
    -- print("INSERT 3")
    return $! (Satisfiable, (tf, Map.insert (map ni assumps) level g))


proverHelperS isKT level b ((d, False):ds) (dict:dx) tf@(TrieForm _ _ _ _ (Just ((sat, _), _, _, _, _)) _ _ _ trie) assumps g nds = proverHelperS isKT level b ds (dict:dx) tf assumps g (d:nds)
proverHelperS isKT level b ((fd@(n, a :-> d), True):ds) (dict:dx) tf@(TrieForm u0 u1 u2 u3 (Just ((sat, u4), u5, uu, np, ni)) u6 u7 u8 trie) assumps g nds = do 
    -- print("YOLO")
    (res, (child, ng)) <- proverS isKT (level + 1) (combineAssumps b d) (fromJust $! IntMap.lookup n trie) g
    let ntf = TrieForm u0 u1 u2 u3 (Just ((sat, u4), u5, uu, np, ni)) u6 u7 u8 (IntMap.insert n child trie)
    case res of
        Satisfiable -> do
            -- printCacheData 0 ntf
            proverHelperS isKT level b ds (dict:dx) ntf assumps ng (fd : nds)
        Unsatisfiable fail -> 
            do  
                if d `elem` fail then do
                    -- print "NORMAL CLAUSE"
                    let res = [neg $! fromJust $! Map.lookup x dict | x <- fail] --Set.toList $ Set.fromList [neg $! fromJust $! Map.lookup x dict | x <- fail]
                    addClause sat res
                    
                    -- let htf@(TrieForm za zb zc zd ze zf zg zh zi) = ntf
                    -- let newDias = fd : (nds) ++ [x | (x, _) <- ds]
                    proverS isKT level assumps ntf ng --(TrieForm za zb zc zd ze zf zg zh zi) ng --zg is heuristic
                else do
                    -- print "MEGA BOOM"
                    sequence_ 
                        [ addClause sat ((neg $ u5 dx) : [neg $! fromJust $! Map.lookup x dict | x <- fail]) --here too (SET)
                            | (_, dx :-> _) <- u3
                        ]
                    proverS isKT level assumps ntf ng
    where 
        combineAssumps (ListAssumps b) d = ListAssumps (d:b)
        combineAssumps (SetAssumps b) d = SetAssumps (TreeSet.insert d b)

prepareSatT :: [Name] -> [Formula] -> TrieForm -> IO TrieForm 
prepareSatT extra extraBoxes (TrieForm d f1 f2 f3 Nothing _ _ _ trie) = do
    sat <- newSolver
    eliminate sat True -- idk what this does

    let allNames = concatMap (getTerms []) (extraBoxes ++ f1 ++ [l | (_, l :-> _) <- f2] ++ [l | (_, l :-> _) <- f3] ++ [r | (_, _ :-> r) <- f2] ++ [r | (_, _ :-> r) <- f3])

    let names = Set.toList $! Set.fromList ("false":(extra ++ allNames))
    univ <- sequence [newLit sat | _ <- names]
    let pnames = extraBoxes
    puniv <- sequence [newLit sat | _ <- pnames]
  
    -- univ2 <- sequence [newLit antisat | _ <- names]
    let lits = Map.fromList (names `zip` univ)
        lit x = fromJust (Map.lookup x lits)
        flit (Atom x) = lit x
        flit (Not (Atom x)) = neg $ lit x
        plits = Map.fromList (pnames `zip` puniv) -- ++ (map (neg . flit) pnames `zip` [neg x | x <- puniv]))
        -- lit to persistant lit
        plit x = fromJust (Map.lookup x plits)

    sequence_ 
        [ addClause sat (map flit x)
            | x <- map extractAtoms f1
        ]
    
    sequence_ 
        [ addClause sat [neg (plit x), flit x]
            | x <- pnames
        ]

    addClause sat [neg $ flit (Atom "false")]

    let small_univ = []

    ntf@(TrieForm nd nf1 nf2 nf3 nf _ _ _ nt) <- helper (IntMap.keys trie) (TrieForm d f1 f2 f3 (Just ((sat, initCache), flit, small_univ, plit, undefined)) [] [] [] trie) 

    return $! TrieForm nd nf1 nf2 nf3 nf (map (litifyB ntf) nf2) (map (litifyD ntf) nf3) (map (litifyDC ntf) nf3) nt

    where
        helper :: [Int] -> TrieForm -> IO TrieForm
        helper [] t = return t
        helper (x:xs) (TrieForm d f1 f2 f3 f _ _ _ t) = do
            ps <- prepareSatT (TreeSet.toList $ TreeSet.fromList $ concatMap (getTerms []) [r | (x, l :-> r) <- f3]) (TreeSet.toList (TreeSet.fromList (extraBoxes ++ [r | (x, l :-> r) <- f2]))) (fromJust $ IntMap.lookup x t)
            let nt = IntMap.insert x ps t
            helper xs (TrieForm d f1 f2 f3 f [] [] [] nt)

        -- CAN BE OPTIMIZED
        litifyB :: TrieForm -> (Int, Formula :-> Formula) -> (Int, Lit :-> Lit) 
        litifyB (TrieForm _ _ _ _ (Just (_, flit1, _, plit1, _)) _ _ _ trie) (n, f1 :-> f2) = (n, flit1 f1 :-> plit2 f2)
            where 
                (flit2, plit2) = case IntMap.lookup n trie of 
                    Just (TrieForm _ _ _ _ (Just (_, flit2, _, plit2, _)) _ _ _ _) -> (flit2, plit2)
                    Nothing -> (flit1, plit1)
                
        litifyD :: TrieForm -> (Int, Formula :-> Formula) -> (Int, Lit :-> Lit) 
        litifyD (TrieForm _ _ _ _ (Just (_, flit1, _, plit1, _)) _ _ _ trie) (n, f1 :-> f2) = (n, flit1 f1 :-> flit2 f2)
            where 
                (flit2, plit2) = case IntMap.lookup n trie of 
                    Just (TrieForm _ _ _ _ (Just (_, flit2, _, plit2, _)) _ _ _ _) -> (flit2, plit2)
                    Nothing -> (flit1, plit1)
        
        -- for A -> <> B can be satisfied by A -> B
        litifyDC :: TrieForm -> (Int, Formula :-> Formula) -> (Int, Lit :-> Lit) 
        litifyDC (TrieForm _ _ _ _ (Just (_, flit1, _, plit1, _)) _ _ _ trie) (n, f1 :-> f2) = (n, flit1 f1 :-> flit1 f2)
    

-- T for transitive
proveTheoremT :: TrieForm -> IO Result
proveTheoremT d = do
    tf <- prepareSatT [] [] d
    (res, _, hs) <- proverT 0 [] [] tf (Map.fromList []) initCache (repeat (Map.fromList []))
    return (case res of
            SatisfiableT _ -> Satisfiable
            UnsatisfiableT _ -> Unsatisfiable [] )

proverT :: Int -> [Lit] -> [Lit] -> TrieForm -> Map.Map [Lit] Int -> Cache -> [Map.Map Lit Int] -> IO (ResultT, TrieForm, [Map.Map Lit Int]) 
proverT level box_assumps dia_assump tf@(TrieForm _ f1 f2 f3 (Just ((sat, memo), flit, univ, persistant, inv)) boxes dias diasC trie) history cur_memo (h:hs) = do
    -- as soon as a box is added, clear history
    -- cant have box and dia (as box implies dia)
    -- Change formula to lit
    -- let litToFormula = Map.fromList ([(flit f, f) | f <- dia_assump] ++ [(persistant $ flit f, f) | f <- box_assumps])
    let assumps = dia_assump ++ box_assumps -- map flit dia_assump ++ map (persistant . flit) box_assumps
    let set_assumps = Set.fromList assumps
    let set_box_assumps = Set.fromList box_assumps
    let tassumps = if' exactCacheType (ListAssumps assumps) (SetAssumps $ TreeSet.fromList assumps)
    if modelReuse univ tassumps memo || modelReuse univ tassumps cur_memo then do
        return (SatisfiableT (level, initCache), tf, h:hs)
    else
        case checkBackEdge assumps history of
            Just x ->
                return (x, tf, h:hs) -- found back edge
            Nothing -> do
                -- original code   
                res <- solve sat assumps -- is satisfiable
                if res then do
                    
                    valsB  <- sequence [modelValueBool sat l | (n, l :-> r) <- boxes]
                    valsD  <- sequence [modelValueBool sat l | (n, l :-> r) <- dias]
                    valsD2 <- sequence [modelValueBool sat r | (n, l :-> r) <- diasC] -- this is the problem (fixed)
                   
                    let trigBox = [(n, l :-> r) | ((n, l :-> r), True) <- boxes `zip` valsB, not (r `Set.member` set_box_assumps)] -- not really multimodal
                    let trigDia = [(n, l :-> r) | (((n, l :-> r), True), False) <- (dias `zip` valsD) `zip` valsD2]
                    let newBox = [r | (n, l :-> r) <- trigBox]

                    let keepDia = map (\x -> case x of 
                                    (((n, l :-> r), True), False) -> True
                                    _ -> False) ((dias `zip` valsD) `zip` valsD2)
                
                    -- There is no overlap as box a -> a -> dia a, so dia a is not checked
                    let trigBox'' = squashFastS trigBox
                    let trigDia'' = squashFastS trigDia

                    let dict = Map.fromList ([(r, l) | (l :-> r) <- trigDia'' ++ trigBox''] ++ [(x, x) | x <- box_assumps])


                    let hdias= sortBy (\(a, _) (b, _) -> heuristic h a b) (filter snd (dias `zip` keepDia))
  
                    (ans, TrieForm d f1 f2 f3 nJust nboxes ndias ndiasC ntrie, nh) <- proverHelperT level hdias dict tf box_assumps (newBox ++ box_assumps) dia_assump tassumps (TreeSet.fromList []) history level cur_memo initCache (h:hs) []
                    return (ans, TrieForm d f1 f2 f3 nJust nboxes ndias ndiasC ntrie, nh)
                else do
                    aPrime <- map neg `fmap` conflict sat
                    return (UnsatisfiableT aPrime, tf, h:hs)
    
    where 
        checkBackEdge :: [Lit] -> Map.Map [Lit] Int -> Maybe ResultT
        checkBackEdge assumps cache = case Map.lookup assumps cache of 
            Just x -> Just $ SatisfiableT (x, initCache)
            Nothing -> Nothing 
        

proverHelperT :: Int -> [((Int, Lit :-> Lit), Bool)] -> Map.Map Lit Lit -> TrieForm -> [Lit] -> [Lit] -> [Lit] -> Assumps -> TreeSet.Set Lit -> Map.Map [Lit] Int -> Int -> Cache -> Cache -> [Map.Map Lit Int] -> [(Int, Lit :-> Lit)] -> IO (ResultT, TrieForm, [Map.Map Lit Int]) 
proverHelperT level [] dict (TrieForm d u1 u2 u3 (Just ((sat, memo), u4, univ, p, i)) uf u6 u7 trie) orig_box_assumps box_assumps dia_assump (SetAssumps assumps) big_assumps history min_back_edge orig_memo added_memo (h:hs) ndias =
    -- Subset Cache
    if level == min_back_edge then
        return (SatisfiableT (level, initCache), TrieForm d u1 u2 u3 (Just ((sat, (SetAssumps big_assumps) `insertCache` (combineCache added_memo memo)), u4, univ, p, i)) uf u6 u7 trie, h:hs)
    else
        return (SatisfiableT (min_back_edge, (SetAssumps big_assumps) `insertCache` added_memo), TrieForm d u1 u2 u3 (Just ((sat, memo), u4, univ, p, i)) uf u6 u7 trie, h:hs) 

proverHelperT level [] dict (TrieForm d u1 u2 u3 (Just ((sat, memo), u4, univ, p, i)) uf u6 u7 trie) orig_box_assumps box_assumps dia_assump la@(ListAssumps assumps) big_assumps history min_back_edge orig_memo added_memo (h:hs) ndias =
    -- Exact Cache
    if level == min_back_edge then
        return (SatisfiableT (level, initCache), TrieForm d u1 u2 u3 (Just ((sat, la `insertCache` (combineCache added_memo memo)), u4, univ, p, i)) uf u6 u7 trie, h:hs)
    else
        return (SatisfiableT (min_back_edge, la `insertCache` added_memo), TrieForm d u1 u2 u3 (Just ((sat, memo), u4, univ, p, i)) uf u6 u7 trie, h:hs) 
        
proverHelperT level ((d, False):dd) dict tf orig_box_assumps box_assumps dia_assump tassumps big_assumps history min_back_edge orig_memo added_memo (h:hs) ndias = proverHelperT level dd dict tf orig_box_assumps box_assumps dia_assump tassumps big_assumps history min_back_edge orig_memo added_memo (h:hs) (d:ndias)

proverHelperT level ((dia@(_, _ :-> d), True):dd) dict tf@(TrieForm g u1 u2 u3 (Just ((sat, u4), uf, uu, up, ui)) u6 u7 u8 trie) orig_box_assumps box_assumps dia_assump tassumps@(ListAssumps as) big_assumps history min_back_edge orig_memo added_memo (h:hs) ndias = do
    let isOnEnd = case IntMap.lookup 1 trie of 
                    Just _ -> False
                    Nothing -> True
    
    let childTrie = fromMaybe tf (IntMap.lookup 1 trie)
    let nHistory = case IntMap.lookup 1 trie of
                    Just _ -> Map.fromList []
                    Nothing -> Map.insert as level history
    let nhs = if' isOnEnd (h:hs) hs

    (res, child, nh:nhs) <- proverT (level + 1) box_assumps [d] childTrie nHistory (combineCache added_memo orig_memo) nhs
    let ntf = if' isOnEnd child (TrieForm g u1 u2 u3 (Just ((sat, u4), uf, uu, up, ui)) u6 u7 u8 (IntMap.insert 1 child trie))

    case res of
        SatisfiableT (back_edge, delta_memo) -> proverHelperT level dd dict ntf orig_box_assumps box_assumps dia_assump tassumps big_assumps history (min back_edge min_back_edge) orig_memo (combineCache delta_memo added_memo) (if' isOnEnd (nh:nhs) (h:nh:nhs)) (dia:ndias)
        UnsatisfiableT fail -> 
            do  
                -- if X from box_assumps, leave as persistant
                -- otherwise unmap
                let res = Set.toList $ Set.fromList [neg $ fromJust (Map.lookup x dict) | x <- d:fail]
                addClause sat res
                
                let htf@(TrieForm za zb zc zd ze zf zg zh zi) = ntf
                -- let alldias = (reverse ndias) ++ dia: [x | (x, _) <- dd]
                -- dia : (52902)
                -- ++[dia] (65226)
                proverT level orig_box_assumps dia_assump (TrieForm za zb zc zd ze zf zg zh zi) history orig_memo (if' isOnEnd (Map.Strict.insertWith (+) d 1 nh:nhs) (Map.Strict.insertWith (+) d 1 h:nh:nhs))
     

modelValueBool :: Solver -> Lit -> IO Bool
modelValueBool sat x = (Just True ==) `fmap` modelValue sat x
modelValueBoolF :: Solver -> Lit -> IO Bool
modelValueBoolF sat x = (Just False ==) `fmap` modelValue sat x

squash :: [Int] -> [(Int, Lit :-> Lit)] -> [(Int, [Lit :-> Lit])]
squash nums info = IntMap.toAscList $! flatten (fill nums (IntMap.fromList [])) info

    where 
        flatten map ((x, l:->r):xs) = flatten (IntMap.adjust ((l:->r):) x map) xs
        flatten map [] = map
        
        fill [] map = map
        fill (x:xs) map = case IntMap.lookup x map of 
            Just _ -> map
            Nothing -> IntMap.insert x [] map 

-- uses the fact we are using monomodal logics
squashFast :: [(Int, Lit :-> Lit)] -> [(Int, [Lit :-> Lit])]
squashFast xs = [(1, [a :-> b | (_, a:->b) <- xs])]

squashFastS :: [(Int, Lit :-> Lit)] -> [Lit :-> Lit]
squashFastS xs = [a :-> b | (_, a:->b) <- xs]

squashFastF :: [(Int, Formula :-> Formula)] -> [(Int, [Formula :-> Formula])]
squashFastF xs = [(1, [a :-> b | (_, a:->b) <- xs])]


printCacheData layer (TrieForm u0 u1 u2 u3 (Just ((sat, SetCache memo), u4, univ, _, _)) u5 u6 u7 trie) =
    if not (IntMap.null trie) then do
        print ("Layer: ", layer, memo)    
        printCacheData (layer + 1) (fromJust $ IntMap.lookup 1 trie)
    else
        print ("Layer: ", layer, memo)

printCacheData layer (TrieForm u0 u1 u2 u3 (Just ((sat, ListCache memo), u4, univ, _, _)) u5 u6 u7 trie) =
    if not (IntMap.null trie) then do
        print ("Layer: ", layer, memo)    
        printCacheData (layer + 1) (fromJust $ IntMap.lookup 1 trie)
    else
        print ("Layer: ", layer, memo)

combineCache :: Cache -> Cache -> Cache
combineCache (SetCache s1) (SetCache s2) = SetCache (Set.union s1 s2)
combineCache (ListCache s1) (ListCache s2) = ListCache (s1 ++ s2)

insertCache :: Assumps -> Cache -> Cache
insertCache (ListAssumps assumps) (SetCache cache) = SetCache (Set.insert assumps cache)
insertCache (SetAssumps assumps) (TrieCache cache) = TrieCache (cache `trieSetInsert` TreeSet.toList assumps)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

-- quick check
quickCheck (SetAssumps assumps) = quickCheck' (TreeSet.toList assumps)
quickCheck (ListAssumps assumps) = quickCheck' assumps 
quickCheck' xs = not $ any (\(a, b) -> minisat_negate a == b) (zip xs (tail xs))

modelReuse = modelReuse' cacheType
modelReuse' SubsetCache univ (SetAssumps assumps) (TrieCache memo)
    | TreeSet.null assumps = case memo of 
        Leaf -> False
        _ -> True -- If assumps is emtpy, answer is Yes if TrieSet contains an element
    | otherwise = trieSetFindSuperset memo (TreeSet.toList assumps) univ
modelReuse' ExactCache univ (ListAssumps assumps) (SetCache memo) = assumps `Set.member` memo

modelReuseGlobal level (TrieForm _ _ _ _ (Just (_, _, _, _, inv)) _ _ _ _) (ListAssumps assumps) g = case Map.lookup fassumps g of
    Just glevel -> level >= glevel
    Nothing -> False
    where
        fassumps = map inv assumps
    
heuristic :: Ord a => Map.Map a Int -> (b, a :-> a) -> (b, a :-> a) -> Ordering 
heuristic map (_, _ :-> x) (_, _ :-> y) = case (Map.lookup x map, Map.lookup y map) of
    (Nothing, _) -> GT
    (_, Nothing) -> LT
    (Just a, Just b)
        | a > b -> LT
        | otherwise -> GT
