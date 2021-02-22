{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiWayIf #-}
module Clausify where

import Defs
import Data.IORef
import Data.Maybe
import Data.List
import Data.Foldable
import MiniSat hiding ( simplify )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad
import Debug.Trace
-- import Data.ListTrie.Map (TrieMap)
-- import qualified Data.ListTrie.Map as Trie
import System.IO.Unsafe
globalRename :: IORef (Map.Map Formula (Int, Formula))
{-# NOINLINE globalRename #-}
globalRename = unsafePerformIO (newIORef (Map.fromList []))


{- Getting a counter to work -}
type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
    r <- newIORef 0
    return (\i -> do modifyIORef r (+i)
                     readIORef r)

uniqueLit :: Counter -> IO String
uniqueLit counter = do
    res <- counter 1
    return ("x" ++ show res)

testCounter :: Counter -> IO ()
testCounter counter = do
    let producer = uniqueLit counter
    b <- producer
    c <- producer
    d <- producer
    print [b,c,d]

-- order of stages is no longer valid
------------------------- STAGE 1: Normal FORM -------------------------------------
negatedNormalForm :: Formula -> Formula
negatedNormalForm formula = case formula of 
    Not inside -> case inside of
        Atom x -> Not inside
        a :&: b -> negatedNormalForm (Not a) :|: negatedNormalForm (Not b)
        a :|: b -> negatedNormalForm (Not a) :&: negatedNormalForm (Not b)
        Not x -> negatedNormalForm x
        Box n x -> Dia n (negatedNormalForm (Not x))
        Dia n x -> Box n (negatedNormalForm (Not x))
        TRUE -> FALSE
        FALSE -> TRUE
        Or fs -> And (map (negatedNormalForm . Not) fs)
        And fs -> Or (map (negatedNormalForm . Not) fs)

    Atom x -> formula
    a :&: b -> negatedNormalForm a :&: negatedNormalForm b
    a :|: b -> negatedNormalForm a :|: negatedNormalForm b
    Or fs -> Or (map negatedNormalForm fs)
    And fs -> And (map negatedNormalForm fs)
    Box n x -> Box n (negatedNormalForm x)
    Dia n x -> Dia n (negatedNormalForm x)
    TRUE -> TRUE
    FALSE -> FALSE

------------------------- Sort ---------------------------------------------------
sortFormula :: Formula -> Formula
sortFormula (Not f) = Not $ sortFormula f
sortFormula (Box n x) = Box n (sortFormula x)
sortFormula (Dia n x) = Dia n (sortFormula x)
sortFormula (a :&: b) 
    | x < y = x :&: y
    | otherwise = y :&: x
    where 
        x = sortFormula a
        y = sortFormula b
sortFormula (a :|: b)
    | x < y = x :|: y
    | otherwise = y :|: x
    where
        x = sortFormula a
        y = sortFormula b
sortFormula (Or fs)  = Or  (sort $ map sortFormula fs)
sortFormula (And fs) = And (sort $ map sortFormula fs)
sortFormula other = other

------------------------- STAGE 2: SIMPLIFY -------------------------------------
simplify :: Formula -> Formula

simplify (p :&: q) = simplify p .&. simplify q
 where
  p .&. q | p == q = p
  TRUE  .&. q     = q
  p     .&. TRUE  = p
  FALSE .&. q     = FALSE
  p     .&. FALSE = FALSE
  p     .&. q     = p :&: q

simplify (p :|: q) = simplify p .|. simplify q
 where
  p .|. q | p == q = p
  TRUE  .|. q     = TRUE
  p     .|. TRUE  = TRUE
  FALSE .|. q     = q
  p     .|. FALSE = p
  p     .|. q     = p :|: q

simplify (Box n TRUE) = TRUE
simplify (Dia n FALSE) = FALSE
simplify (Box x p) = Box x $ simplify p 
simplify (Dia x p) = Dia x $ simplify p
simplify (Not p) = Not $ simplify p
simplify (Or xs) = Or $ map simplify xs
simplify (And xs) = And $ map simplify xs
simplify p = p

simplifyS4 :: Formula -> Formula
simplifyS4 (Box n (Box m x)) = simplifyS4 (Box n x)
simplifyS4 (Dia n (Dia m x)) = simplifyS4 (Dia n x)
simplifyS4 (Box n (Dia m (Box l (Dia k x)))) = simplifyS4 (Box n (Dia n x))
simplifyS4 (Box n x) = Box n $ simplifyS4 x
simplifyS4 (Dia n x) = Dia n $ simplifyS4 x
simplifyS4 (Not p) = Not $ simplifyS4 p
simplifyS4 (Or xs) = Or $ map simplifyS4 xs
simplifyS4 (And xs) = And $ map simplifyS4 xs
simplifyS4 p = p

boxFlatten :: Formula -> Formula
boxFlatten (Box n x) = Box n (boxFlatten x)
boxFlatten (Dia n x) = Dia n (boxFlatten x)
boxFlatten (Not x) = Not (boxFlatten x)
boxFlatten (Or xs)
    | null nxs = Dia 1 (Or comb)
    | length comb >= 2 = Or (Dia 1 (Or comb) : nxs)
    | length comb == 1 = Or (Dia 1 (head comb) : nxs)
    | otherwise = Or nxs
    where
        comb = getDia xs
        nxs = getDiaRest xs
boxFlatten (And xs)
    | null nxs = Box 1 (And comb)
    | length comb >= 2 = And (Box 1 (And comb) : nxs)
    | length comb == 1 = And (Box 1 (head comb) : nxs)
    | otherwise = And nxs
    where
        comb = getBox xs
        nxs = getBoxRest xs
boxFlatten p = p

getDia :: [Formula] -> [Formula]
getDia [] = []
getDia (Dia n (Or xs):fs) = xs ++ getDia fs
getDia (Dia n x:fs) = x : getDia fs
getDia (f:fs) = getDia fs

getDiaRest :: [Formula] -> [Formula]
getDiaRest [] = []
getDiaRest (Dia n x:fs) = getDiaRest fs
getDiaRest (f:fs) = f : getDiaRest fs

getBox :: [Formula] -> [Formula]
getBox [] = []
getBox (Box n (And xs):fs) = xs ++ getBox fs
getBox (Box n x:fs) = x : getBox fs
getBox (f:fs) = getBox fs

getBoxRest :: [Formula] -> [Formula]
getBoxRest [] = []
getBoxRest (Box n x:fs) = getBoxRest fs
getBoxRest (f:fs) = f : getBoxRest fs

------------------------- STAGE 3: COMPRESS -------------------------------------

-- eg (a & b) & (c & d) -> [a, b, c, d]
removeAnds :: Formula -> [Formula]
removeAnds (And fs) = fs
removeAnds other = [other]

removeAndsO :: Formula -> [Formula]
removeAndsO = removeAndsO' []

removeAndsO' :: [Formula] -> Formula -> [Formula]
removeAndsO' fs (a :&: b)= removeAndsO' (removeAndsO' fs a) b
removeAndsO' fs f = f:fs

removeOrs :: Formula -> [Formula]
removeOrs = removeOrs' []

removeOrs' :: [Formula] -> Formula -> [Formula]
removeOrs' fs (a :|: b)= removeOrs' (removeOrs' fs a) b
removeOrs' fs f = f:fs

-- use lists for conjunction/disjunction
listFormula :: Formula -> Formula
listFormula f@(Atom _) = f
listFormula f@(Not _) = f
listFormula (Box n f) = Box n (listFormula f)
listFormula (Dia n f) = Dia n (listFormula f) 
listFormula (a :&: b) = And $ map listFormula (removeAndsO (a :&: b))
listFormula (a :|: b) = Or  $ map listFormula (removeOrs  (a :|: b))
-- listFormula (And fs)  = And $ map listFormula fs
-- listFormula (Or fs)   = Or  $ map listFormula fs
listFormula x = x

------------------------- STAGE 4: Normal Form -------------------------------------
data TrieForm = TrieForm (Map.Map Formula Formula) [Formula] [(Int, Formula :-> Formula)] [(Int, Formula :-> Formula)] (Maybe ((Solver, Cache), Formula -> Lit, [Lit], Formula -> Lit, Lit -> Formula)) [(Int, Lit :-> Lit)] [(Int, Lit :-> Lit)] [(Int, Lit :-> Lit)] (IntMap.IntMap TrieForm)

drawTrie :: String -> TrieForm -> IO ()
drawTrie pref (TrieForm _ f1 f2 f3 _ _ _ _ trie) = do
    mapM_ (putStrLn . nicef1) f1
    mapM_ (putStrLn . nicef2) f2
    mapM_ (putStrLn . nicef3) f3
    sequence_ 
        [ drawTrie (pref ++ "[]") (fromJust (IntMap.lookup n trie)) 
        | n <- IntMap.keys trie  ]
    where
        nicef1 x = pref ++ show x
        nicef2 (n, a :-> b) = pref ++ show a ++ " => []" ++ show n ++ " " ++ show b
        nicef3 (n, a :-> b) = pref ++ show a ++ " => <>" ++ show n ++ " " ++ show b

data ReduceOutput = ReduceOutput Formula TrieForm

-- data Ftypes = Ftypes [Formula] [Formula :-> Formula] [Formula :-> Formula]
-- modalise :: IO String -> Formula -> TrieForm -> IO (TrieForm)
-- modalise g f t = modalise' g f t

-- Modalise, but bottom up
-- modaliseBU :: Formula -> TrieForm -> IO TrieForm
-- modaliseBU formula tf = do
--     ReduceOutput reduced (TrieForm data f1 f2 f3 s [] [] t) <- reduce formula t
--     return $ TrieForm data (reduced:f1) f2 f3 s [] [] t
--     where 
--         reduce :: Formula -> TrieForm -> IO ReduceOutput
--         reduce (Or fs) tf = do
--             let nfs <- map 

modalise :: Int -> Int -> IO String -> Formula -> TrieForm -> IO (TrieForm)
modalise':: Int -> Int -> IO String -> Formula -> TrieForm -> IO (TrieForm)
modalise level isReflexive generateLit (Box x formula) t@(TrieForm d f1 f2 f3 s _ _ _ trie) = case IntMap.lookup x trie of 
    Just child -> do
        tf <- modalise (level + 1) isReflexive generateLit formula child
        let nt = IntMap.insert x tf trie
        return $ TrieForm d f1 f2 f3 s [] [] [] nt
    Nothing -> do  
        modalise level isReflexive generateLit (Box x formula) (TrieForm d f1 f2 f3 Nothing [] [] [] (IntMap.insert x (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) trie))

modalise level isReflexive generateLit (Dia n x) t@(TrieForm d f1 f2 f3 s [] [] [] trie)
    | isPrimitive x = do
        let a1 = TRUE
        let t2 = IntMap.insert n (IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie) trie -- make sure it exists
        return $ TrieForm d f1 f2 ((n, a1 :-> x):f3) s [] [] [] t2
    | otherwise = do
        -- make sure child exists   
        let basec = IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie
        
        (a2, nbasec, seen) <- renameFormula level isReflexive generateLit basec x
        let a1 = TRUE
        
        if seen then 
            return $ TrieForm d f1 f2 ((n, a1 :-> a2):f3) s [] [] [] trie
        else do
            nc <- repeatedModalise (level+1) isReflexive generateLit (map (\y -> case y of 
                Or xs -> Or ((negatedNormalForm $ Not a2):xs)
                other -> Or [negatedNormalForm $ Not a2, other]) (removeAnds x)) nbasec 
                
            let t3 = IntMap.insert n nc trie
            return $ TrieForm d f1 f2 ((n, a1 :-> a2):f3) s [] [] [] t3

modalise level isReflexive generateLit TRUE t = return t
modalise level isReflexive generateLit (And []) t = return t
modalise level isReflexive generateLit (And (f:fs)) t = do
    t1 <- modalise level isReflexive generateLit f t
    modalise level isReflexive generateLit (And fs) t1

-- ==================== case bash ==================
modalise level isReflexive generateLit formula@(Or [Box n b, a]) tf
    | isPrimitive a = modalise level isReflexive generateLit (Or [a, Box n b]) tf
    | otherwise = modalise' level isReflexive generateLit formula tf
modalise level isReflexive generateLit formula@(Or [a, Box n b]) tf@(TrieForm d f1 f2 f3 s _ _ _ trie)
    | isPrimitive a && isPrimitive b = do
        -- make sure child exists
        return $ TrieForm d f1 ((n, negatedNormalForm (Not a) :-> b):f2) f3 s [] [] [] (IntMap.insert n (IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie) trie)
    | isPrimitive a = do
        -- make sure child exists
        let basec = IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie
        
        (a2, nbasec, seen) <- renameFormula (level+1) isReflexive generateLit basec b
        
        if seen then do
            return $ TrieForm d f1 ((n, negatedNormalForm (Not a) :-> a2):f2) f3 s [] [] [] trie
        else do
            nc <- foldM (\trie form -> modalise (level + 1) isReflexive generateLit form trie) nbasec (map (\y -> case y of 
                Or xs -> Or ((negatedNormalForm $ Not a2):xs)
                other -> Or [negatedNormalForm $ Not a2, other]) (removeAnds b))

            let t3 = IntMap.insert n nc trie
            return $ TrieForm d f1 ((n, negatedNormalForm (Not a) :-> a2):f2) f3 s [] [] [] t3
    | otherwise = modalise' level isReflexive generateLit formula tf

modalise level isReflexive generateLit formula@(Or [Dia n b, a]) tf 
    | isPrimitive a = modalise level isReflexive generateLit (Or [a, Dia n b]) tf
    | otherwise = modalise' level isReflexive generateLit formula tf
modalise level isReflexive generateLit formula@(Or [a, Dia n b]) tf@(TrieForm d f1 f2 f3 s _ _ _ trie)
    | isPrimitive a && isPrimitive b =  do
        -- make sure child exists
        return $ TrieForm d f1 f2 ((n, negatedNormalForm (Not a) :-> b):f3) s [] [] [] (IntMap.insert n (IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie) trie)
    | isPrimitive a = do
        -- make sure child exists 
        let basec = IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie
        
        (a2, nbasec, seen) <- renameFormula (level+1) isReflexive generateLit basec b
        
        if seen then do
            return $ TrieForm d f1 f2 ((n, negatedNormalForm (Not a) :-> a2):f3) s [] [] [] trie
        else do
            nc <- foldM (\trie form -> modalise (level+1) isReflexive generateLit form trie) nbasec (map (\y -> case y of 
                Or xs -> Or ((negatedNormalForm $ Not a2):xs)
                other -> Or [negatedNormalForm $ Not a2, other]) (removeAnds b))

            let t3 = IntMap.insert n nc trie
            return $ TrieForm d f1 f2 ((n, negatedNormalForm (Not a) :-> a2):f3) s [] [] [] t3
    | otherwise = modalise' level isReflexive generateLit formula tf


modalise level isReflexive generateLit formula t = modalise' level isReflexive generateLit formula t
modalise' level isReflexive generateLit formula t@(TrieForm d f1 f2 f3 s _ _ _ trie) = do
    ReduceOutput r (TrieForm d f1 f2 f3 s [] [] [] t) <- reduce generateLit formula t -- should be in type 1 form now 
    return $ TrieForm d (r:f1) f2 f3 s [] [] [] t

    where 
        reduceChain :: IO String -> Formula -> ReduceOutput -> IO ReduceOutput
        reduceChain generateLit f (ReduceOutput (Or cur) t) = do
            ReduceOutput nf nt <- reduce generateLit f t
            return $ ReduceOutput (Or $ nf:cur) nt

        reduce :: IO String -> Formula -> TrieForm -> IO ReduceOutput
        reduce generateLit formula t@(TrieForm d f1 f2 f3 s [] [] [] trie) = case formula of 
            FALSE -> return $ ReduceOutput FALSE t
            TRUE -> return $ ReduceOutput TRUE t
            Atom a -> return $ ReduceOutput (Atom a) t
            Not a -> do
                ReduceOutput x u <- reduce generateLit a t
                return $ ReduceOutput (Not x) u
            Or fs -> do
                ReduceOutput (Or final) ft <- foldrM (reduceChain generateLit) (ReduceOutput (Or []) t) fs
                return $ ReduceOutput (Or final) ft
            And fs -> do
                (a1, nt, seen) <- renameFormula level isReflexive generateLit t (And fs)

                if seen then do
                    return $ ReduceOutput a1 nt
                else do
                    ft <- repeatedModalise level isReflexive generateLit (map (\y -> case y of 
                        Or xs -> Or ((negatedNormalForm $ Not a1):xs)
                        other -> Or [negatedNormalForm $ Not a1, other]) (removeAnds (And fs))) nt 
                    return $ ReduceOutput a1 ft    
            Box n x -> do
                let t2 = IntMap.insert n (IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie) trie -- make sure it exists
                
                if 
                    | isPrimitive x -> do
                        (a1, TrieForm nd _ _ _ _ _ _ _ _, seen) <- renameFormula level isReflexive generateLit t (Box n x)
                        return $ ReduceOutput a1 (TrieForm nd f1 ((n, a1 :-> x):f2) f3 s [] [] [] t2)
                    | otherwise -> do
                        (a1, nt@(TrieForm nd _ _ _ _ _ _ _ _), seen) <- renameFormula level isReflexive generateLit t (Box n x)

                        if seen then do
                            return $ ReduceOutput a1 nt
                        else do

                            (a2, basec, seen2) <- renameFormula (level+1) isReflexive generateLit (fromJust $ IntMap.lookup n t2) x

                            if seen2 then do
                                let t3 = IntMap.insert n basec t2
                                return $ ReduceOutput a1 (TrieForm nd f1 ((n, a1 :-> a2):f2) f3 s [] [] [] t3)
                            else do
                                nc <- repeatedModalise (level+1) isReflexive generateLit (map (\y -> case y of 
                                                                    Or xs -> Or ((negatedNormalForm $ Not a2):xs)
                                                                    other -> Or [negatedNormalForm $ Not a2, other]) (removeAnds x)) basec 

                                let t3 = IntMap.insert n nc t2
                                return $ ReduceOutput a1 (TrieForm nd f1 ((n, a1 :-> a2):f2) f3 s [] [] [] t3)

            Dia n x -> do   
                let t2 = IntMap.insert n (IntMap.findWithDefault (TrieForm (Map.fromList []) [] [] [] Nothing [] [] [] (IntMap.fromList [])) n trie) trie -- make sure it exists
                
                if 
                    | isPrimitive x -> do
                        (a1, TrieForm nd _ _ _ _ _ _ _ _, seen) <- renameFormula level isReflexive generateLit t (Dia n x)
                        return $ ReduceOutput a1 (TrieForm nd f1 f2 ((n, a1 :-> x):f3) s [] [] [] t2)
                    | otherwise -> do
                        (a1, nt@(TrieForm nd _ _ _ _ _ _ _ _), seen) <- renameFormula level isReflexive generateLit t (Dia n x)

                        if seen then do
                            return $ ReduceOutput a1 nt
                        else do

                            (a2, basec, seen2) <- renameFormula (level+1) isReflexive generateLit (fromJust $ IntMap.lookup n t2) x

                            if seen2 then do
                                let t3 = IntMap.insert n basec t2
                                return $ ReduceOutput a1 (TrieForm nd f1 f2 ((n, a1 :-> a2):f3) s [] [] [] t3)
                            else do
                                nc <- repeatedModalise (level+1) isReflexive generateLit (map (\y -> case y of 
                                                                    Or xs -> Or ((negatedNormalForm $ Not a2):xs)
                                                                    other -> Or [negatedNormalForm $ Not a2, other]) (removeAnds x)) basec 

                                let t3 = IntMap.insert n nc t2
                                return $ ReduceOutput a1 (TrieForm nd f1 f2 ((n, a1 :-> a2):f3) s [] [] [] t3)

repeatedModalise :: Int -> Int -> IO String -> [Formula] -> TrieForm -> IO TrieForm
repeatedModalise level isReflexive generateLit [] tf = return tf
repeatedModalise level isReflexive generateLit (f:fs) tf = do
    res <- repeatedModalise level isReflexive generateLit fs tf
    modalise level isReflexive generateLit f res
    

    
-- assumes negated normal form
isPrimitive :: Formula -> Bool
isPrimitive TRUE = True
isPrimitive FALSE = True
isPrimitive (Atom _) = True
isPrimitive (Not _) = True
isPrimitive _ = False

removeDupeClauses :: TrieForm -> IO TrieForm
removeDupeClauses (TrieForm d f1 f2 f3 Nothing x y z trie) = do
    let nf1 = Set.toList $ Set.fromList f1 -- can be removed
    let nf2 = Set.toList $ Set.fromList f2
    let nf3 = Set.toList $ Set.fromList f3
    newTrieList <- helper (IntMap.toList trie)
    return (TrieForm d nf1 nf2 nf3 Nothing x y z (IntMap.fromList newTrieList))
        
        where
            helper :: [(Int, TrieForm)] -> IO [(Int, TrieForm)]
            helper [] = return []
            helper ((n, tf):rest) = do
                ntf <- removeDupeClauses tf
                rrest <- helper rest
                return $ (n, ntf):rest

reduceClauses :: Int -> IO String -> TrieForm -> IO TrieForm 
reduceClauses isReflexive generateLit tf@(TrieForm d f1 of2 of3 Nothing _ _ _ trie) = do
    -- simplify 

    -- (a -> <> c) & (b -> <> c)
    -- = (a | b) -> <> b

    -- (a -> [] c) & (b -> [] c)
    -- = (a | b) -> [] c

    -- (<> b) & (<> c) 
    -- ([] b) & ([] c) = [] (b & c)

    let sf1 = map (sortFormula . simplify) f1
    let ssf1 = map removeTF (filter (/=TRUE) sf1)
    let sssf1 = map negatedNormalForm ssf1-- maybe Not FALSE can cause a problems 
    let f2 = [(n, (removeTF a) :-> (removeTF b)) | (n, a :-> b) <- of2]
    let f3 = [(n, (removeTF a) :-> (removeTF b)) | (n, a :-> b) <- of3]

    -- combine same boxes
    let samebox = Map.fromListWith (++) [((n, r), [l]) | (n, l :-> r) <- f2]
    (brf1, nf2, ntf) <- combineBoxR tf generateLit ([], []) (Map.toList samebox)

    let samedia = Map.fromListWith (++) [((n, r), [l]) | (n, l :-> r) <- f3]
    (drf1, nf3, nntf) <- combineDiaR ntf generateLit ([], []) (Map.toList samedia)
    
    let TrieForm nd nof1 nof2 nof3 Nothing [] [] [] ntrie = nntf
    
    case IntMap.lookup 1 ntrie of 
        Nothing -> return (TrieForm nd (brf1 ++ drf1 ++ sssf1) nf2 nf3 Nothing [] [] [] ntrie)
        Just c -> do
            let samebox2 = Map.fromListWith (++) [((n, l), [r]) | (n, l :-> r) <- nf2]
            (blf1, nnf2, nc) <- combineBoxL c generateLit ([], []) (Map.toList samebox2)

            let (TrieForm cd cf1 cf2 cf3 Nothing [] [] [] trie) = nc
            let nnnc = TrieForm cd (blf1 ++ cf1) cf2 cf3 Nothing [] [] [] trie

            let newTrie = IntMap.insert 1 nnnc ntrie
            helper isReflexive generateLit (IntMap.keys newTrie) (TrieForm nd (brf1 ++ drf1 ++ sssf1) nnf2 nf3 Nothing [] [] [] newTrie) 
    
    where
        helper :: Int -> IO String -> [Int] -> TrieForm -> IO TrieForm
        helper isReflexive generateLit [] t = return t
        helper isReflexive generateLit (x:xs) (TrieForm d f1 f2 f3 f _ _ _ t) = do
            ps <- reduceClauses isReflexive generateLit (fromJust $ IntMap.lookup x t)
            let nt = IntMap.insert x ps t
            helper isReflexive generateLit xs (TrieForm d f1 f2 f3 f [] [] [] nt)     
        
        --- technically can make this even better 
        -- (unfortunately I forgot what this comment was about)
        combineBoxR :: TrieForm -> IO String -> ([Formula], [(Int, Formula :-> Formula)]) -> [((Int, Formula), [Formula])] -> IO ([Formula], [(Int, Formula :-> Formula)], TrieForm)
        combineBoxR tf generateLit (f1, f2) [] = return (f1, f2, tf)
        combineBoxR tf generateLit (f1, f2) (((n, r), [l]):rest) = combineBoxR tf generateLit (f1, (n, l :-> r):f2) rest
        combineBoxR tf generateLit (f1, f2) (((n, r), ls):rest) = do 
            (nl, ntf, _) <- renameFormula 0 isReflexive generateLit tf (sortFormula $ negatedNormalForm (Or ls))
            combineBoxR ntf generateLit ([sortFormula $ Or [negatedNormalForm (Not a), nl] | a <- ls] ++ f1, (n, nl :-> r):f2) rest

        combineDiaR :: TrieForm -> IO String -> ([Formula], [(Int, Formula :-> Formula)]) -> [((Int, Formula), [Formula])] -> IO ([Formula], [(Int, Formula :-> Formula)], TrieForm)
        combineDiaR tf generateLit (f1, f3) [] = return (f1, f3, tf)
        combineDiaR tf generateLit (f1, f3) (((n, r), [l]):rest) = combineDiaR tf generateLit (f1, (n, l :-> r):f3) rest
        combineDiaR tf generateLit (f1, f3) (((n, r), ls):rest) = do 
            (nl, ntf, _) <- renameFormula 0 isReflexive generateLit tf (sortFormula $ negatedNormalForm (Or ls))
            combineDiaR ntf generateLit ([sortFormula $ Or [negatedNormalForm (Not a), nl] | a <- ls] ++ f1, (n, nl :-> r):f3) rest

        combineBoxL :: TrieForm -> IO String -> ([Formula], [(Int, Formula :-> Formula)]) -> [((Int, Formula), [Formula])] -> IO ([Formula], [(Int, Formula :-> Formula)], TrieForm)
        combineBoxL tf generateLit (f1, f2) [] = return (f1, f2, tf)
        combineBoxL tf generateLit (f1, f2) (((n, l), [r]):rest) = combineBoxL tf generateLit (f1, (n, l :-> r):f2) rest
        combineBoxL tf@(TrieForm d a b c Nothing x y z trie) generateLit (f1, f2) (((n, l), rs):rest) = do 
            (nr, ntf, _) <- renameFormula 0 isReflexive generateLit tf (sortFormula $ negatedNormalForm (And rs))
            combineBoxL ntf generateLit ([sortFormula $ Or [negatedNormalForm (Not nr), a] | a <- rs] ++ f1, (n, l :-> nr):f2) rest

        combineDiaL :: TrieForm -> IO String -> ([Formula], [(Int, Formula :-> Formula)]) -> [((Int, Formula), [Formula])] -> IO ([Formula], [(Int, Formula :-> Formula)], TrieForm)
        combineDiaL tf generateLit (f1, f3) [] = return (f1, f3, tf)
        combineDiaL tf generateLit (f1, f3) (((n, l), [r]):rest) = combineDiaL tf generateLit (f1, (n, l :-> r):f3) rest
        combineDiaL tf@(TrieForm d a b c Nothing x y z trie) generateLit (f1, f3) (((n, l), rs):rest) = do 
            (nr, ntf, _) <- renameFormula 0 isReflexive generateLit tf (sortFormula $ negatedNormalForm (And rs))
            combineDiaL ntf generateLit ([sortFormula $ Or [negatedNormalForm (Not nr), a] | a <- rs] ++ f1, (n, l :-> nr):f3) rest
    
removeTF :: Formula -> Formula
removeTF TRUE = Not (Atom "false")
removeTF FALSE = Atom "false"
removeTF (Atom x) = Atom x
removeTF (Not a) = Not (removeTF a)
removeTF (Box a b) = Box a (removeTF b)
removeTF (Dia a b) = Dia a (removeTF b)
removeTF (Or x) = case orTFHelper x of
    Nothing -> Not (Atom "false")
    Just x -> Or x
    where
        orTFHelper :: [Formula] -> Maybe [Formula]
        orTFHelper  [] = Just []
        orTFHelper (TRUE:fs) = Nothing
        orTFHelper (Not (Atom "false"):fs) = Nothing
        orTFHelper (FALSE:fs) = orTFHelper fs
        orTFHelper (Atom "false":fs) = orTFHelper fs
        orTFHelper (f:fs) = case orTFHelper fs of 
            Nothing -> Nothing
            Just x -> Just $ removeTF f : x


-- transitive means we use a global cache
renameFormula :: Int -> Int -> IO String -> TrieForm -> Formula -> IO (Formula, TrieForm, Bool)
renameFormula level ltype a b c
    | ltype >= 1 = renameFormulaT' ltype level reuseRenameType a b c
    | otherwise = renameFormulaN' reuseRenameType a b c

-- for modal K
renameFormulaN' :: Bool -> IO String -> TrieForm -> Formula -> IO (Formula, TrieForm, Bool)
renameFormulaN' True generateLit (TrieForm cache f1 f2 f3 s [] [] [] t) uformula
    | Map.member formula cache = return (fromJust $ Map.lookup formula cache, TrieForm cache f1 f2 f3 s [] [] [] t, True)
    | otherwise = do
        label <- generateLit
        return (Atom label, TrieForm ((Map.insert formula (Atom label) cache)) f1 f2 f3 s [] [] [] t, False)
    where
        formula = sortFormula uformula
renameFormulaN' False generateLit tf@(TrieForm _ _ _ _ _ _ _ _ _) _ = do
    label <- generateLit
    return (Atom label, tf, False)    

    -- Interesting
    -- ([]1 (<>1 []1 <>1 p2 & p1) & <>1 []1 ~p2)
    -- $true => <>1 ~x2
    -- []p1
    -- []$true => <>1 x1
    -- [][]x1 => []1 x2
    -- [][][]x2 => <>1 p2
    -- Satisfiable

-- for modal KT and S4
renameFormulaT' :: Int -> Int -> Bool -> IO String -> TrieForm -> Formula -> IO (Formula, TrieForm, Bool)
renameFormulaT' ltype level True generateLit (TrieForm cache f1 f2 f3 s [] [] [] t) uformula = do
    let formula = sortFormula uformula
    gcache <- readIORef globalRename
    if Map.member formula gcache then do
        let (n, renamedFormula) = fromJust $ Map.lookup formula gcache
        if ((n == 0 && level >= 1 && ltype == 2) || (ltype == 1 && level > n)) then do
            let gcache2 = Map.insert formula (level, renamedFormula) gcache
            writeIORef globalRename gcache2
            return (renamedFormula, TrieForm cache f1 f2 f3 s [] [] [] t, False)
        else
            return (renamedFormula, TrieForm cache f1 f2 f3 s [] [] [] t, True)
    else do
        label <- generateLit 
        let gcache2 = Map.insert formula (level, Atom label) gcache
        -- let gcache3 = Map.insert (sortFormula $ negatedNormalForm $ Not formula) (Not $ Atom label) gcache2
        writeIORef globalRename gcache2
        return (Atom label, TrieForm cache f1 f2 f3 s [] [] [] t, False) 
        
renameFormulaT' _ _ False generateLit tf@(TrieForm _ _ _ _ _ _ _ _ _) _ = do
    label <- generateLit
    return (Atom label, tf, False)        

backpropKT :: TrieForm -> TrieForm 
backpropKT (TrieForm d f1 f2 f3 Nothing _ _ _ trie) = case traverse $ IntMap.lookup 1 trie of 
    Just x@(TrieForm _ a1 a2 a3 Nothing _ _ _ _) -> TrieForm d (uniq (axiomize (a2 ++ f2) ++ a1 ++ f1)) (uniq (a2 ++ f2)) (uniq (a3 ++ f3)) Nothing [] [] [] (IntMap.insert 1 x trie)
    Nothing -> TrieForm d (axiomize f2 ++ f1) f2 f3 Nothing [] [] [] trie
    where 
        traverse :: Maybe TrieForm -> Maybe TrieForm
        traverse Nothing = Nothing
        traverse (Just x) = Just (backpropKT x)

        uniq x = Set.toList $ Set.fromList x

axiomize :: [(Int, Formula :-> Formula)] -> [Formula]
axiomize = map (\(n, a :-> b) -> sortFormula $ negatedNormalForm (Or [Not a, b]))

processS4 :: TrieForm -> TrieForm
processS4 tf@(TrieForm d f1 f2 f3 Nothing a b  c trie) = case IntMap.lookup 1 trie of 
    Just x@(TrieForm d2 _ _ _ _ _ _ _ _) -> TrieForm d (f1++c1++(axiomize (f2 ++ c2))) (f2++c2) (f3++c3) Nothing [] [] [] (IntMap.fromList [(1, TrieForm d2 ((axiomize c2) ++ c1) c2 c3 Nothing [] [] [] (IntMap.fromList []))]) -- skip first stage
        where 
            (c1, c2, c3) = collect ([], [], []) x
            collect :: ([Formula], [(Int, Formula :-> Formula)], [(Int, Formula :-> Formula)]) -> TrieForm -> ([Formula], [(Int, Formula :-> Formula)], [(Int, Formula :-> Formula)])
            collect (a, b, c) (TrieForm _ g1 g2 g3 Nothing [] [] [] trie) = case IntMap.lookup 1 trie of 
                Just x -> collect (g1 ++ a, g2 ++ b, g3 ++ c) x
                Nothing -> (g1 ++ a, g2 ++ b, g3 ++ c)
    Nothing -> TrieForm d (axiomize f2 ++ f1) f2 f3 Nothing a b c trie

getMemo :: TrieForm -> Map.Map Formula Formula
getMemo (TrieForm d f1 f2 f3 Nothing x y z trie) = d
