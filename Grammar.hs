{-# OPTIONS_GHC -w #-}
module Grammar where

import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,70) ([22496,1,1,5502,0,121,0,22008,32256,21,0,0,1024,8,2049,24448,5,0,8192,32256,21,128,22496,1,0,0,0,72,128,22008,32256,21,512,18432,30,0,5502,0,16,0,22008,0,8,512,0,63488,85,0,24448,5,0,0,32256,32789,1375,0,12,768,49152,0,48,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Fml","int","var","dia","box","true","false","'('","')'","'['","']'","'<'","'>'","'~'","'='","'&'","'|'","'-'","%eof"]
        bit_start = st * 22
        bit_end = (st + 1) * 22
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..21]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_4
action_0 (7) = happyShift action_5
action_0 (8) = happyShift action_6
action_0 (9) = happyShift action_7
action_0 (10) = happyShift action_8
action_0 (11) = happyShift action_2
action_0 (13) = happyShift action_9
action_0 (15) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (6) = happyShift action_4
action_2 (7) = happyShift action_5
action_2 (8) = happyShift action_6
action_2 (9) = happyShift action_7
action_2 (10) = happyShift action_8
action_2 (11) = happyShift action_2
action_2 (13) = happyShift action_9
action_2 (15) = happyShift action_10
action_2 (17) = happyShift action_11
action_2 (4) = happyGoto action_24
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (15) = happyShift action_19
action_3 (18) = happyShift action_20
action_3 (19) = happyShift action_21
action_3 (20) = happyShift action_22
action_3 (21) = happyShift action_23
action_3 (22) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_15

action_5 (6) = happyShift action_4
action_5 (7) = happyShift action_5
action_5 (8) = happyShift action_6
action_5 (9) = happyShift action_7
action_5 (10) = happyShift action_8
action_5 (11) = happyShift action_2
action_5 (13) = happyShift action_9
action_5 (15) = happyShift action_10
action_5 (17) = happyShift action_11
action_5 (4) = happyGoto action_18
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (6) = happyShift action_4
action_6 (7) = happyShift action_5
action_6 (8) = happyShift action_6
action_6 (9) = happyShift action_7
action_6 (10) = happyShift action_8
action_6 (11) = happyShift action_2
action_6 (13) = happyShift action_9
action_6 (15) = happyShift action_10
action_6 (17) = happyShift action_11
action_6 (4) = happyGoto action_17
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_16

action_8 _ = happyReduce_17

action_9 (5) = happyShift action_15
action_9 (14) = happyShift action_16
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (5) = happyShift action_13
action_10 (16) = happyShift action_14
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (6) = happyShift action_4
action_11 (7) = happyShift action_5
action_11 (8) = happyShift action_6
action_11 (9) = happyShift action_7
action_11 (10) = happyShift action_8
action_11 (11) = happyShift action_2
action_11 (13) = happyShift action_9
action_11 (15) = happyShift action_10
action_11 (17) = happyShift action_11
action_11 (4) = happyGoto action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_2

action_13 (16) = happyShift action_35
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (6) = happyShift action_4
action_14 (7) = happyShift action_5
action_14 (8) = happyShift action_6
action_14 (9) = happyShift action_7
action_14 (10) = happyShift action_8
action_14 (11) = happyShift action_2
action_14 (13) = happyShift action_9
action_14 (15) = happyShift action_10
action_14 (17) = happyShift action_11
action_14 (4) = happyGoto action_34
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (14) = happyShift action_33
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (6) = happyShift action_4
action_16 (7) = happyShift action_5
action_16 (8) = happyShift action_6
action_16 (9) = happyShift action_7
action_16 (10) = happyShift action_8
action_16 (11) = happyShift action_2
action_16 (13) = happyShift action_9
action_16 (15) = happyShift action_10
action_16 (17) = happyShift action_11
action_16 (4) = happyGoto action_32
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_7

action_18 _ = happyReduce_4

action_19 (18) = happyShift action_30
action_19 (21) = happyShift action_31
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (16) = happyShift action_29
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (6) = happyShift action_4
action_21 (7) = happyShift action_5
action_21 (8) = happyShift action_6
action_21 (9) = happyShift action_7
action_21 (10) = happyShift action_8
action_21 (11) = happyShift action_2
action_21 (13) = happyShift action_9
action_21 (15) = happyShift action_10
action_21 (17) = happyShift action_11
action_21 (4) = happyGoto action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (6) = happyShift action_4
action_22 (7) = happyShift action_5
action_22 (8) = happyShift action_6
action_22 (9) = happyShift action_7
action_22 (10) = happyShift action_8
action_22 (11) = happyShift action_2
action_22 (13) = happyShift action_9
action_22 (15) = happyShift action_10
action_22 (17) = happyShift action_11
action_22 (4) = happyGoto action_27
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (16) = happyShift action_26
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (12) = happyShift action_25
action_24 (15) = happyShift action_19
action_24 (18) = happyShift action_20
action_24 (19) = happyShift action_21
action_24 (20) = happyShift action_22
action_24 (21) = happyShift action_23
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_1

action_26 (6) = happyShift action_4
action_26 (7) = happyShift action_5
action_26 (8) = happyShift action_6
action_26 (9) = happyShift action_7
action_26 (10) = happyShift action_8
action_26 (11) = happyShift action_2
action_26 (13) = happyShift action_9
action_26 (15) = happyShift action_10
action_26 (17) = happyShift action_11
action_26 (4) = happyGoto action_41
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (19) = happyShift action_21
action_27 _ = happyReduce_10

action_28 _ = happyReduce_9

action_29 (6) = happyShift action_4
action_29 (7) = happyShift action_5
action_29 (8) = happyShift action_6
action_29 (9) = happyShift action_7
action_29 (10) = happyShift action_8
action_29 (11) = happyShift action_2
action_29 (13) = happyShift action_9
action_29 (15) = happyShift action_10
action_29 (17) = happyShift action_11
action_29 (4) = happyGoto action_40
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (16) = happyShift action_39
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (16) = happyShift action_38
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_6

action_33 (6) = happyShift action_4
action_33 (7) = happyShift action_5
action_33 (8) = happyShift action_6
action_33 (9) = happyShift action_7
action_33 (10) = happyShift action_8
action_33 (11) = happyShift action_2
action_33 (13) = happyShift action_9
action_33 (15) = happyShift action_10
action_33 (17) = happyShift action_11
action_33 (4) = happyGoto action_37
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_3

action_35 (6) = happyShift action_4
action_35 (7) = happyShift action_5
action_35 (8) = happyShift action_6
action_35 (9) = happyShift action_7
action_35 (10) = happyShift action_8
action_35 (11) = happyShift action_2
action_35 (13) = happyShift action_9
action_35 (15) = happyShift action_10
action_35 (17) = happyShift action_11
action_35 (4) = happyGoto action_36
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_5

action_37 _ = happyReduce_8

action_38 (6) = happyShift action_4
action_38 (7) = happyShift action_5
action_38 (8) = happyShift action_6
action_38 (9) = happyShift action_7
action_38 (10) = happyShift action_8
action_38 (11) = happyShift action_2
action_38 (13) = happyShift action_9
action_38 (15) = happyShift action_10
action_38 (17) = happyShift action_11
action_38 (4) = happyGoto action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (6) = happyShift action_4
action_39 (7) = happyShift action_5
action_39 (8) = happyShift action_6
action_39 (9) = happyShift action_7
action_39 (10) = happyShift action_8
action_39 (11) = happyShift action_2
action_39 (13) = happyShift action_9
action_39 (15) = happyShift action_10
action_39 (17) = happyShift action_11
action_39 (4) = happyGoto action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (19) = happyShift action_21
action_40 (20) = happyShift action_22
action_40 _ = happyReduce_11

action_41 (19) = happyShift action_21
action_41 (20) = happyShift action_22
action_41 _ = happyReduce_12

action_42 (19) = happyShift action_21
action_42 (20) = happyShift action_22
action_42 _ = happyReduce_13

action_43 (19) = happyShift action_21
action_43 (20) = happyShift action_22
action_43 _ = happyReduce_14

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (PNot happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	_
	 =  HappyAbsSyn4
		 (PDia (Int 1) happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (PDia (Int 1) happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 4 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PDia (Int happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	_
	 =  HappyAbsSyn4
		 (PBox (Int 1) happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (PBox (Int 1) happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PBox (Int happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (PAnd happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (POr happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 4 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PImplies happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 4 happyReduction_12
happyReduction_12 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PImplies happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 4 happyReduction_13
happyReduction_13 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PIff happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 4 happyReduction_14
happyReduction_14 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PIff happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  4 happyReduction_15
happyReduction_15 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (PAtom (Var happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  4 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn4
		 (PTrue
	)

happyReduce_17 = happySpecReduce_1  4 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn4
		 (PFalse
	)

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 5;
	TokenVar happy_dollar_dollar -> cont 6;
	TokenDia -> cont 7;
	TokenBox -> cont 8;
	TokenTrue -> cont 9;
	TokenFalse -> cont 10;
	TokenOP -> cont 11;
	TokenCP -> cont 12;
	TokenOB -> cont 13;
	TokenCB -> cont 14;
	TokenOD -> cont 15;
	TokenCD -> cont 16;
	TokenNot -> cont 17;
	TokenEq -> cont 18;
	TokenAnd -> cont 19;
	TokenOr -> cont 20;
	TokenMinus -> cont 21;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 22 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"


data Fml 
    = PNot Fml
    | PDia PNum Fml
    | PBox PNum Fml
    | PAnd Fml Fml
    | POr Fml Fml
    | PImplies Fml Fml
    | PIff Fml Fml 
    | PAtom Var
    | PTrue
    | PFalse
    deriving Show

data PNum = Int Int
    deriving Show

data Var = Var String
    deriving Show

data Token
    = TokenOP
    | TokenCP
    | TokenOB
    | TokenCB
    | TokenOD
    | TokenCD
    | TokenNot
    | TokenImplies
    | TokenIff
    | TokenAnd
    | TokenOr
    | TokenEq
    | TokenInt Int
    | TokenVar String
    | TokenBox
    | TokenDia
    | TokenMinus
    | TokenTrue
    | TokenFalse
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | isAlphaNum c = lexVar (c:cs)
    
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOB : lexer cs
lexer (']':cs) = TokenCB : lexer cs
lexer ('<':cs) = TokenOD : lexer cs
lexer ('>':cs) = TokenCD : lexer cs
lexer ('~':cs) = TokenNot : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('|':cs) = TokenOr : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs

lexNum cs = TokenInt (read num) : lexer rest 
    where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlphaNum cs of 
        ("box", rest) -> TokenBox : lexer rest
        ("dia", rest) -> TokenDia : lexer rest
        ("true", rest) -> TokenTrue : lexer rest
        ("false", rest) -> TokenFalse : lexer rest
        (var, rest) -> TokenVar var : lexer rest

-- main = getContents >>= print . calc . lexer
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc4868_0\\ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
