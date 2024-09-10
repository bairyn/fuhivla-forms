-- runhaskell '-package containers' Main.hs
{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .

-- TODO: minor enhancement: for X, maybe add a column to make it like table-annotated.txt, but there are just a few of those, so doing it manually is not a big deal here.

module Main where

import Control.Monad
import Data.List
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
	forM_ etransitions $ putStrLn . (showExtendedTransition' startPState)
	return ()

data Transition = Transition PState Input PState deriving (Eq, Ord)

data ExtendedTransition = ExtendedTransition Transition StepKind ChainState deriving (Eq, Ord)

data StepKind = X | O | B | L deriving (Eq, Ord)

data Input = V | C | E deriving (Eq, Ord)

showTransition :: Transition -> String
showTransition (Transition pstate input pstate') = (showPstate pstate) ++ " " ++ (showInput input) ++ " -> " ++ (showPstate pstate')

showExtendedTransition :: ExtendedTransition -> String
--showExtendedTransition (ExtendedTransition transition stepKind chains) = (showTransition transition) ++ "  " ++ (showStepKind stepKind) ++ (prefixNonempty " " $ showChains chains)
showExtendedTransition (ExtendedTransition transition stepKind chains) = (showTransition transition) ++ "  " ++ (showStepKind stepKind) ++ (prefixNonempty " " $ showChainState chains)

-- A variant that prefixes the initial parser state with an asterisk.
showExtendedTransition' :: PState -> ExtendedTransition -> String
--showExtendedTransition (ExtendedTransition transition stepKind chains) = (showTransition transition) ++ "  " ++ (showStepKind stepKind) ++ (prefixNonempty " " $ showChains chains)
showExtendedTransition' theStartPState (ExtendedTransition transition@(Transition from _ _) stepKind chains) = prefix ++ (showTransition transition) ++ "  " ++ (showStepKind stepKind) ++ (prefixNonempty " " $ showChainState chains)
	where
		prefix :: String
		prefix
			| from == theStartPState = "^"
			| otherwise              = " "

prefixNonempty :: String -> String -> String
prefixNonempty _      s@[] = s
prefixNonempty prefix s    = prefix ++ s

suffixNonempty :: String -> String -> String
suffixNonempty _      s@[] = s
suffixNonempty suffix s    = s ++ suffix

showInput :: Input -> String
showInput C = "C"
showInput V = "V"
showInput E = "$"

cycleInput :: Input -> Input
cycleInput V = C
cycleInput C = E
cycleInput E = C

showStepKind :: StepKind -> String
showStepKind X = "X"  -- Extinct chains; all died.
showStepKind O = "O"  -- At least one chain is in progress, but if parsing stops now, all chains go extinct.
showStepKind B = "B"  -- At least one chain is in progress, and at least one token has empty input, meaning it would survive an end-of-parse ‘$’ input.
showStepKind L = "L"  -- Loop back to a previous declared parser state.

showInputLower :: Input -> String
showInputLower V = "v"
showInputLower C = "c"
showInputLower E = "$"

-- showPstate and showChains are later.

data PState = PState CVVPState CVCPState CCVPState CVCCPState CCVCPState CVCCVPState CCVCVPState deriving (Eq, Ord)

data CVVState   a = CVVState   a a a     deriving (Eq, Ord)
data CVCState   a = CVCState   a a a     deriving (Eq, Ord)
data CCVState   a = CCVState   a a a     deriving (Eq, Ord)
data CVCCState  a = CVCCState  a a a a   deriving (Eq, Ord)
data CCVCState  a = CCVCState  a a a a   deriving (Eq, Ord)
data CVCCVState a = CVCCVState a a a a a deriving (Eq, Ord)
data CCVCVState a = CCVCVState a a a a a deriving (Eq, Ord)

instance Functor CVVState   where fmap = fmapCVVState
instance Functor CVCState   where fmap = fmapCVCState
instance Functor CCVState   where fmap = fmapCCVState
instance Functor CVCCState  where fmap = fmapCVCCState
instance Functor CCVCState  where fmap = fmapCCVCState
instance Functor CVCCVState where fmap = fmapCVCCVState
instance Functor CCVCVState where fmap = fmapCCVCVState

type CVVPState   = CVVState   Bool
type CVCPState   = CVCState   Bool
type CCVPState   = CCVState   Bool
type CVCCPState  = CVCCState  Bool
type CCVCPState  = CCVCState  Bool
type CVCCVPState = CVCCVState Bool
type CCVCVPState = CCVCVState Bool

fmapCVVState :: (a -> b) -> (CVVState a -> CVVState b)
fmapCVVState f (CVVState c0 v1 v2) = CVVState (f c0) (f v1) (f v2)

fmapCVCState :: (a -> b) -> (CVCState a -> CVCState b)
fmapCVCState f (CVCState c0 v1 c2) = CVCState (f c0) (f v1) (f c2)

fmapCCVState :: (a -> b) -> (CCVState a -> CCVState b)
fmapCCVState f (CCVState c0 c1 v2) = CCVState (f c0) (f c1) (f v2)

fmapCVCCState :: (a -> b) -> (CVCCState a -> CVCCState b)
fmapCVCCState f (CVCCState c0 v1 c2 c3) = CVCCState (f c0) (f v1) (f c2) (f c3)

fmapCCVCState :: (a -> b) -> (CCVCState a -> CCVCState b)
fmapCCVCState f (CCVCState c0 c1 v2 c3) = CCVCState (f c0) (f c1) (f v2) (f c3)

fmapCVCCVState :: (a -> b) -> (CVCCVState a -> CVCCVState b)
fmapCVCCVState f (CVCCVState c0 v1 c2 c3 v4) = CVCCVState (f c0) (f v1) (f c2) (f c3) (f v4)

fmapCCVCVState :: (a -> b) -> (CCVCVState a -> CCVCVState b)
fmapCCVCVState f (CCVCVState c0 c1 v2 c3 v4) = CCVCVState (f c0) (f c1) (f v2) (f c3) (f v4)

data ChainState = ChainState CVVChainState CVCChainState CCVChainState CVCCChainState CCVCChainState CVCCVChainState CCVCVChainState deriving (Eq, Ord)

showPstate :: PState -> String
showPstate (PState cvvPState cvcPState ccvPState cvccPState ccvcPState cvccvPState ccvcvPState) = (showCVVPState cvvPState) ++ "-" ++ (showCVCPState cvcPState) ++ "-" ++ (showCCVPState ccvPState) ++ "-" ++ (showCVCCPState cvccPState) ++ "-" ++ (showCCVCPState ccvcPState) ++ "-" ++ (showCVCCVPState cvccvPState) ++ "-" ++ (showCCVCVPState ccvcvPState)

showC :: Bool -> String
showC False = "c"
showC True = "C"
showV :: Bool -> String
showV False = "v"
showV True = "V"

showCVCPState :: CVCPState -> String
showCVCPState (CVCState c0 v1 c2) = showC c0 ++ showV v1 ++ showC c2

showCCVPState :: CCVPState -> String
showCCVPState (CCVState c0 c1 v2) = showC c0 ++ showC c1 ++ showV v2

showCVVPState :: CVVPState -> String
showCVVPState (CVVState c0 v1 v2) = showC c0 ++ showV v1 ++ showV v2

showCVCCPState :: CVCCPState -> String
showCVCCPState (CVCCState c0 v1 c2 c3) = showC c0 ++ showV v1 ++ showC c2 ++ showC c3

showCCVCPState :: CCVCPState -> String
showCCVCPState (CCVCState c0 c1 v2 c3) = showC c0 ++ showC c1 ++ showV v2 ++ showC c3

showCVCCVPState :: CVCCVPState -> String
showCVCCVPState (CVCCVState c0 v1 c2 c3 v4) = showC c0 ++ showV v1 ++ showC c2 ++ showC c3 ++ showV v4

showCCVCVPState :: CCVCVPState -> String
showCCVCVPState (CCVCVState c0 c1 v2 c3 v4) = showC c0 ++ showC c1 ++ showV v2 ++ showC c3 ++ showV v4

f, t :: Bool
f = False
t = True

cvv :: a -> a -> a -> CVVState a
cvv = CVVState
cvc :: a -> a -> a -> CVCState a
cvc = CVCState
ccv :: a -> a -> a -> CCVState a
ccv = CCVState
cvcc :: a -> a -> a -> a -> CVCCState a
cvcc = CVCCState
ccvc :: a -> a -> a -> a -> CCVCState a
ccvc = CCVCState
cvccv :: a -> a -> a -> a -> a -> CVCCVState a
cvccv = CVCCVState
ccvcv :: a -> a -> a -> a -> a -> CCVCVState a
ccvcv = CCVCVState

fu'ivlaPState :: PState
fu'ivlaPState = PState (cvv f f f) (cvc f f f) (ccv f f f) (cvcc f f f f) (ccvc f f f f) (cvccv f f f f f) (ccvcv f f f f f)

-- pre-slinku'i:
-- Also, this would need to be updated when changing initial chain state to middle chain state (this observation fixes the TODO that was before middleState about this).
--startPState :: PState
--startPState = PState (cvv t f f) (cvc t f f) (ccv t f f) (cvcc t f f f) (ccvc t f f f) (cvccv t f f f f) (ccvcv t f f f f)

startPState :: PState
startPState = startPState' initialChainState

startPState' :: ChainState -> PState
startPState' (ChainState cvvState cvcState ccvState cvccState ccvcState cvccvState ccvcvState) =
	PState
		(fmapCVVState   (not . null) cvvState)
		(fmapCVCState   (not . null) cvcState)
		(fmapCCVState   (not . null) ccvState)
		(fmapCVCCState  (not . null) cvccState)
		(fmapCCVCState  (not . null) ccvcState)
		(fmapCVCCVState (not . null) cvccvState)
		(fmapCCVCVState (not . null) ccvcvState)

-- TODO:
type CVVChainState   = CVVState   [Chain]
type CVCChainState   = CVCState   [Chain]
type CCVChainState   = CCVState   [Chain]
type CVCCChainState  = CVCCState  [Chain]
type CCVCChainState  = CCVCState  [Chain]
type CVCCVChainState = CVCCVState [Chain]
type CCVCVChainState = CCVCVState [Chain]

--data Chain = TODO deriving (Eq, Ord)

data TokenID
	= CVVID
	| CVCID
	| CCVID
	| CVCCID
	| CCVCID
	| CVCCVID
	| CCVCVID
	deriving (Eq, Ord, Enum, Show)

-- Note: the next input is _not_ included in the list in this representation.  That can be inferred from 'getNextTokenInput'.
data Token = Token TokenID [Input] deriving (Eq, Ord)

tokenSizes :: TokenID -> Integer
tokenSizes CVVID   = 3
tokenSizes CVCID   = 3
tokenSizes CCVID   = 3
tokenSizes CVCCID  = 4
tokenSizes CCVCID  = 4
tokenSizes CVCCVID = 5
tokenSizes CCVCVID = 5

allTokens :: [TokenID]
allTokens =
	[
		CVVID,
		CVCID,
		CCVID,
		CVCCID,
		CCVCID,
		CVCCVID,
		CCVCVID
	]

isTokenCompleted :: Token -> Bool
isTokenCompleted (Token id xs)
	| l > s || l < 0 = error $ "isTokenCompleted: invalid data; l > s || l < 0: (l, s): " ++ (show (l, s))
	| otherwise = if not $ l == s then False else True
	where
		l = genericLength xs
		s = tokenSizes id

-- Nothing when it's full.
getNextTokenInput :: Token -> Maybe Input
getNextTokenInput t@(Token id xs) | isTokenCompleted t = Nothing | otherwise = Just $ getNextTokenInput' id (genericLength xs)

getNextTokenInput' :: TokenID -> Integer -> Input
getNextTokenInput' CVVID   0 = C
getNextTokenInput' CVVID   1 = V
getNextTokenInput' CVVID   2 = V
getNextTokenInput' CVCID   0 = C
getNextTokenInput' CVCID   1 = V
getNextTokenInput' CVCID   2 = C
getNextTokenInput' CCVID   0 = C
getNextTokenInput' CCVID   1 = C
getNextTokenInput' CCVID   2 = V
getNextTokenInput' CVCCID  0 = C
getNextTokenInput' CVCCID  1 = V
getNextTokenInput' CVCCID  2 = C
getNextTokenInput' CVCCID  3 = C
getNextTokenInput' CCVCID  0 = C
getNextTokenInput' CCVCID  1 = C
getNextTokenInput' CCVCID  2 = V
getNextTokenInput' CCVCID  3 = C
getNextTokenInput' CVCCVID 0 = C
getNextTokenInput' CVCCVID 1 = V
getNextTokenInput' CVCCVID 2 = C
getNextTokenInput' CVCCVID 3 = C
getNextTokenInput' CVCCVID 4 = V
getNextTokenInput' CCVCVID 0 = C
getNextTokenInput' CCVCVID 1 = C
getNextTokenInput' CCVCVID 2 = V
getNextTokenInput' CCVCVID 3 = C
getNextTokenInput' CCVCVID 4 = V
getNextTokenInput' id      l = error $ "getNextTokenInput': error: unknown input id l: " ++ show (id, l)

-- | Invariant: the last one is not finished, and all other are.
-- (It's a quick Haskell script, so we can lower our standards and be more lazy and do things like have invariants on type aliases.)
-- (Note: chains should never be empty.)
type Chain = [Token]

--showChains :: [Chain] -> String
--showChains _ = "TODO"

chainsTuple :: ChainState -> ([Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain], [Chain])
chainsTuple (ChainState (CVVState l6 l7 l8) (CVCState l0 l1 l2) (CCVState l3 l4 l5) (CVCCState l9 l10 l11 l12) (CCVCState l13 l14 l15 l16) (CVCCVState l17 l18 l19 l20 l21) (CCVCVState l22 l23 l24 l25 l26)) = (l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26)

-- maybe you could set up something like folds, but one-off, you're almost done with everything but showChainState and etransitions, so just do it by hand for now I guess.
chainGroups :: ChainState -> [[Chain]]
chainGroups (ChainState (CVVState l6 l7 l8) (CVCState l0 l1 l2) (CCVState l3 l4 l5) (CVCCState l9 l10 l11 l12) (CCVCState l13 l14 l15 l16) (CVCCVState l17 l18 l19 l20 l21) (CCVCVState l22 l23 l24 l25 l26)) = [l0, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, l21, l22, l23, l24, l25, l26]

getChainGroups :: ChainState -> [[Chain]]
getChainGroups = chainGroups

chains :: ChainState -> [Chain]
chains = concat . chainGroups

-- For shadowed ‘chains’.
getChains :: ChainState -> [Chain]
getChains = chains

--data ChainState = ChainState CVVChainState CVCChainState CCVChainState CVCCChainState CCVCChainState CVCCVChainState CCVCVChainState deriving (Eq, Ord)
-- Start from the beginning for each token.
-- (The normal initial chain state.)
startChainStateIgnoreSlinku'i :: ChainState
startChainStateIgnoreSlinku'i = ChainState (cvv (s [Token CVVID []]) n n) (cvc (s [Token CVCID []]) n n) (ccv (s [Token CCVID []]) n n) (cvcc (s [Token CVCCID []]) n n n) (ccvc (s [Token CCVCID []]) n n n) (cvccv (s [Token CVCCVID []]) n n n n) (ccvcv (s [Token CCVCVID []]) n n n n)
	where n = []; s x = [x]

-- 2024-09-09 I forgot to include the slinku'i test.  This version of startChainState incorporates it.
-- CLL 4.7 defines the slinku'i test: a CV prefix should not make a lujvo.
-- As before, but add 1 part: besides handling the case of no prefix, also
-- handle the case that assumes a CV has already been parsed.
-- Thus words like {ca} can unambiguously precede a brivla or fu'ivla without joining the word.
startChainStateWithSlinku'i :: ChainState
startChainStateWithSlinku'i = ChainState (cvv (s [Token CVVID []]) n (s [Token CVVID [C, V]])) (cvc (s [Token CVCID []]) n (s [Token CVCID [C, V]])) (ccv (s [Token CCVID []]) n n) (cvcc (s [Token CVCCID []]) n (s [Token CVCCID [C, V]]) n) (ccvc (s [Token CCVCID []]) n n n) (cvccv (s [Token CVCCVID []]) n (s [Token CVCCVID [C, V]]) n n) (ccvcv (s [Token CCVCVID []]) n n n n)
	where n = []; s x = [x]

-- slinku'i is part of the definition of fu'ivla (condition #3 in CLL 4.7), so
-- this needs to be enabled.  The omission of the slinku'i test is now fixed (2024-09-09).
useSlinku'i :: Bool
useSlinku'i = True

startChainState :: ChainState
startChainState = if' useSlinku'i startChainStateWithSlinku'i startChainStateIgnoreSlinku'i
	where if' c t e = if c then t else e

-- Start from an unknown location: any valid prefix can be prepended.
-- 2023-07-30 TODO FIXME: when this is the initial chain state, it's not the state where everything is capitalized as expected.  There's an issue somehow where with this.  EDIT 2024-09-09 fixed: I think you just forgot about startPState and the priority queue, and the first line is not necessarily the start state.  I added an update so it starts with a ‘^’ to indicate initial state  EDIT 2024-09-09 fixed: I think you just forgot about the priority queue, and the first line is not necessarily the start state.  I added an update so it starts with a ‘^’ to indicate initial state
-- 2024-09-09 (Note: this isn't used anywhere in the output of the main program.)
middleChainState :: ChainState
middleChainState = ChainState (cvv (s [Token CVVID []]) (s [Token CVVID [C]]) (s [Token CVVID [C, V]])) (cvc (s [Token CVCID []]) (s [Token CVCID [C]]) (s [Token CVCID [C, V]])) (ccv (s [Token CCVID []]) (s [Token CCVID [C]]) (s [Token CCVID [C, C]])) (cvcc (s [Token CVCCID []]) (s [Token CVCCID [C]]) (s [Token CVCCID [C, V]]) (s [Token CVCCID [C, V, C]])) (ccvc (s [Token CCVCID []]) (s [Token CCVCID [C]]) (s [Token CCVCID [C, C]]) (s [Token CCVCID [C, C, V]])) (cvccv (s [Token CVCCVID []]) (s [Token CVCCVID [C]]) (s [Token CVCCVID [C, V]]) (s [Token CVCCVID [C, V, C]]) (s [Token CVCCVID [C, V, C, C]])) (ccvcv (s [Token CCVCVID []]) (s [Token CCVCVID [C]]) (s [Token CCVCVID [C, C]]) (s [Token CCVCVID [C, C, V]]) (s [Token CCVCVID [C, C, V, C]]))
	where n = []; s x = [x]

emptyChainState :: ChainState
emptyChainState = ChainState (cvv n n n) (cvc n n n) (ccv n n n) (cvcc n n n n) (ccvc n n n n) (cvccv n n n n n) (ccvcv n n n n n)
	where n = []

initialChainState :: ChainState
initialChainState = startChainState
--initialChainState = middleChainState

showChainState :: ChainState -> String
showChainState cs@(ChainState cvvChains cvcChains ccvChains cvccChains ccvcChains cvccvChains ccvcvChains) =
	intercalate " | " . filter (not . null) . map (\chainGroup -> gl chainGroup ++ (intercalate " | " . map showChain $ chainGroup) ++ gr chainGroup) . chainGroups $ cs
	where
		gl chainGroup | genericLength chainGroup <= 1 = "" | otherwise = "<"
		gr chainGroup | genericLength chainGroup <= 1 = "" | otherwise = ">"

-- | Requires non-empty.
showChain :: Chain -> String
showChain ts = suffixNonempty " " (showCompletedTokens completed) ++ showPartial partial
	where (completed, partial) = (init ts, last ts)

showCompletedTokens :: [Token] -> String
showCompletedTokens = intercalate " " . fmap showToken

showPartial :: Token -> String
showPartial t = showToken t

showToken :: Token -> String
showToken t@(Token id inputs)
	| isTokenCompleted t = concatMap showInputLower inputs
	| otherwise = concatMap showInputLower inputs ++ "(" ++ showInputLower (getNextTokenInput' id (genericLength inputs)) ++ ")"

chainStateToPState :: ChainState -> PState
chainStateToPState (ChainState cvvPState cvcPState ccvPState cvccPState ccvcPState cvccvPState ccvcvPState) =
	PState (fmapCVVState nn cvvPState) (fmapCVCState nn cvcPState) (fmapCCVState nn ccvPState) (fmapCVCCState nn cvccPState) (fmapCCVCState nn ccvcPState) (fmapCVCCVState nn cvccvPState) (fmapCCVCVState nn ccvcvPState)
	where nn = not . null

-- (chains should never be null.)
stepKindToPState :: (ChainState -> Bool) -> ChainState -> StepKind
stepKindToPState known chainState
	| chainState == emptyChainState = X
	| known chainState = L
	| not . null . filter (\tokens -> let Token tid inputs = last tokens in null $ inputs) . chains $ chainState = B
	| otherwise = O

-- (chains should never be null.)
-- NOTE: we would need to sort this, but I think the way we do this means it's already sorted, meaning an extra explicit sort step would be redundant.
groupChainsToChainState :: [Chain] -> ChainState
groupChainsToChainState chains = ChainState cvvChainState cvcChainState ccvChainState cvccChainState ccvcChainState cvccvChainState ccvcvChainState
	where
		cvvChainState :: CVVChainState
		cvvChainState = CVVState (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVVID && genericLength inputs == 0) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVVID && genericLength inputs == 1) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVVID && genericLength inputs == 2)
		cvcChainState :: CVCChainState
		cvcChainState = CVCState (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCID && genericLength inputs == 0) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCID && genericLength inputs == 1) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCID && genericLength inputs == 2)
		ccvChainState :: CCVChainState
		ccvChainState = CCVState (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVID && genericLength inputs == 0) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVID && genericLength inputs == 1) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVID && genericLength inputs == 2)
		cvccChainState :: CVCCChainState
		cvccChainState = CVCCState (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCID && genericLength inputs == 0) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCID && genericLength inputs == 1) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCID && genericLength inputs == 2) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCID && genericLength inputs == 3)
		ccvcChainState :: CCVCChainState
		ccvcChainState = CCVCState (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCID && genericLength inputs == 0) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCID && genericLength inputs == 1) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCID && genericLength inputs == 2) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCID && genericLength inputs == 3)
		cvccvChainState :: CVCCVChainState
		cvccvChainState = CVCCVState (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCVID && genericLength inputs == 0) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCVID && genericLength inputs == 1) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCVID && genericLength inputs == 2) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCVID && genericLength inputs == 3) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CVCCVID && genericLength inputs == 4)
		ccvcvChainState :: CCVCVChainState
		ccvcvChainState = CCVCVState (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCVID && genericLength inputs == 0) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCVID && genericLength inputs == 1) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCVID && genericLength inputs == 2) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCVID && genericLength inputs == 3) (flip filter chains $ \tokens -> let Token tid inputs = last tokens in tid == CCVCVID && genericLength inputs == 4)

etransitions :: [ExtendedTransition]
etransitions = start ++ unfoldr step seed
	where 
		-- (Priority queue of undeclared states for future steps (one is selected at each step), chainstates for getting there.)
		seed :: (S.Set PState, M.Map PState ChainState, [ExtendedTransition])
		seed = (S.singleton startPState, M.singleton startPState initialChainState, [])
		step :: (S.Set PState, M.Map PState ChainState, [ExtendedTransition]) -> Maybe (ExtendedTransition, (S.Set PState, M.Map PState ChainState, [ExtendedTransition]))
		step (s, chainStates, chunk@(extendedTransition:rest)) = Just (extendedTransition, (s, chainStates, rest))
		step (s, chainStates, []) =
			let
				(thisPState, s') = S.deleteFindMin s
				lastChainState = fromMaybe (error "internal error: etransitions: chainstate should be in map but wasn't") $ flip M.lookup chainStates thisPState
				--nextChainStates = (chainStates `insertRotr` nextPState) nextChainState
				nextChainStates :: M.Map PState ChainState
				nextChainStates = foldl' (\chainStates' (ExtendedTransition (Transition _fromPState _input toPState) _stepKind chainState) -> insertNewRotr chainStates' toPState chainState) chainStates nextExtendedTransitions  -- (Loop case): if a chain state already is in there, just leave the original alone.
				insertRotr a b c = M.insert b c a
				insertNewRotr a b c = M.insertWith (flip const) b c a

				result :: Maybe (ExtendedTransition, (S.Set PState, M.Map PState ChainState, [ExtendedTransition]))
				result = if S.null s || null nextExtendedTransitions then Nothing else Just (head nextExtendedTransitions, (nextPqueue, nextChainStates, tail nextExtendedTransitions))

				-- We need to find:
				-- nextExtendedTransitions :: [ExtendedTransition]  -- 3-len, 1 for each input.
				-- nextPqueue  :: S.Set PState  -- Original queue, then remove min for us to process, then optionally add more to process, and let Set prioritize it properly.
				_goal0 = (nextExtendedTransitions :: [ExtendedTransition], nextPqueue :: S.Set PState)

				nextPqueue :: S.Set PState
				nextPqueue =
					let
						toPStates = map (\(ExtendedTransition (Transition _fromPState _input toPState) _stepKind _chainState) -> toPState) nextExtendedTransitions
						toPStates' = filter (\toPState -> (flip M.member nextChainStates $ toPState) && (toPState /= emptyPState) && not (flip M.member chainStates $ toPState)) toPStates
						emptyPState = chainStateToPState emptyChainState
						s'' = foldl' (\pStates pState -> flip S.insert pStates pState) s' toPStates'
					in
						s''
				nextExtendedTransitions :: [ExtendedTransition]
				--nextExtendedTransitions = [nextExtendedTransition i | i <- [V, C, E]]  -- TODO
				nextExtendedTransitions = [nextExtendedTransition i | i <- [V, C{-, E-}]]

				-- Get the extended transitions.
				-- Get the chains: there are several approaches, but our approach will just take all the chains, process them, and we'll group them back into a state at the end.
				chains :: [Chain]
				chains = getChains lastChainState
				chainsSurviving :: Input -> [Chain]
				chainsSurviving input = flip filter chains $ \chain -> not (null chain) && let t = last chain in fromMaybe (error "internal error: etransitions: chain was full in a chain state.") (getNextTokenInput t) == input
				-- These chains need only 1 character.  We need to consider them since for every penultimate chain that gets completed, it becomes a copy for every of the 7 token forms.
				penultimateChains :: Input -> [Chain]
				-- Note: chains never be empty.
				penultimateChains input = flip filter (chainsSurviving input) $ \chain -> not (null chain) && let (Token id inputs) = last chain in genericLength inputs == tokenSizes id - 1
				nonPenultimateChains :: Input -> [Chain]
				nonPenultimateChains input = flip filter (chainsSurviving input) $ not . \chain -> not (null chain) && let (Token id inputs) = last chain in genericLength inputs == tokenSizes id - 1
				-- For nonPenultimateChains, extend.  For penultimateChains, copy for each form.  (This can end up out of order, so we'll need to re-sort them again.)
				nonPenultimateChainsExtended :: Input -> [Chain]
				nonPenultimateChainsExtended input = flip fmap (nonPenultimateChains input) $ \tokens -> init tokens ++ [let Token id inputs = last tokens in Token id (inputs ++ [input])]
				penultimateChainsExtended :: Input -> [Chain]
				-- (tokens should be non-null always.)
				penultimateChainsExtended input = [r | tid <- allTokens, penultimateChainExtended <- flip map (penultimateChains input) $ \tokens -> init tokens ++ [let Token id inputs = last tokens in Token id (inputs ++ [input])] ++ [Token tid []], r <- [penultimateChainExtended]]
				chainsExtended :: Input -> [Chain]
				chainsExtended input = penultimateChainsExtended input ++ nonPenultimateChainsExtended input

				nextExtendedTransition :: Input -> ExtendedTransition
				nextExtendedTransition input = ExtendedTransition (Transition fromPState input toPState) stepKind toChainState
					where
						fromPState = thisPState
						toPState = chainStateToPState toChainState
						stepKind :: StepKind
						stepKind = stepKindToPState (\chainState -> flip M.member chainStates . chainStateToPState $ chainState) toChainState
						toChainState :: ChainState
						toChainState = groupChainsToChainState $ chainsExtended input
			in
				result
		start :: [ExtendedTransition]
		start =
			[
				ExtendedTransition (Transition fu'ivlaPState V fu'ivlaPState) X emptyChainState,
				ExtendedTransition (Transition fu'ivlaPState C fu'ivlaPState) X emptyChainState,
				ExtendedTransition (Transition fu'ivlaPState E fu'ivlaPState) X emptyChainState
			]

-- showPstate and showChains are later.
