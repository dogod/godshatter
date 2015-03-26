{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Parsec
import Control.Applicative
test p s = case parse p "" s of; Left e -> error (show e); Right x -> x
type Var = String
type Label = String
type Type = Term
data Term = LET [Context] Term
          | V Var | TYPE
          | PI Var Type Type | LAMB Var Term | APP Term Term
          | SUM Var Type Type | PAIR Term Term | SPLIT Term Var Var Term
          | ENUM [Label] | L Label | CASE Term [(Label,Term)]
          | LIFT Type | BOX Term | EVAL Term
          | REC Type | FOLD Term | UNFOLD Term Var Term
                                   deriving (Show,Eq)
data Context = INTRO Var Type | DEFN Var Term deriving (Show,Eq)
pTerm = choice $ map try [pLet,
                          V <$> pVar, string "Type" >> return TYPE,
                          pPi, pLamb, pApp,
                          pSum, pPair, pSplit,
                          pEnum, pLabel, pCase,
                          pLift, pBox, pEval,
                          pRec, pFold, pUnfold]
pType = pTerm
pLet = do
  string "let " >> spaces
  c <- pContexts
  spaces >> string "in" >> spaces
  LET c <$> pTerm

pContext = do
  x <- pVar
  spaces
  sp <- oneOf ":="
  spaces
  s <- pType
  return $ case sp of
    ':' -> INTRO x s
    '=' -> DEFN x s
pContexts = between (char '{') (char '}') $ sepBy pContext (char ';')
pVar = char '_' >> many1 alphaNum
pPi = do
  x <- char '(' >> pVar
  spaces >> char ':' >> spaces
  s <- pType
  char '(' >> spaces >> string " -> " >> spaces
  PI x s <$> pType
pLamb = do
  x <- char '%' >> pVar
  spaces >> string " -> " >> spaces
  LAMB x <$> pTerm
pApp = APP <$> pTerm <*> (spaces >> pTerm)
pSum = do
  x <- char '(' >> pVar
  spaces >> char ':' >> spaces
  s <- pType
  char ')' >> spaces >> char '*' >> spaces
  SUM x s <$> pType
pPair = do
  t <- char '(' >> pTerm
  char ',' >> spaces
  u <- pTerm
  char ')' >> return (PAIR t u)
pSplit = do
  string "split " >> spaces
  t <- pTerm
  spaces >> string " with " >> spaces
  x <- char '(' >> pVar
  y <- char ',' >> spaces >> pVar
  char ')' >> spaces >> string " -> " >> spaces
  SPLIT t x y <$> pTerm
pLabel = L <$> (char '`' >> many1 alphaNum)
pEnum = ENUM <$> between (char '{') (char '}')
        (sepBy (many1 alphaNum) (many1 space))
pCase = do
  t <- string "case " >> spaces >> pTerm
  cs <- between (char '{') (char '}') $
        sepBy (do l <- many1 alphaNum
                  spaces >> string " -> " >> spaces
                  (,) l <$> pTerm)
        (spaces >> char '|' >> spaces)
  return $ CASE t cs
pLift = LIFT <$> (char '^' >> pType)
pBox = BOX <$> between (char '[') (char ']') pTerm
pEval = EVAL <$> (char '!' >> pTerm)
pRec = REC <$> (string "Rec" >> spaces >> pType)
pFold = FOLD <$> (string "fold" >> spaces >> pTerm)
pUnfold = do
  string "unfold" >> spaces
  t <- pTerm
  spaces >> string " as " >> spaces
  x <- pVar
  spaces >> string " -> " >> spaces
  UNFOLD t x <$> pTerm
