module Parser where

import AST
import Data.Functor ((<&>))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis =
  makeTokenParser
    ( emptyDef
        { commentStart = "/*",
          commentEnd = "*/",
          commentLine = "//",
          opLetter = char '=',
          reservedNames = ["true", "false", "if", "else", "repeat", "skip", "until"],
          reservedOpNames =
            [ "+",
              "-",
              "*",
              "/",
              "<",
              ">",
              "&&",
              "||",
              "!",
              "=",
              "==",
              "!=",
              ";",
              ","
            ]
        }
    )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intexp :: Parser (Exp Int)
intexp = chainl1 intexp2 (reservedOp lis "," >> return ESeq)

intexp2 :: Parser (Exp Int)
intexp2 =
  try (EAssgn <$> identifier lis <*> (reservedOp lis "=" >> intexp2)) <|> intexp3

intexp3 :: Parser (Exp Int)
intexp3 = chainl1 intexp4 suma

intexp4 :: Parser (Exp Int)
intexp4 = chainl1 intexp5 mult

intexp5 :: Parser (Exp Int)
intexp5 =
  (reservedOp lis "-" >> (UMinus <$> intexp5))
    <|> (Const . fromIntegral <$> natural lis)
    <|> (Var <$> identifier lis)
    <|> parens lis intexp

suma :: Parser (Exp Int -> Exp Int -> Exp Int)
suma =
  (reservedOp lis "+" >> return Plus)
    <|> (reservedOp lis "-" >> return Minus)

mult :: Parser (Exp Int -> Exp Int -> Exp Int)
mult =
  (reservedOp lis "*" >> return Times)
    <|> (reservedOp lis "/" >> return Div)

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolexp2 (reservedOp lis "||" >> return Or)

boolexp2 :: Parser (Exp Bool)
boolexp2 = chainl1 boolexp3 (reservedOp lis "&&" >> return And)

boolexp3 :: Parser (Exp Bool)
boolexp3 =
  (reservedOp lis "!" >> (Not <$> boolexp3))
    <|> try intComparison
    <|> (reserved lis "true" >> return BTrue)
    <|> (reserved lis "false" >> return BFalse)
    <|> parens lis boolexp

intComparison :: Parser (Exp Bool)
intComparison = do
  i1 <- intexp
  (Eq i1 <$> (reservedOp lis "==" >> intexp))
    <|> (NEq i1 <$> (reservedOp lis "!=" >> intexp))
    <|> (Lt i1 <$> (reservedOp lis "<" >> intexp))
    <|> (Gt i1 <$> (reservedOp lis ">" >> intexp))

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm2 (reservedOp lis ";" >> return Seq)

comm2 :: Parser Comm
comm2 = ifStatement <|> repeatStatement <|> (reserved lis "skip" >> return Skip) <|> assignmentStatement

ifStatement :: Parser Comm
ifStatement = do
  reserved lis "if"
  cond <- boolexp
  block1 <- braces lis comm
  (reserved lis "else" >> (IfThenElse cond block1 <$> braces lis comm))
    <|> return (IfThen cond block1)

repeatStatement :: Parser Comm
repeatStatement = reserved lis "repeat" >> (Repeat <$> braces lis comm <*> (reserved lis "until" >> boolexp))

assignmentStatement :: Parser Comm
assignmentStatement = do
  var <- identifier lis
  reservedOp lis "="
  Let var <$> intexp

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
