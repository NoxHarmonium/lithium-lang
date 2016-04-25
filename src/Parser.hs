module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binops = [[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]
        ,[binary "==" Ex.AssocLeft,
          binary "!=" Ex.AssocLeft]
        ,[binary ">" Ex.AssocLeft,
          binary "<" Ex.AssocLeft]
        ,[binary ">=" Ex.AssocLeft,
          binary "<=" Ex.AssocLeft]]

expr :: Parser Expr
expr =  Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

variableDef :: Parser Expr
variableDef = do
  var <- identifier
  colon <- colon
  typ <- identifier
  return $ VarDef var typ

clause :: Parser Expr
clause = do
  condition <- expr
  colon <- colon
  body <- expr
  return $ Clause condition body

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variableDef
  colon <- colon
  body <- expr
  return $ Function name args body

when :: Parser Expr
when = do
  reserved "when"
  colon <- colon
  clauses <- parens $ many clause
  return $ When clauses

modul :: Parser Expr
modul = do
  reserved "module"
  name <- identifier
  body <- parens $ many expr
  return $ Module name body

els :: Parser Expr
els = do
    reserved "else"
    return $ Else

clas :: Parser Expr
clas = do
  reserved "class"
  name <- identifier
  body <- parens $ many expr
  return $ Class name body

impor :: Parser Expr
impor = do
  reserved "import"
  name <- identifier
  reserved "from"
  path <- stringLiteral
  return $ Import name path

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variableDef
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

methodCall :: Parser Expr
methodCall = do
  reserved "this"
  dot <- dot
  name <- identifier
  args <- parens $ commaSep expr
  return $ MethodCall name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try impor
      <|> try extern
      <|> try function
      <|> try when
      <|> try els
      <|> try modul
      <|> try clas
      <|> try methodCall
      <|> try call
      <|> variable
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s