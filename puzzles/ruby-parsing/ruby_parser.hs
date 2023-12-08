import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO
import Data.Functor

data Expr
  = BoolLiteral Bool
  deriving (Show)

data Stmt
  = Puts String
  | Cout String
  | Cond Expr [Stmt] [Stmt]  -- Conditional expr (succeed?) (fail?)
  deriving (Show)

-- data Stmt
--   = Puts String
--   | Cout String
--   | Cond Expr Stmt Stmt  -- Conditional expr (succeed?) (fail?)
--   | Seq [Stmt]
--   deriving (Show)

sym :: String -> Parser String
sym s = spaces *> string s <* spaces

parseStringLiteral :: Parser String
parseStringLiteral = choice
  [ char '\'' *> manyTill anyChar (char '\'')
  , char '\"' *> manyTill anyChar (char '\"')
  ]

parseInteger :: Parser String
parseInteger = many1 digit

parsePrintable :: Parser String
parsePrintable = parseStringLiteral <|> parseInteger

parsePutsStmt :: Parser Stmt
parsePutsStmt = sym "puts" *> (Puts <$> parsePrintable) <* spaces

parseCoutStmt :: Parser Stmt
parseCoutStmt = do
  sym "$>"
  strings <- many $ try $ sym "<<" *> parsePrintable <* spaces
  return $ Cout (concat strings)

parsePrintStmt :: Parser Stmt
parsePrintStmt = parsePutsStmt <|> parseCoutStmt

parseStmt :: Parser Stmt
parseStmt = parseIfStmt <|> parsePrintStmt

parseBlock :: Parser [Stmt]
parseBlock = choice
  [ try $ between openTok closeTok $ many parseStmt
  | (openTok, closeTok) <- [ (sym "do", sym "end"), (sym "{", sym "}") ]
  ]

parseBoolLiteral :: Parser Expr
parseBoolLiteral = 
  (string "true" $> BoolLiteral True) <|> (string "false" $> BoolLiteral False)

parseIfStmt :: Parser Stmt
parseIfStmt = do
  boolLit <- between (sym "(") (sym ")") parseBoolLiteral
  string ".if?"
  blockBody <- parseBlock
  elsifBranches <- many (try parseElsifStmt)
  elseBranch <- option [] (try parseElse)
  return (Cond boolLit blockBody (elsifBranches ++ elseBranch))

parseElsifStmt :: Parser Stmt
parseElsifStmt = do
  string ".elsif?"
  boolLit <- between (sym "(") (sym ")") parseBoolLiteral
  blockBody <- parseBlock
  return (Cond boolLit blockBody [])

parseElse :: Parser [Stmt]
parseElse = do
  string ".else?"
  blockBody <- parseBlock
  return [Cond (BoolLiteral True) blockBody []]

mainParser :: String -> Either ParseError [Stmt]
mainParser = parse parseIfStmts ""
  where parseIfStmts = many parseIfStmt <* eof
    
-- Resolve function to convert If and IfElse statements into Puts statements
resolve :: Stmt -> [Stmt]
resolve (Puts str) = [Puts str]
resolve (Cout str) = [Cout str]
resolve (Cond (BoolLiteral True) stmts _) = concatMap resolve stmts
resolve (Cond (BoolLiteral False) _ elseStmts) = concatMap resolve elseStmts

eval :: Stmt -> IO ()
eval (Puts str) = putStrLn str
eval (Cout str) = putStr str

main :: IO ()
main = do
  -- let code = "(false).if? do\nputs 'Nein'\nend.elsif?(true) do\nputs 'Ja klar'\nend"
  code <- tail <$> getContents
  case mainParser code of
    Left err         -> print err
    Right statements -> do
      let resolvedStatements = concatMap resolve statements

      hPrint stderr statements
      hPrint stderr resolvedStatements

      case resolvedStatements of
        [] -> putStrLn "none"
        x -> mapM_ eval x
