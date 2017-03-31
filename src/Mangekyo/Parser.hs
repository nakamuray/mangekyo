{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Mangekyo.Parser where

import Control.Applicative hiding (many)
import Data.Char (isHexDigit)
import Numeric (readHex)
import Data.Scientific (Scientific)
import Text.Parser.Expression
import Text.Trifecta hiding (symbol, whiteSpace, parens, braces, comma, brackets)
import Text.Trifecta.Delta (Delta(Directed))

import Data.Word (Word8)

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BU

import Mangekyo.AST

type SourceName = String

parseMangekyo :: SourceName -> T.Text -> Either String Program
parseMangekyo n t =
    case parseString program (Directed (BU.fromString n) 0 0 0 0) $ T.unpack t of
        Failure e  -> Left $ show e
        Success p -> Right p

parseMangekyoFile :: FilePath -> IO (Either String Program)
parseMangekyoFile fp = do
    r <- parseFromFileEx program fp
    case r of
        Failure e  -> return $ Left $ show e
        Success p -> return $ Right p

program :: Parser Program
program = Program <$> block <* eof

block, block1 :: Parser [Expression]
block = blankLines *> whiteSpace *> expression `sepEndBy` ((semicolon <|> symbol "\n") <* blankLines <* whiteSpace)
block1 = blankLines *> whiteSpace *> expression `sepEndBy1` ((semicolon <|> symbol "\n") <* blankLines <* whiteSpace)

expression :: Parser Expression
expression = buildExpressionParser table term

symbol :: String -> Parser String
symbol name = lexeme (string name)

parens, braces, brackets :: Parser a -> Parser a
parens p   = between (symbol "(") (symbol ")") p
braces p   = between (symbol "{") (symbol "}") p
brackets p = between (symbol "[") (symbol "]") p

comma, colon, semicolon, dot :: Parser String
comma      = symbol ","
colon      = symbol ":"
semicolon = symbol ";"
dot        = symbol "."

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

lexeme' :: Parser a -> Parser a
lexeme' p = p <* whiteSpace'

whiteSpace :: Parser ()
whiteSpace = skipMany (simpleSpaces <|> newlineFollowingBackspace <|> oneLineComment <?> "white spaces")
  where
    simpleSpaces = oneOf " \t\r" >> return ()
    newlineFollowingBackspace = string "\\\n" >> return ()
    oneLineComment = do
        char '#'
        skipMany (satisfy (/= '\n'))
        return ()

whiteSpace' :: Parser ()
whiteSpace' = blankLines >> whiteSpace

blankLines :: Parser ()
blankLines = do
    many $ try (whiteSpace >> newline)
    return ()


table :: OperatorTable Parser Expression
-- TODO: more think about operator precedence
table = [ [unary "-" "negative", unary "+" "number"]
        , [binary "." AssocLeft]
        , [binary "^." AssocLeft]
        , [binary' "*" AssocLeft, binary' "/" AssocLeft, binary' "%" AssocLeft]
        , [binary' "+" AssocLeft, binary' "-" AssocLeft]
        , [binary' ">" AssocLeft, binary' "<" AssocLeft]
        , [binary "==" AssocNone, binary "!=" AssocNone
          , binary "=~" AssocNone, binary "!~" AssocNone]
        , [unary "!" "!"]
        , [binary "&&" AssocNone, binary "||" AssocNone]
        , [binary ".~" AssocRight, binary "%~" AssocRight]
        , [binary' "&" AssocLeft]
        , [binary' "$" AssocRight, binary' "?" AssocLeft]
        , [binary ".=" AssocRight]
        ]

unary :: String -> T.Text -> Operator Parser Expression
unary name fun = Prefix $ do
    loc <- getLocation
    reservedOp name
    return $ \x -> FuncCall loc (Variable fun) x
  <?> "operator"

binary :: String -> Assoc -> Operator Parser Expression
binary name assoc = Infix (do
    loc <- getLocation
    reservedOp2 name
    return $ \x y -> FuncCall loc (FuncCall loc (Variable $ T.pack name) x) y
  <?> "binary operator") assoc

binary' :: String -> Assoc -> Operator Parser Expression
binary' prefix assoc = Infix (do
    loc <- getLocation
    name <- userDefinedOp2 prefix
    return $ \x y -> FuncCall loc (FuncCall loc (Variable name) x) y
  <?> "binary operator") assoc

reservedOp :: String -> Parser ()
reservedOp name = try $ lexeme $ reservedOp' name

reservedOp2 :: String -> Parser ()
reservedOp2 name = try $ lexeme' $ reservedOp' name

reservedOp' :: String -> Parser ()
reservedOp' name = do
    string name
    notFollowedBy opLetter <?> ("end of " ++ show name)

opLetter :: Parser Char
opLetter = oneOf "!%&*+-/:$<=>?|~^."

userDefinedOp2 :: String -> Parser T.Text
userDefinedOp2 prefix = try $ lexeme' $ do
    string prefix
    cs <- many opLetter
    return $ T.pack $ prefix ++ cs

term :: Parser Expression
term = do
    es <- term' `sepBy1` symbol "|"
    case es of
        []  -> error "sepBy1 ensures thre is at least one element"
        [e] -> return e
        _   -> return $ pipe es

pipe :: [Expression] -> Expression
pipe es = Pipe $ map pipeBody es

term' :: Parser Expression
term' = go <$> many1 term''
     <?> "expression"
  where
    go (e:es) = go' e es
    go [] = error "many1 ensures there is at least one element"
    go' (loc, acc) ((loc', e): es) = go' (loc', FuncCall loc acc e) es
    go' (_, acc) [] = acc

pipeBody :: Expression -> Expression
pipeBody e@(Function _ _) = e
-- treat expression as function, such as "_ -> { e }"
-- XXX: may I have to do this conversion at interpreter, instead?
pipeBody e = Function (PVariable "_" Nothing) [e]

term'' :: Parser (Location, Expression)
term'' = lexeme $ do
    e <- try function <|> try parensesEnclosed <|> array <|> object <|> variable <|> string_ <|> number <|> lens
    loc <- getLocation
    whiteSpace
    return (loc, e)
  <?> "expression"

parensesEnclosed :: Parser Expression
parensesEnclosed = do
    t <- parens tupleBody
    case t of
        -- single is just a value
        Tuple [o] -> return o
        _         -> return t

tupleBody :: Parser Expression
tupleBody = Tuple <$> makeArrayParser expression

-- [ pattern -> ] { expression [; expression ...] }
function :: Parser Expression
function = uncurry Function <$> (try function' <|> function'')

function' :: Parser (Pattern, [Expression])
function' = do
    pat <- funcPattern
    es <- braces block1
    return $ (pat, es)
  <?> "function"

funcPattern :: Parser Pattern
funcPattern = option (PVariable "_" Nothing) $ try $ do
    pat <- pattern
    symbol "->"
    return pat
  <?> "function parameters"

function'' :: Parser (Pattern, [Expression])
function'' = do
    pat <- try pattern
    symbol "->"
    e <- expression
    return $ (pat, [e])
  <?> "function"

variable :: Parser Expression
variable = Variable <$> (varName <|> varOp)
    <?> "variable"

string_ :: Parser Expression
string_ = String <$> string_'

string_' :: Parser T.Text
string_' = T.pack <$> p_string
    <?> "string"

number :: Parser Expression
number = Number <$> (number' <|> negativeNumber')

number' :: Parser Scientific
number' = do
    d <- many1 digit
    mdot <- optional $ char '.'
    case mdot of
        Just _ -> do
            n <- many1 digit
            return $ read $ d ++ "." ++ n
        Nothing -> return $ read d
  <?> "number"

negativeNumber' :: Parser Scientific
negativeNumber' = do
    string "-"
    s <- number'
    return (-s)

tuple :: Parser Expression
tuple = Tuple <$> parens (makeArrayParser expression) <?> "tuple"

array :: Parser Expression
array = Array <$> brackets (makeArrayParser expression)

makeArrayParser :: Parser a -> Parser [a]
makeArrayParser p = (whiteSpace' *> lexeme' p `sepEndBy` lexeme' comma)
    <?> "array"

object :: Parser Expression
object = Object <$> makeObjectParser expression

makeObjectParser :: Parser a -> Parser [(T.Text, a)]
makeObjectParser p = (braces $ kv `sepEndBy` lexeme' comma)
    <?> "object"
  where
    kv = do
        whiteSpace'
        k <- varName <|> string_'
        whiteSpace'
        symbol ":"
        whiteSpace'
        v <- p
        whiteSpace'
        return (k, v)

lens :: Parser Expression
lens = Lens <$> (try textLens <|> numberLens)

textLens, numberLens :: Parser Expression
textLens = String <$> (string "@" >> varName)

numberLens = do
    string "@"
    n <- number
    return n

pattern :: Parser Pattern
pattern = lexeme (try pVariable <|> pString <|> pNumber <|> pTuple <|> pArray <|> pObject)

pVariable :: Parser Pattern
pVariable = PVariable <$> (varName <|> varOp) <*> optional (symbol "@" >> pattern)

pString :: Parser Pattern
pString = PString <$> string_'

pNumber :: Parser Pattern
pNumber = PNumber <$> number'

pTuple :: Parser Pattern
pTuple = do
    t <- parens (makeArrayParser pattern)
    case t of
        -- single is just a value
        [o] -> return o
        _   -> return (PTuple t)

pArray :: Parser Pattern
pArray = PArray <$> brackets (makeArrayParser pattern)

pObject :: Parser Pattern
pObject = PObject <$> makeObjectParser pattern

varName :: Parser T.Text
varName = lexeme $ do
    h <- oneOf $ "_" ++ ['a'..'z'] ++ ['A'..'Z']
    t <- many $ oneOf "_" <|> alphaNum
    return $ T.pack $ h : t

varOp :: Parser T.Text
varOp = lexeme $ parens $ T.pack <$> many opLetter

subscripts :: Parser [T.Text]
subscripts = do
    char '.'
    (T.pack <$> many (char '_' <|> alphaNum) <|> (T.pack <$> p_string)) `sepBy1` char '.'

tryAll :: [Parser a] -> Parser a
tryAll ps = foldl1 (<|>) $ map try ps

toWord :: Char -> Word8
toWord = toEnum . fromEnum

toChar :: Word8 -> Char
toChar = toEnum . fromEnum


-- XXX: copied from json-0.7:Text.JSON.Parsec
--tok              :: Parser a -> Parser a
--tok p             = p <* spaces

p_string         :: Parser String
--p_string          = between (tok (char '"')) (tok (char '"')) (many p_char)
p_string          = between (char '"') (char '"') (many p_char)
  where p_char    =  (char '\\' >> p_esc)
                 <|> (satisfy (\x -> x /= '"' && x /= '\\'))

        p_esc     =  ('"'   <$ char '"')
                 <|> ('\\'  <$ char '\\')
                 <|> ('/'   <$ char '/')
                 <|> ('\b'  <$ char 'b')
                 <|> ('\f'  <$ char 'f')
                 <|> ('\n'  <$ char 'n')
                 <|> ('\r'  <$ char 'r')
                 <|> ('\t'  <$ char 't')
                 <|> (char 'u' *> p_uni)
                 <?> "escape character"

        p_uni     = check =<< count 4 (satisfy isHexDigit)
          where check x | code <= max_char  = pure (toEnum code)
                        | otherwise         = empty
                  where code      = fst $ head $ readHex x
                        max_char  = fromEnum (maxBound :: Char)

many1 :: Parser a -> Parser [a]
many1 p = do
    r <- p
    rs <- many p
    return $ r : rs


getLocation :: Parser Location
getLocation = do
    d <- position
    l <- line
    return (d, l)
