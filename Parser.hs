module Parser where

import Control.Applicative
import Data.Char

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Int
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser x) =
     Parser $ \input -> do
       (rest, value) <- x input
       return (rest, f value)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input',  a) <- p1 input
      (input'', b) <- p2 input'
      return (input'', a b)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Semigroup a => Semigroup (Parser a) where
  (Parser p1) <> (Parser p2) =
    Parser $ \input -> do
      (input',  a) <- p1 input
      (input'', b) <- p2 input'
      return (input'', a <> b)

charP :: Char -> Parser Char
charP char = Parser f
  where f (c:cs) | c == char = Just (cs, c)
        f _                  = Nothing

stringP :: String -> Parser String
stringP text = sequenceA $ fmap charP text

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
    in Just (rest, token)

wsP :: Parser String
wsP = spanP isSpace

quoteP :: Parser Char
quoteP = charP '"'

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sep element = (:) <$> element <*> many (sep *> element) <|> pure []

stringLiteralP :: Parser String
stringLiteralP = quoteP *> spanP (/= '"') <* quoteP

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = (\a -> JsonBool $ f a) <$> ((stringP "true") <|> (stringP "false"))
  where f "true" = True
        f _      = False

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteralP

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> wsP *>
                           elements
                           <* wsP <* charP ']')
  where
    elements = sepByP (wsP *> charP ',' <* wsP) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> wsP *> pairs <* wsP <* charP '}')
  where pairs = sepByP (wsP *> charP ',' <* wsP) pair
        pair = (\key _ value -> (key, value)) <$> stringLiteralP <*> (wsP *> charP ':' <* wsP) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

