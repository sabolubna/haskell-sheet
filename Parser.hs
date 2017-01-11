{-
parser.hs
Zadanie z parsowania funkcyjnego do wykładu "Specyfikacje formalne
i programy funkcyjne". Marcin Szlenk 2015

Na podstawie:
Graham Hutton, Programming in Haskell, Cambridge University Press, 2007
http://www.cs.nott.ac.uk/~gmh/book.html
("Functional parsing library" i "Expression parser")

Uwaga: Wymaga ghc w wersji 7.10 lub nowszej.
-}
module Parser where
import Data.Char

data Parser a = P (String -> [(a, String)])

-- parser 'return' i operator sekwencji
-- (od wersji ghc 7.10 klasa Monad jest podklasą klasy Applicative)

instance Functor Parser where
  fmap f m = m >>= pure . f

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)

instance Monad Parser where
  return = pure
  p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

-- podstawowe parsery

item  :: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\inp -> [])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

-- dalsze prymitywy

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

-- wyrażenia arytmetyczne

{-
expr       ::= term [plus_term | minus_term]*
plus_term  ::= '+' term
minus_term ::= '-' term
term       ::= factor [mul_factor | div_factor]*
mul_factor ::= '*' factor
div_factor ::= '/' factor
factor     ::= digit | '(' expr ')'
digit      ::= '0' | '1' | ... | '9'
-}

expr :: Parser Double
expr = failure -- TO DO

plus_term = failure -- TO DO

minus_term = failure -- TO DO

term :: Parser Double
term = failure -- TO DO

mul_factor = failure -- TO DO

div_factor = failure -- TO DO

factor :: Parser Double
factor = do d <- digit
            return ((read [d]) :: Double)
           +++
             do char '('
                e <- expr
                char ')'
                return e

eval :: String -> Double
eval inp = case parse expr inp of
              [(n, [])] -> n
              [(_, out)] -> error ("nieskonsumowane " ++ out)
              [] -> error "bledne wejscie"
