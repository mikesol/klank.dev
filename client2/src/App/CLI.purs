module App.Cli where

import Prelude
import Control.Alt ((<|>))
import Data.Array (some, singleton, many)
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as SCU
import Text.Parsing.Parser (Parser, ParserT)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (class StringLike, oneOf, satisfy, string)

whiteSpace' :: forall s m. StringLike s => Monad m => ParserT s m String
whiteSpace' = do
  cs <- some $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
  pure $ SCU.fromCharArray cs

-- algebra for the CLI
data CLI
  = Help
  | Login
  | Logout
  | SignUp
  | Home
  | Ls
  | Rename String String
  | Store String String

cli ∷ ∀ s. StringLike s ⇒ Parser s CLI
cli =
  try login
    <|> try logout
    <|> try signup
    <|> try home
    <|> try rename
    <|> try help
    <|> try list
    <|> store

help ∷ ∀ s. StringLike s ⇒ Parser s CLI
help = (try $ string "help" <|> string "h") *> pure Help

login ∷ ∀ s. StringLike s ⇒ Parser s CLI
login = string "login" *> pure Login

logout ∷ ∀ s. StringLike s ⇒ Parser s CLI
logout = string "logout" *> pure Logout

signup ∷ ∀ s. StringLike s ⇒ Parser s CLI
signup = string "signup" *> pure Login

home ∷ ∀ s. StringLike s ⇒ Parser s CLI
home = string "home" *> pure Home

--------------
-- chars
--------------
upper :: Array Char
upper = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]

lower :: Array Char
lower = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]

digits :: Array Char
digits = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

toCA ∷ ∀ s. Char → Parser s (Array Char)
toCA = pure <<< singleton

name ∷ ∀ s. StringLike s ⇒ Parser s String
name =
  fromCharArray
    <$> ( (<>)
          <$> (oneOf (upper <> lower <> [ '_' ]) >>= toCA)
          <*> (many (oneOf $ upper <> lower <> digits <> [ '_' ]))
      )

list ∷ ∀ s. StringLike s ⇒ Parser s CLI
list = string "list" *> whiteSpace' *> pure Ls

rename ∷ ∀ s. StringLike s ⇒ Parser s CLI
rename = string "rename" *> whiteSpace' *> (Rename <$> name <*> (whiteSpace' *> name))

store ∷ ∀ s. StringLike s ⇒ Parser s CLI
store =
  (try $ string "store" <|> string "u")
    *> whiteSpace'
    *> (Store <$> name <*> (whiteSpace' *> name))
