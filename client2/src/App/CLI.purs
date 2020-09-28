module App.CLI where

import Prelude
import Control.Alt ((<|>))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (class StringLike, string)

-- algebra for the CLI
data CLI
  = Help
  | Editor
  | EditorCanvas
  | Canvas
  | Play
  | Stop
  | Compile

cli ∷ ∀ s. StringLike s ⇒ Parser s CLI
cli =
  try help
    <|> try editorCanvas
    <|> try editor
    <|> try canvas
    <|> try play
    <|> try stop
    <|> compile

help ∷ ∀ s. StringLike s ⇒ Parser s CLI
help = (try $ string "help" <|> string "h") *> pure Help

editor ∷ ∀ s. StringLike s ⇒ Parser s CLI
editor = (try $ string "edit" <|> string "e") *> pure Editor

canvas ∷ ∀ s. StringLike s ⇒ Parser s CLI
canvas = (try $ string "canvas" <|> string "c") *> pure Canvas

play ∷ ∀ s. StringLike s ⇒ Parser s CLI
play = (try $ string "play" <|> string "p") *> pure Play

stop ∷ ∀ s. StringLike s ⇒ Parser s CLI
stop = (try $ string "stop" <|> string "s") *> pure Stop

compile ∷ ∀ s. StringLike s ⇒ Parser s CLI
compile = (try $ string "compile" <|> string "k") *> pure Compile

editorCanvas ∷ ∀ s. StringLike s ⇒ Parser s CLI
editorCanvas = (try $ string "split" <|> string "ec") *> pure EditorCanvas
