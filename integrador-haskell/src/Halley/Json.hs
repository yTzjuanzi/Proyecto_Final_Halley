{-# LANGUAGE OverloadedStrings #-}

-- | JSON mínimo, sin bibliotecas externas: un tipo algebraico, una función
--   que lo escribe y un analizador hecho con combinadores (ReadP, de base).
module Halley.Json
  ( JSON (..)
  , codificar
  , codificarPorLineas
  , analizar
  , campo
  , comoTexto
  , comoNumero
  , comoLista
  , comoObjeto
  , texto
  , entero
  , lista
  , quizas
  ) where

import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (readHex, showHex)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

data JSON
  = JNulo
  | JBool Bool
  | JEntero Integer
  | JNumero Double
  | JTexto Text
  | JLista [JSON]
  | JObjeto [(Text, JSON)]
  deriving (Eq, Show)

-- Atajos para construir valores.
texto :: Text -> JSON
texto = JTexto

entero :: Integral a => a -> JSON
entero = JEntero . fromIntegral

lista :: (a -> JSON) -> [a] -> JSON
lista f = JLista . map f

quizas :: (a -> JSON) -> Maybe a -> JSON
quizas = maybe JNulo

-- Consultas sobre un valor leído.
campo :: Text -> JSON -> Maybe JSON
campo k (JObjeto pares) = lookup k pares
campo _ _ = Nothing

comoTexto :: JSON -> Maybe Text
comoTexto (JTexto t) = Just t
comoTexto _ = Nothing

comoNumero :: JSON -> Maybe Double
comoNumero (JNumero d) = Just d
comoNumero (JEntero i) = Just (fromIntegral i)
comoNumero _ = Nothing

comoLista :: JSON -> Maybe [JSON]
comoLista (JLista xs) = Just xs
comoLista _ = Nothing

comoObjeto :: JSON -> Maybe [(Text, JSON)]
comoObjeto (JObjeto pares) = Just pares
comoObjeto _ = Nothing

-- ---------------------------------------------------------------------------
-- Escribir

codificar :: JSON -> Text
codificar j = case j of
  JNulo -> "null"
  JBool b -> if b then "true" else "false"
  JEntero i -> T.pack (show i)
  JNumero d -> T.pack (show d)
  JTexto t -> cadena t
  JLista xs -> "[" <> T.intercalate "," (map codificar xs) <> "]"
  JObjeto pares -> "{" <> T.intercalate "," [cadena k <> ":" <> codificar v | (k, v) <- pares] <> "}"

-- | Un campo por línea y, en las listas, un elemento por línea: fácil de leer
--   y de comparar con diff.
codificarPorLineas :: JSON -> Text
codificarPorLineas (JObjeto pares) = "{\n" <> T.intercalate ",\n" (map campoEnLinea pares) <> "\n}\n"
  where
    campoEnLinea (k, JLista xs@(_ : _)) =
      "  " <> cadena k <> ": [\n" <> T.intercalate ",\n" (map (("    " <>) . codificar) xs) <> "\n  ]"
    campoEnLinea (k, v) = "  " <> cadena k <> ": " <> codificar v
codificarPorLineas j = codificar j <> "\n"

cadena :: Text -> Text
cadena t = "\"" <> T.concatMap escapar t <> "\""
  where
    escapar c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ | ord c < 32 -> T.pack ("\\u" <> rellenar (showHex (ord c) ""))
        | otherwise -> T.singleton c
    rellenar h = replicate (4 - length h) '0' <> h

-- ---------------------------------------------------------------------------
-- Leer: cada parte del JSON es un pequeño analizador y se combinan entre sí

analizar :: Text -> Either Text JSON
analizar t = case [v | (v, "") <- readP_to_S (valor <* eof) (T.unpack t)] of
  (v : _) -> Right v
  [] -> Left "no es JSON válido"

valor :: ReadP JSON
valor = skipSpaces *> unValor <* skipSpaces
  where
    unValor =
      objeto
        <++ arreglo
        <++ (JTexto <$> cadenaJ)
        <++ numero
        <++ (JBool True <$ string "true")
        <++ (JBool False <$ string "false")
        <++ (JNulo <$ string "null")

objeto :: ReadP JSON
objeto = JObjeto <$> between (char '{' <* skipSpaces) (char '}') (sepBy par (char ','))
  where
    par = do
      skipSpaces
      clave <- cadenaJ
      skipSpaces
      _ <- char ':'
      v <- valor
      pure (clave, v)

arreglo :: ReadP JSON
arreglo = JLista <$> between (char '[' <* skipSpaces) (char ']') (sepBy valor (char ','))

cadenaJ :: ReadP Text
cadenaJ = T.pack <$> (char '"' *> manyTill caracter (char '"'))
  where
    caracter = (char '\\' *> escape) <++ get
    escape = do
      c <- get
      case c of
        'n' -> pure '\n'
        't' -> pure '\t'
        'r' -> pure '\r'
        'b' -> pure '\b'
        'f' -> pure '\f'
        'u' -> do
          digitos <- count 4 (satisfy isHexDigit)
          case readHex digitos of
            [(n, "")] -> pure (chr n)
            _ -> pfail
        _ -> pure c -- \" \\ \/

numero :: ReadP JSON
numero = do
  s <- munch1 (\c -> isDigit c || c `elem` ("+-.eE" :: String))
  case (readMaybe s, readMaybe s) of
    (Just i, _) -> pure (JEntero i)
    (_, Just d) -> pure (JNumero d)
    _ -> pfail
