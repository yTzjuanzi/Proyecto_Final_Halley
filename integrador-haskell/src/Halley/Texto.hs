{-# LANGUAGE OverloadedStrings #-}

-- | Utilidades de texto. Funciones puras que no saben nada del caso.
module Halley.Texto
  ( normalizar
  , palabras
  , oraciones
  , contiene
  , distancia
  , separar
  , leerEntero
  , leerDecimal
  , compatibles
  , mostrar
  ) where

import Data.Array (array, listArray, (!))
import Data.Char (isAlphaNum, toLower)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | Minúsculas, sin tildes y sin espacios sobrantes: "  Valcárcel " -> "valcarcel".
normalizar :: Text -> Text
normalizar = T.strip . T.map quitarTilde . T.toLower

quitarTilde :: Char -> Char
quitarTilde c = case c of
  'á' -> 'a'
  'é' -> 'e'
  'í' -> 'i'
  'ó' -> 'o'
  'ú' -> 'u'
  'ü' -> 'u'
  'ñ' -> 'n'
  _ -> c

-- | Palabras normalizadas, sin signos. Conserva el guion: "¿LAB-B?" -> ["lab-b"].
palabras :: Text -> [Text]
palabras = T.words . T.map soloLetras . normalizar
  where
    soloLetras c = if isAlphaNum c || c == '-' then c else ' '

-- | Parte un texto en oraciones (por . ? ! ;).
oraciones :: Text -> [Text]
oraciones = filter (not . T.null) . map T.strip . T.split (`elem` (".?!;" :: String))

-- | ¿Aparece esta secuencia de palabras dentro de la otra?
contiene :: [Text] -> [Text] -> Bool
contiene frase enTexto = frase `isInfixOf` enTexto

-- | Distancia de edición (Levenshtein): cuántas letras hay que cambiar, agregar
--   o quitar para pasar de una palabra a otra.  distancia "luka" "lucas" == 2
--
--   La tabla se define celda por celda; gracias a la evaluación perezosa,
--   cada celda se calcula solo cuando otra la necesita.
distancia :: Text -> Text -> Int
distancia a b = tabla ! (n, m)
  where
    n = T.length a
    m = T.length b
    letraA = listArray (1, n) (T.unpack a)
    letraB = listArray (1, m) (T.unpack b)
    tabla = array ((0, 0), (n, m)) [((i, j), celda i j) | i <- [0 .. n], j <- [0 .. m]]
    celda i 0 = i -- quitar i letras
    celda 0 j = j -- agregar j letras
    celda i j =
      minimum
        [ tabla ! (i - 1, j) + 1 -- quitar una letra
        , tabla ! (i, j - 1) + 1 -- agregar una letra
        , tabla ! (i - 1, j - 1) + (if letraA ! i == letraB ! j then 0 else 1) -- cambiarla
        ]

-- | Separa los campos de una línea: separar ',' "a, b" == ["a", "b"].
separar :: Char -> Text -> [Text]
separar sep = map T.strip . T.split (== sep)

leerEntero :: Text -> Maybe Integer
leerEntero = readMaybe . T.unpack . T.strip

leerDecimal :: Text -> Maybe Double
leerDecimal = readMaybe . T.unpack . T.strip

-- | Opciones conocidas que encajan con un valor dañado: los caracteres
--   ilegibles (? # *) valen por cualquiera y el valor puede venir cortado.
--   compatibles "AK-0?" tarjetas == ["AK-05"],  compatibles "DO" modulos == ["DOR"]
compatibles :: Text -> [Text] -> [Text]
compatibles danado = filter encaja
  where
    encaja opcion =
      not (T.null danado)
        && T.length danado <= T.length opcion
        && and [igual x y | (x, y) <- T.zip danado opcion]
    igual x y = x `elem` ("?#*" :: String) || toLower x == toLower y

mostrar :: Show a => a -> Text
mostrar = T.pack . show
