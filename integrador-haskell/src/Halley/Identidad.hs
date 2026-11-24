{-# LANGUAGE OverloadedStrings #-}

-- | Una sola noción de identidad: cada forma de nombrar a alguien ("Petrov",
--   "jefe", "la doctora", "Luka") se resuelve a una persona, siempre con su
--   nivel de confianza y el motivo. También se extraen de los textos libres
--   las afirmaciones que el sistema sabe reconocer.
module Halley.Identidad
  ( idDePersona
  , construirAlias
  , identificar
  , analizarTexto
  ) where

import Data.List (isPrefixOf, nub, nubBy, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Halley.Texto
import Halley.Tipos

-- | El identificador de una persona es su apellido: "Irene Valcárcel" -> "valcarcel".
idDePersona :: Text -> PersonaId
idDePersona nombre = case reverse (palabras nombre) of
  (apellido : _) -> apellido
  [] -> normalizar nombre

-- ---------------------------------------------------------------------------
-- Alias: las formas conocidas de nombrar a cada persona

-- | Apodos que la tripulación usa para un cargo de personal.csv.
apodosDeCargo :: [(Text, Text)]
apodosDeCargo =
  [ ("jefe", "jefe de estacion")
  , ("doc", "medico")
  , ("doctor", "medico")
  ]

-- | Títulos que, seguidos de un apellido en algún texto, enseñan un alias:
--   si la bitácora dice "la doctora Valcárcel", "doctora" es Valcárcel.
titulos :: [Text]
titulos = ["doctora", "doctor", "ingeniera", "ingeniero"]

construirAlias :: [Persona] -> [Origen] -> [Alias]
construirAlias personas textos = porNombre ++ porCargo ++ porTitulo
  where
    -- 1. Nombre completo, nombre de pila y apellido.
    porNombre = concatMap aliasDeNombre personas
    aliasDeNombre p =
      let todas = palabras (perNombre p)
       in Alias todas (perId p) Exacta "nombre completo en personal.csv"
            : [Alias [w] (perId p) Exacta "nombre de pila en personal.csv" | w <- take 1 todas, length todas > 1]
            ++ [Alias [perId p] (perId p) Exacta "apellido en personal.csv" | length todas > 1]

    -- 2. El cargo completo ("jefe de estacion") y sus apodos ("jefe").
    porCargo =
      [ Alias (palabras (perCargo p)) (perId p) PorCargo ("es su cargo en personal.csv")
      | p <- personas
      ]
        ++ [ Alias [apodo] (perId p) PorCargo ("'" <> apodo <> "' designa el cargo '" <> perCargo p <> "'")
           | (apodo, cargo) <- apodosDeCargo
           , p <- personas
           , normalizar (perCargo p) == cargo
           ]

    -- 3. Títulos vistos junto a un apellido en la radio o la bitácora.
    vistos =
      [ (titulo, perId p, idOrigen o)
      | o <- textos
      , let ps = palabras (oTexto o)
      , (titulo, siguiente) <- zip ps (drop 1 ps)
      , titulo `elem` titulos
      , p <- personas
      , siguiente == perId p
      ]
    porTitulo =
      [ Alias [titulo] pid PorContexto ("en " <> donde <> " aparece '" <> titulo <> " " <> pid <> "'")
      | (titulo, pid, donde) <- nubBy (\(a, _, _) (b, _, _) -> a == b) vistos
      , length (nub [q | (t, q, _) <- vistos, t == titulo]) == 1 -- solo si no es ambiguo
      ]

-- ---------------------------------------------------------------------------
-- Identificar a quien habla o escribe

-- | Primero busca un alias exacto; si no hay, uno que se escriba muy parecido.
identificar :: Referencias -> Text -> Identificacion
identificar ref escrito =
  case nubBy (\a b -> alPersona a == alPersona b) exactos of
    [a] -> Identificacion (Just (alPersona a)) escrito (alConfianza a) (alMotivo a)
    [] -> parecido
    varios -> Identificacion Nothing escrito SinResolver ("ambiguo entre " <> T.intercalate ", " (map alPersona varios))
  where
    ps = dropWhile (`elem` ["el", "la"]) (palabras escrito)
    exactos = sortOn alConfianza [a | a <- refAlias ref, alPalabras a == ps]

    -- Si exactamente una persona tiene un nombre o apellido a distancia <= 2.
    parecido = case ps of
      [p]
        | T.length p >= 4 ->
            case nubBy (\(a, _, _) (b, _, _) -> a == b) (candidatos p) of
              [(pid, nombre, d)] ->
                Identificacion
                  (Just pid)
                  escrito
                  Probable
                  ("'" <> escrito <> "' se parece a '" <> nombre <> "' (distancia de edición " <> mostrar d <> "); identificación NO segura")
              _ -> sinIdentificar
      _ -> sinIdentificar
    candidatos p =
      [ (perId per, nombre, d)
      | per <- refPersonas ref
      , nombre <- take 1 (palabras (perNombre per)) ++ [perId per]
      , let d = distancia p nombre
      , d <= 2
      ]
    sinIdentificar = Identificacion Nothing escrito SinResolver "no coincide con ningún nombre, cargo ni alias conocido"

-- ---------------------------------------------------------------------------
-- Leer un texto libre: a quién menciona y qué afirma

analizarTexto :: [Alias] -> Identificacion -> Text -> TextoLibre
analizarTexto alias quien dicho =
  TextoLibre
    { tlQuien = quien
    , tlTexto = dicho
    , tlMenciones = personasMencionadas alias (palabras dicho)
    , tlAfirmaciones = nub (concatMap (afirmaciones alias (idPersona quien)) (oraciones dicho))
    }

-- | Recorre las palabras y, en cada posición, se queda con la frase más larga
--   del diccionario que empiece ahí. Sirve para personas y para módulos.
buscarFrases :: [([Text], a)] -> [Text] -> [a]
buscarFrases diccionario = recorrer
  where
    ordenado = sortOn (negate . length . fst) [d | d@(frase, _) <- diccionario, not (null frase)]
    recorrer [] = []
    recorrer ps@(_ : resto) =
      case [(frase, v) | (frase, v) <- ordenado, frase `isPrefixOf` ps] of
        ((frase, v) : _) -> v : recorrer (drop (length frase) ps)
        [] -> recorrer resto

personasMencionadas :: [Alias] -> [Text] -> [Identificacion]
personasMencionadas alias ps =
  nubBy (\a b -> idPersona a == idPersona b)
    [ Identificacion (Just (alPersona a)) (T.unwords (alPalabras a)) (alConfianza a) (alMotivo a)
    | a <- buscarFrases [(alPalabras a, a) | a <- alias] ps
    ]

-- | Cómo se nombra cada módulo en los textos (tabla del enunciado, §2).
--   Las frases con Nothing se consumen pero NO son un módulo.
frasesDeModulo :: [([Text], Maybe ModuloId)]
frasesDeModulo =
  [ (["puerta", "exterior"], Nothing)
  , (["por", "radio"], Nothing)
  , (["comedor"], Just "COM")
  , (["dormitorios"], Just "DOR")
  , (["dormitorio"], Just "DOR")
  , (["laboratorio", "a"], Just "LAB-A")
  , (["lab-a"], Just "LAB-A")
  , (["laboratorio", "b"], Just "LAB-B")
  , (["lab-b"], Just "LAB-B")
  , (["el", "b"], Just "LAB-B")
  , (["generadores"], Just "GEN")
  , (["radio"], Just "RAD")
  , (["almacen"], Just "ALM")
  , (["exterior"], Just "EXT")
  ]

modulosMencionados :: [Text] -> [ModuloId]
modulosMencionados ps = nub (catMaybes (buscarFrases frasesDeModulo ps))

-- | Reglas para una oración. Son pocas y explícitas, a propósito:
--
--   | Si la oración...                                   | se afirma            |
--   |----------------------------------------------------|----------------------|
--   | dice "se fue la luz"                               | SeFueLaLuz           |
--   | dice "sin luz" o "sin energia"                     | SinLuz (módulo?)     |
--   | dice "volvio la energia" o "volvio la luz"         | VolvioLaLuz          |
--   | dice "perdio" y "tarjeta" y nombra a alguien       | PerdioTarjeta        |
--   | dice "puerta exterior"                             | PuertaExterior       |
--   | habla en 1ª persona (aqui/sigo/estoy/escribo)      | EstaEn quien-habla   |
--   |   y nombra un solo módulo                          |                      |
--   | dice sigue/esta/estaba, nombra a UNA persona       | EstaEn esa-persona   |
--   |   y a un solo módulo                               |                      |
afirmaciones :: [Alias] -> Maybe PersonaId -> Text -> [Afirmacion]
afirmaciones alias quienHabla oracion =
  concat
    [ [SeFueLaLuz | dice ["se", "fue", "la", "luz"]]
    , [SinLuz (unico modulos) | dice ["sin", "luz"] || dice ["sin", "energia"]]
    , [VolvioLaLuz | dice ["volvio", "la", "energia"] || dice ["volvio", "la", "luz"]]
    , [PerdioTarjeta p | algunaDe ["perdio", "perdi"], algunaDe ["tarjeta"], p <- otrasPersonas]
    , [PuertaExterior | dice ["puerta", "exterior"]]
    , [EstaEn h m | Just h <- [quienHabla], algunaDe ["aqui", "sigo", "estoy", "escribo"], Just m <- [unico modulos]]
    , [EstaEn p m | algunaDe ["sigue", "esta", "estaba"], Just p <- [unico otrasPersonas], Just m <- [unico modulos]]
    ]
  where
    ps = palabras oracion
    dice frase = contiene frase ps
    algunaDe ws = any (`elem` ps) ws
    modulos = modulosMencionados ps
    otrasPersonas = [p | Just p <- map idPersona (personasMencionadas alias ps), Just p /= quienHabla]
    unico [x] = Just x
    unico _ = Nothing
