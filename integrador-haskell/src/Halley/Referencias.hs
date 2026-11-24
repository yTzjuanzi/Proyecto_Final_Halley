{-# LANGUAGE OverloadedStrings #-}

-- | Consultas sobre lo que se sabe de antemano: el personal y la estación.
module Halley.Referencias
  ( titularDe
  , buscarPersona
  , nombreDe
  , buscarPuerta
  , conecta
  , normalizarModulo
  , esExterior
  , nombreModulo
  , inferirFecha
  ) where

import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Halley.Tiempo (fechasEn)
import Halley.Tipos

-- | La persona a quien pertenece una tarjeta.
titularDe :: Referencias -> TarjetaId -> Maybe Persona
titularDe ref tarjeta = find (\p -> perTarjeta p == tarjeta) (refPersonas ref)

buscarPersona :: Referencias -> PersonaId -> Maybe Persona
buscarPersona ref pid = find (\p -> perId p == pid) (refPersonas ref)

-- | "valcarcel" -> "Irene Valcárcel"
nombreDe :: Referencias -> PersonaId -> Text
nombreDe ref pid = maybe pid perNombre (buscarPersona ref pid)

buscarPuerta :: Referencias -> Text -> Maybe Puerta
buscarPuerta ref texto = find (\p -> puId p == T.toUpper (T.strip texto)) (refPuertas ref)

-- | ¿La puerta une estos dos módulos (en cualquier sentido)?
conecta :: Puerta -> ModuloId -> ModuloId -> Bool
conecta p a b = (puA p, puB p) == (a, b) || (puA p, puB p) == (b, a)

-- | Devuelve el código oficial del módulo y si venía escrito distinto.
--   "LAB_B" -> Just ("LAB-B", True),  "GEN" -> Just ("GEN", False)
normalizarModulo :: Referencias -> Text -> Maybe (ModuloId, Bool)
normalizarModulo ref escrito
  | oficial `elem` refModulos ref = Just (oficial, oficial /= limpio)
  | otherwise = Nothing
  where
    limpio = T.strip escrito
    oficial = T.toUpper (T.replace "_" "-" (T.replace " " "-" limpio))

-- | El exterior no tiene sensores de presencia.
esExterior :: ModuloId -> Bool
esExterior m = m == "EXT"

-- | Nombres de la tabla del enunciado (§2); no vienen en ningún archivo.
nombreModulo :: ModuloId -> Text
nombreModulo m = fromMaybe m (lookup m nombres)
  where
    nombres =
      [ ("COM", "Comedor")
      , ("DOR", "Dormitorios")
      , ("LAB-A", "Laboratorio A")
      , ("LAB-B", "Laboratorio B")
      , ("GEN", "Generadores")
      , ("RAD", "Sala de radio")
      , ("ALM", "Almacén")
      , ("EXT", "Exterior (plataforma de hielo)")
      ]

-- | La fecha del caso: la que más se repite en los textos que sí traen fecha.
inferirFecha :: [Text] -> Maybe Day
inferirFecha textos = fmap fst (listToMaybe (sortOn (Down . snd) (Map.toList conteo)))
  where
    conteo = Map.fromListWith (+) [(dia, 1 :: Int) | t <- textos, dia <- fechasEn t]
