{-# LANGUAGE OverloadedStrings #-}

-- | Lecturas en vivo (tarjetas, presencia y temperatura), para la capacidad C3.
--
--   Entra una línea JSON por lectura:
--       {"fuente":"tarjetas","linea":"2041-06-14T03:10:00,JS-01,P7,GEN,ALM,OK"}
--   y sale una línea JSON con el evento normalizado y sus problemas.
--
--   'pasoVivo' es una función pura: (estado, línea) -> (estado nuevo, respuesta).
--   El bucle que la repite por cada línea está en Main (el borde de IO).
module Halley.Vivo
  ( EstadoVivo
  , estadoInicial
  , pasoVivo
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Halley.Fuentes
import Halley.Integracion
import Halley.Json
import Halley.Salida (eventoJSON, problemaJSON)
import Halley.Texto (mostrar)
import Halley.Tipos
import Halley.Validacion

data EstadoVivo = EstadoVivo
  { viReferencias :: Referencias
  , viVistos :: Map.Map Clave Text -- para detectar duplicados
  , viContador :: Map.Map Fuente Int -- número de lectura por fuente
  , viReportes :: [(PersonaId, UTCTime, Text)] -- tarjetas reportadas perdidas
  }

-- | El estado parte de lo que ya se integró desde los archivos.
estadoInicial :: Resultado -> EstadoVivo
estadoInicial r =
  EstadoVivo
    { viReferencias = reReferencias r
    , viVistos = Map.fromList [(clave e, evId e) | e <- reEventos r]
    , viContador = Map.empty
    , viReportes = reportesDePerdida (reEventos r)
    }

pasoVivo :: EstadoVivo -> Text -> (EstadoVivo, JSON)
pasoVivo est linea =
  case analizar (T.dropWhile (== '\xFEFF') linea) of -- algunos programas anteponen un BOM
    Left _ -> (est, fallo "la línea recibida no es JSON")
    Right j -> case (campo "fuente" j >>= comoTexto, campo "linea" j >>= comoTexto) of
      (Just "tarjetas", Just crudo) -> procesar Tarjetas leerLineaTarjeta crudo
      (Just "presencia", Just crudo) -> procesar Presencia leerLineaPresencia crudo
      (Just "temperatura", Just crudo) -> procesar Temperatura leerLineaTemperatura crudo
      (Just otra, Just _) -> (est, fallo ("fuente no admitida en vivo: '" <> otra <> "' (solo tarjetas, presencia, temperatura)"))
      _ -> (est, fallo "faltan los campos 'fuente' y 'linea'")
  where
    ref = viReferencias est
    fallo mensaje = JObjeto [("ok", JBool False), ("error", texto mensaje)]

    procesar fuente leerLinea crudo =
      let n = Map.findWithDefault 0 fuente (viContador est) + 1
          o = Origen fuente EnVivo n crudo
          est' = est {viContador = Map.insert fuente n (viContador est)}
       in case leerLinea ref o of
            Usada e avisos -> revisar est' e avisos
            Descartada _ p -> (est', respuesta o "descartado" [] [p])
            _ -> (est', respuesta o "sin_datos" [] [])

    -- Las mismas revisiones de los archivos que tienen sentido para una sola lectura.
    revisar est' e avisos =
      case Map.lookup (clave e) (viVistos est') of
        Just primero -> (est', respuesta (evOrigen e) "descartado" [] (avisos ++ [duplicado e primero]))
        Nothing ->
          let (marcado, perdida) = marcarSiEstaPerdida ref (viReportes est') e
              problemas = avisos ++ perdida ++ revisarAccesos ref [marcado]
           in ( est' {viVistos = Map.insert (clave e) (evId e) (viVistos est')}
              , respuesta (evOrigen e) "usado" [marcado] problemas
              )

    respuesta o estado eventos problemas =
      JObjeto
        [ ("ok", JBool True)
        , ("origen", texto (idOrigen o))
        , ("estado", texto estado)
        , ("eventos", lista eventoJSON eventos)
        , ("problemas", lista problemaJSON (zip [idOrigen o <> "#" <> mostrar k | k <- [1 :: Int ..]] problemas))
        ]
