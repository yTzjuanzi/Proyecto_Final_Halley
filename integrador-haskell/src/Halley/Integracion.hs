{-# LANGUAGE OverloadedStrings #-}

-- | C1 completa como UNA función pura:
--
--       textos de los 8 archivos  ->  línea de tiempo + problemas + estado de cada línea
--
--   Los pasos están numerados en 'integrar'.
module Halley.Integracion
  ( Resultado (..)
  , Registro (..)
  , integrar
  ) where

import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (fromGregorian, toGregorian)
import Halley.Fuentes
import Halley.Identidad (construirAlias)
import Halley.Referencias (inferirFecha)
import Halley.Texto (mostrar)
import Halley.Tiempo (fechasEn)
import Halley.Tipos
import Halley.Validacion (validar)

data Resultado = Resultado
  { reReferencias :: Referencias
  , reEventos :: [Evento] -- la línea de tiempo, en orden
  , reProblemas :: [(Text, Problema)] -- cada problema con su código PRB-NNN
  , reRegistros :: [Registro] -- cada línea original y qué pasó con ella
  , reSupuestos :: [Text]
  }

data Registro = Registro
  { regOrigen :: Origen
  , regEstado :: Text -- usado | descartado | encabezado | vacia
  , regMotivos :: [Text] -- códigos de los problemas que la mencionan
  }

integrar :: [Problema] -> Map.Map Fuente Text -> Resultado
integrar avisosDeLectura archivos =
  Resultado
    { reReferencias = ref
    , reEventos = eventos
    , reProblemas = numerados
    , reRegistros = sortOn enOrden (map (registroDe numerados) lecturas)
    , reSupuestos = supuestos ref
    }
  where
    texto f = Map.findWithDefault "" f archivos
    enOrden g = (oFuente (regOrigen g), oLinea (regOrigen g))

    -- 1. Referencias: quién es quién y cómo es la estación.
    (personas, lecturasPersonal) = leerPersonal (texto Personal)
    (puertas, lecturasEstacion) = leerEstacion (texto Estacion)
    ref =
      Referencias
        { refPersonas = personas
        , refPuertas = puertas
        , refModulos = nub (concat [[puA p, puB p] | p <- puertas])
        , refAlias = construirAlias personas (lineasDe Radio (texto Radio) ++ lineasDe Bitacora (texto Bitacora))
        , refFecha = fromMaybe (fromGregorian 1970 1 1) (inferirFecha [texto Tarjetas, texto Energia, texto Radio])
        }

    -- 2. Leer cada archivo, línea por línea.
    lecturas =
      concat
        [ lecturasPersonal
        , lecturasEstacion
        , leerTarjetas ref (texto Tarjetas)
        , leerRadio ref (texto Radio)
        , leerEnergia ref (texto Energia)
        , leerPresencia ref (texto Presencia)
        , leerTemperatura ref (texto Temperatura)
        , leerBitacora ref (texto Bitacora)
        ]
    leidos = [e | Usada e _ <- lecturas]
    problemasDeLectura = [p | Descartada _ p <- lecturas] ++ concat [avisos | Usada _ avisos <- lecturas]

    -- 3. Revisar entre fuentes (duplicados, tarjetas perdidas, relojes...).
    (eventos, problemasDeValidacion) = validar ref leidos

    -- 4. Juntar y numerar todos los problemas.
    todos =
      avisosDeLectura
        ++ archivosQueFaltan archivos
        ++ avisosDeFecha ref archivos
        ++ problemasDeLectura
        ++ problemasDeValidacion
    numerados = zip (map codigo [1 :: Int ..]) (sortOn porLinea todos)
    codigo n = "PRB-" <> T.justifyRight 3 '0' (mostrar n)
    porLinea p = case prOrigenes p of
      (o : _) -> (0 :: Int, Just (oFuente o), oLinea o)
      [] -> (1, Nothing, 0)

archivosQueFaltan :: Map.Map Fuente Text -> [Problema]
archivosQueFaltan archivos =
  [ Problema FormatoInesperado Advertencia [] [] ("no se recibió el archivo " <> T.pack (archivoFuente f))
  | f <- [minBound .. maxBound]
  , Map.notMember f archivos
  ]

-- | Fuentes que no traen la fecha completa: se deja constancia de lo que se asumió.
avisosDeFecha :: Referencias -> Map.Map Fuente Text -> [Problema]
avisosDeFecha ref archivos =
  [ aviso ("presencia.csv solo trae la hora (HH.MM); se asume la fecha del caso " <> fecha)
  | Map.member Presencia archivos
  ]
    ++ [ aviso ("bitacora.txt usa fechas DD/MM sin año; se asume " <> mostrar anio)
       | Map.member Bitacora archivos
       ]
    ++ [ aviso ("radio.txt no trae fecha; se asume la fecha del caso " <> fecha)
       | Just t <- [Map.lookup Radio archivos]
       , null (fechasEn t)
       ]
  where
    fecha = mostrar (refFecha ref)
    (anio, _, _) = toGregorian (refFecha ref)
    aviso = Problema FechaInferida Informativo [] []

-- | El estado final de una línea: lo que dijo su lectura, salvo que una
--   revisión posterior la haya descartado (por ejemplo, por duplicada).
registroDe :: [(Text, Problema)] -> Lectura -> Registro
registroDe problemas lectura = Registro o estado (map fst relacionados)
  where
    o = origenDeLectura lectura
    mismaLinea x = (oFuente x, oCanal x, oLinea x) == (oFuente o, oCanal o, oLinea o)
    relacionados = [(codigo, p) | (codigo, p) <- problemas, any mismaLinea (prOrigenes p)]
    descartadaDespues = any ((== Descartado) . prSeveridad . snd) relacionados
    estado = case lectura of
      Ignorada _
        | T.null (T.strip (oTexto o)) -> "vacia"
        | otherwise -> "encabezado"
      Descartada _ _ -> "descartado"
      _ | descartadaDespues -> "descartado"
        | otherwise -> "usado"

supuestos :: Referencias -> [Text]
supuestos ref =
  [ "Toda hora se expresa en UTC: el enunciado dice que la estación funciona en hora UTC."
  , "La fecha del caso (" <> mostrar (refFecha ref) <> ") es la que más se repite en tarjetas.csv, energia.json y radio.txt."
  , "temperatura.csv usa segundos Unix (epoch) en UTC; convertidos caen el mismo día del caso."
  , "Una hora sin segundos (radio, presencia, bitácora) es el intervalo [HH:MM:00, HH:MM:59]."
  , "Una hora 'aprox.' de la bitácora es el intervalo de ±10 minutos."
  , "Una entrada de bitácora sin hora cubre el día completo."
  , "El identificador de cada persona es su apellido normalizado."
  , "Una tarjeta identifica a su titular, salvo que este haya reportado su pérdida antes del uso."
  , "Lo que dicen la radio y la bitácora son testimonios, no registros de sensor."
  ]
