{-# LANGUAGE OverloadedStrings #-}

-- | Una sola noción de hora: todo se convierte a intervalos UTC.
--
--   Formatos que llegan:
--     tarjetas.csv, energia.json   2041-06-14T00:12:04    (ISO, sin zona)
--     radio.txt                    [01:47] + fecha en el encabezado
--     presencia.csv                01.30                  (sin fecha)
--     temperatura.csv              2254786200             (segundos Unix)
--     bitacora.txt                 14/06, "01:50 aprox."  (sin año)
module Halley.Tiempo
  ( leerISO
  , leerHora
  , leerFechaCorta
  , rangoIlegible
  , enElDia
  , exacto
  , deUnMinuto
  , aproximado
  , deTodoElDia
  , nominal
  , aEpoch
  , deEpoch
  , iso
  , hora
  , minutosEntre
  , fechasEn
  ) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Halley.Tipos

-- ---------------------------------------------------------------------------
-- Leer

-- | "2041-06-14T00:12:04" (la estación funciona en UTC).
leerISO :: Text -> Maybe UTCTime
leerISO t = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack (T.strip t))

-- | "01:30" o "01.30" -> (1, 30)
leerHora :: Text -> Maybe (Int, Int)
leerHora t = case T.split (`elem` (":." :: String)) (T.strip t) of
  [h, m] | sonDigitos h && sonDigitos m -> validar (read (T.unpack h)) (read (T.unpack m))
  _ -> Nothing
  where
    sonDigitos x = not (T.null x) && T.length x <= 2 && T.all isDigit x
    validar h m = if h < 24 && m < 60 then Just (h, m) else Nothing

-- | "12/06" -> (12, 6)
leerFechaCorta :: Text -> Maybe (Int, Int)
leerFechaCorta t = case T.splitOn "/" (T.strip t) of
  [d, m] | numero d && numero m -> Just (read (T.unpack d), read (T.unpack m))
  _ -> Nothing
  where
    numero x = not (T.null x) && T.length x <= 2 && T.all isDigit x

-- | Para una fecha con dígitos ilegibles ("2041-06-14T0#:4?:1?"), el rango de
--   horas posible: cada caracter ilegible se cambia por 0 y luego por 9.
rangoIlegible :: Text -> Maybe (UTCTime, UTCTime)
rangoIlegible t = do
  menor <- leerISO (T.map (reemplazar '0') t)
  mayor <- leerISO (T.map (reemplazar '9') t)
  if T.any ilegible t && menor <= mayor then Just (menor, mayor) else Nothing
  where
    ilegible c = c `elem` ("?#*" :: String)
    reemplazar d c = if ilegible c then d else c

-- | Todas las fechas completas (AAAA-MM-DD o DD/MM/AAAA) que aparecen en un texto.
fechasEn :: Text -> [Day]
fechasEn = mapMaybe fecha . T.split (\c -> not (isDigit c || c == '-' || c == '/'))
  where
    fecha trozo = case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack trozo) of
      Just d -> Just d
      Nothing -> parseTimeM False defaultTimeLocale "%d/%m/%Y" (T.unpack trozo)

-- ---------------------------------------------------------------------------
-- Construir intervalos

enElDia :: Day -> (Int, Int) -> UTCTime
enElDia dia (h, m) = UTCTime dia (secondsToDiffTime (fromIntegral (h * 3600 + m * 60)))

-- | Un instante exacto, al segundo.
exacto :: UTCTime -> Instante
exacto t = Instante t t AlSegundo

-- | "01:47" es cualquier segundo entre 01:47:00 y 01:47:59.
deUnMinuto :: UTCTime -> Instante
deUnMinuto t = Instante t (addUTCTime 59 t) AlMinuto

-- | "01:50 aprox." es el intervalo [01:50 - N min, 01:50 + N min].
aproximado :: Int -> UTCTime -> Instante
aproximado minutos t = Instante (addUTCTime (-margen) t) (addUTCTime margen t) (Aproximada minutos)
  where
    margen = fromIntegral (minutos * 60)

deTodoElDia :: Day -> Instante
deTodoElDia dia = Instante (UTCTime dia 0) (UTCTime dia 86399) AlDia

-- | La hora tal como la escribió la fuente (el centro, si era "aprox.").
nominal :: Instante -> UTCTime
nominal i = case iPrecision i of
  Aproximada m -> addUTCTime (fromIntegral (m * 60)) (iDesde i)
  _ -> iDesde i

-- ---------------------------------------------------------------------------
-- Convertir y mostrar

aEpoch :: UTCTime -> Integer
aEpoch = floor . utcTimeToPOSIXSeconds

deEpoch :: Integer -> UTCTime
deEpoch = posixSecondsToUTCTime . fromIntegral

iso :: UTCTime -> Text
iso = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

hora :: UTCTime -> Text
hora = T.pack . formatTime defaultTimeLocale "%H:%M:%S"

minutosEntre :: UTCTime -> UTCTime -> Integer
minutosEntre despues antes = round (realToFrac (diffUTCTime despues antes) / (60 :: Double))
