{-# LANGUAGE OverloadedStrings #-}

-- | Traduce el resultado a JSON (para Prolog, Java y el informe) y a un
--   resumen legible para la consola. Funciones puras.
--
--   OJO: el motor Prolog (C2) lee estos nombres de campos. Si se cambia uno,
--   hay que cambiarlo también en motor-prolog/convertir.pl.
module Halley.Salida
  ( resultadoJSON
  , eventoJSON
  , problemaJSON
  , resumen
  ) where

import Data.Char (isUpper, toLower)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Halley.Integracion
import Halley.Json
import Halley.Referencias (esExterior, nombreModulo)
import Halley.Texto (mostrar)
import Halley.Tiempo
import Halley.Tipos

resultadoJSON :: Resultado -> JSON
resultadoJSON r =
  JObjeto
    [ ("sistema", texto "Halley-7 - integrador de evidencia (C1, Haskell)")
    , ("zona_horaria", texto "UTC")
    , ("fecha_caso", texto (mostrar (refFecha ref)))
    , ("supuestos", lista texto (reSupuestos r))
    , ("personas", lista personaJSON (refPersonas ref))
    , ("modulos", lista moduloJSON (refModulos ref))
    , ("puertas", lista puertaJSON (refPuertas ref))
    , ("eventos", lista eventoJSON (reEventos r))
    , ("problemas", lista problemaJSON (reProblemas r))
    , ("registros", lista registroJSON (reRegistros r))
    ]
  where
    ref = reReferencias r
    personaJSON p =
      JObjeto
        [ ("id", texto (perId p))
        , ("nombre", texto (perNombre p))
        , ("cargo", texto (perCargo p))
        , ("tarjeta", texto (perTarjeta p))
        , ("accesos", lista texto (perAccesos p))
        , ("alias", lista aliasJSON [a | a <- refAlias ref, alPersona a == perId p])
        ]
    aliasJSON a =
      JObjeto
        [ ("texto", texto (T.unwords (alPalabras a)))
        , ("confianza", texto (confianzaTxt (alConfianza a)))
        , ("motivo", texto (alMotivo a))
        ]
    moduloJSON m =
      JObjeto [("id", texto m), ("nombre", texto (nombreModulo m)), ("sensor_presencia", JBool (not (esExterior m)))]
    puertaJSON p =
      JObjeto [("id", texto (puId p)), ("a", texto (puA p)), ("b", texto (puB p)), ("lector", JBool (puLector p))]
    registroJSON g =
      JObjeto
        [ ("origen", texto (idOrigen (regOrigen g)))
        , ("fuente", texto (nombreFuente (oFuente (regOrigen g))))
        , ("linea", entero (oLinea (regOrigen g)))
        , ("texto", texto (oTexto (regOrigen g)))
        , ("estado", texto (regEstado g))
        , ("motivos", lista texto (regMotivos g))
        ]

eventoJSON :: Evento -> JSON
eventoJSON e =
  JObjeto $
    [ ("id", texto (evId e))
    , ("fuente", texto (nombreFuente (oFuente o)))
    , ("canal", texto (if oCanal o == EnVivo then "vivo" else "archivo"))
    , ("linea", entero (oLinea o))
    , ("desde", texto (iso (iDesde c)))
    , ("hasta", texto (iso (iHasta c)))
    , ("nominal", texto (iso (nominal c)))
    , ("t0", entero (aEpoch (iDesde c)))
    , ("t1", entero (aEpoch (iHasta c)))
    , ("precision", texto (precisionTxt (iPrecision c)))
    ]
      ++ contenidoJSON (evContenido e)
      ++ [("notas", lista texto (evNotas e)), ("original", texto (oTexto o))]
  where
    o = evOrigen e
    c = evCuando e

contenidoJSON :: Contenido -> [(Text, JSON)]
contenidoJSON contenido = case contenido of
  UsoTarjeta u ->
    [ ("tipo", texto "uso_tarjeta")
    , ("tarjeta", texto (usTarjeta u))
    , ("titular", quizas texto (usTitular u))
    , ("puerta", texto (usPuerta u))
    , ("modulo_desde", texto (usDesde u))
    , ("modulo_hacia", texto (usHacia u))
    , ("resultado", texto (if usResultado u == Autorizado then "OK" else "DENEGADO"))
    , ("tarjeta_reportada_perdida", quizas texto (usReportadaPerdida u))
    ]
  MensajeRadio t -> ("tipo", texto "mensaje_radio") : ("hablante", identJSON (tlQuien t)) : textoLibreJSON t
  NotaBitacora t -> ("tipo", texto "nota_bitacora") : ("autor", identJSON (tlQuien t)) : textoLibreJSON t
  ConsumoElectrico kws -> [("tipo", texto "consumo_electrico"), ("consumo_kw", JObjeto [(m, JNumero kw) | (m, kw) <- kws])]
  CorteEnergia afecta nota -> [("tipo", texto "corte_energia"), ("afecta", lista texto afecta), ("nota", quizas texto nota)]
  Restablecimiento -> [("tipo", texto "restablecimiento_energia")]
  ConteoPersonas m n -> [("tipo", texto "conteo_presencia"), ("modulo", texto m), ("personas", entero n)]
  PresenciaSinEnergia m -> [("tipo", texto "presencia_sin_energia"), ("modulo", quizas texto m)]
  LecturaTemperatura s m g -> [("tipo", texto "temperatura"), ("sensor", texto s), ("modulo", texto m), ("celsius", JNumero g)]
  where
    textoLibreJSON t =
      [ ("texto", texto (tlTexto t))
      , ("menciones", lista identJSON (tlMenciones t))
      , ("afirmaciones", lista afirmacionJSON (tlAfirmaciones t))
      ]

identJSON :: Identificacion -> JSON
identJSON i =
  JObjeto
    [ ("persona", quizas texto (idPersona i))
    , ("texto", texto (idTexto i))
    , ("confianza", texto (confianzaTxt (idConfianza i)))
    , ("motivo", texto (idMotivo i))
    ]

afirmacionJSON :: Afirmacion -> JSON
afirmacionJSON a = JObjeto $ case a of
  EstaEn p m -> [("tipo", texto "ubicacion"), ("persona", texto p), ("modulo", texto m)]
  SeFueLaLuz -> [("tipo", texto "corte_energia")]
  SinLuz m -> [("tipo", texto "sin_energia"), ("modulo", quizas texto m)]
  VolvioLaLuz -> [("tipo", texto "restablecimiento")]
  PerdioTarjeta p -> [("tipo", texto "tarjeta_perdida"), ("persona", texto p)]
  PuertaExterior -> [("tipo", texto "puerta_exterior")]

problemaJSON :: (Text, Problema) -> JSON
problemaJSON (codigo, p) =
  JObjeto
    [ ("id", texto codigo)
    , ("tipo", texto (tipoTxt (prTipo p)))
    , ("severidad", texto (severidadTxt (prSeveridad p)))
    , ("origenes", lista (texto . idOrigen) (prOrigenes p))
    , ("eventos", lista texto (prEventos p))
    , ("descripcion", texto (prDescripcion p))
    ]

-- ---------------------------------------------------------------------------
-- Nombres en minúscula para el JSON

precisionTxt :: Precision -> Text
precisionTxt p = case p of
  AlSegundo -> "segundo"
  AlMinuto -> "minuto"
  Aproximada m -> "aprox_" <> mostrar m <> "min"
  AlDia -> "dia"

confianzaTxt :: Confianza -> Text
confianzaTxt c = case c of
  Exacta -> "exacta"
  PorCargo -> "por_cargo"
  PorContexto -> "por_contexto"
  Probable -> "probable"
  SinResolver -> "sin_resolver"

severidadTxt :: Severidad -> Text
severidadTxt = T.toLower . mostrar

-- | AliasModulo -> "alias_modulo": antes de cada mayúscula va un "_".
tipoTxt :: TipoProblema -> Text
tipoTxt = T.drop 1 . T.concatMap separar . mostrar
  where
    separar letra
      | isUpper letra = T.pack ['_', toLower letra]
      | otherwise = T.singleton letra

-- ---------------------------------------------------------------------------
-- Resumen para la consola

resumen :: Resultado -> Text
resumen r =
  T.unlines $
    [ "== Halley-7 · integrador (C1) =="
    , "Fecha del caso: " <> mostrar (refFecha (reReferencias r)) <> " (UTC)"
    , "Líneas leídas: " <> mostrar (length (reRegistros r)) <> "  " <> contar (map regEstado (reRegistros r))
    , "Eventos en la línea de tiempo: " <> mostrar (length (reEventos r))
    , "Problemas: " <> mostrar (length (reProblemas r)) <> "  " <> contar (map (severidadTxt . prSeveridad . snd) (reProblemas r))
    , ""
    , "-- Línea de tiempo --"
    ]
      ++ map lineaDeEvento (reEventos r)
      ++ ["", "-- Problemas --"]
      ++ [codigo <> " [" <> severidadTxt (prSeveridad p) <> "] " <> tipoTxt (prTipo p) <> ": " <> prDescripcion p | (codigo, p) <- reProblemas r]
  where
    contar xs = "(" <> T.intercalate ", " [k <> ": " <> mostrar n | (k, n) <- Map.toList (Map.fromListWith (+) [(x, 1 :: Int) | x <- xs])] <> ")"

lineaDeEvento :: Evento -> Text
lineaDeEvento e = cuando <> "  " <> T.justifyLeft 16 ' ' (evId e) <> describir (evContenido e) <> notas
  where
    c = evCuando e
    dia = T.take 10 (iso (iDesde c))
    cuando = case iPrecision c of
      AlDia -> dia <> " (todo el día)"
      Aproximada m -> dia <> " " <> hora (nominal c) <> " ±" <> mostrar m <> "m"
      _ -> T.justifyLeft 24 ' ' (dia <> " " <> hora (iDesde c))
    notas = if null (evNotas e) then "" else "  [!] " <> T.intercalate " | " (evNotas e)
    describir contenido = case contenido of
      UsoTarjeta u ->
        usTarjeta u <> " " <> usPuerta u <> " " <> usDesde u <> "->" <> usHacia u <> " "
          <> (if usResultado u == Autorizado then "OK" else "DENEGADO")
      MensajeRadio t -> "radio, " <> quien t <> ": " <> tlTexto t
      NotaBitacora t -> "bitácora, " <> quien t <> ": " <> tlTexto t
      ConsumoElectrico _ -> "lectura de consumo eléctrico"
      CorteEnergia afecta _ -> "CORTE de energía en " <> T.intercalate "," afecta
      Restablecimiento -> "energía RESTABLECIDA"
      ConteoPersonas m n -> "presencia " <> m <> " = " <> mostrar n
      PresenciaSinEnergia _ -> "sensores de presencia SIN ENERGÍA"
      LecturaTemperatura s _ g -> s <> " = " <> mostrar g <> " °C"
    quien t = maybe ("¿" <> idTexto (tlQuien t) <> "?") id (idPersona (tlQuien t))
