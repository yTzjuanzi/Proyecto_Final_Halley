{-# LANGUAGE OverloadedStrings #-}

-- | Un lector por archivo. Todos siguen la misma idea:
--
--       texto del archivo  ->  líneas  ->  una 'Lectura' por línea
--
--   y cada 'Lectura' dice qué pasó con esa línea: se ignoró, se usó o se
--   descartó (con el motivo). Ninguna línea se pierde.
module Halley.Fuentes
  ( lineasDe
  , leerPersonal
  , leerEstacion
  , leerTarjetas
  , leerLineaTarjeta
  , leerRadio
  , leerEnergia
  , leerPresencia
  , leerLineaPresencia
  , leerTemperatura
  , leerLineaTemperatura
  , leerBitacora
  ) where

import Data.List (partition)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, fromGregorianValid, toGregorian)
import Halley.Identidad
import Halley.Json
import Halley.Referencias
import Halley.Texto
import Halley.Tiempo
import Halley.Tipos

-- ---------------------------------------------------------------------------
-- Ayudas comunes

-- | Las líneas de un archivo, numeradas desde 1, con su texto original.
lineasDe :: Fuente -> Text -> [Origen]
lineasDe fuente txt =
  [ Origen fuente Archivo n (T.dropWhileEnd (== '\r') linea)
  | (n, linea) <- zip [1 ..] (T.lines (T.dropWhile (== '\xFEFF') txt))
  ]

vacia :: Origen -> Bool
vacia = T.null . T.strip . oTexto

-- | Para archivos con encabezado: separa lo que se ignora (encabezado y líneas
--   vacías) de las filas con datos.
filas :: Fuente -> Text -> Text -> ([Lectura], [Origen])
filas fuente primeraColumna txt =
  case llenas of
    (encabezado : datos)
      | primeraColumna `T.isPrefixOf` normalizar (oTexto encabezado) ->
          (map Ignorada (encabezado : vacias), datos)
    _ -> (map Ignorada vacias, llenas)
  where
    (vacias, llenas) = partition vacia (lineasDe fuente txt)

-- | Junta cada línea que empieza un registro con las que la continúan
--   (en el PDF, algunos mensajes quedaron partidos en dos líneas).
agrupar :: (Origen -> Bool) -> [Origen] -> [(Origen, [Origen])]
agrupar _ [] = []
agrupar esInicio (primera : resto) =
  let (continuacion, siguientes) = break esInicio resto
   in (primera, continuacion) : agrupar esInicio siguientes

-- | Un registro de varias líneas se trata como uno solo.
unir :: (Origen, [Origen]) -> (Origen, Text)
unir (primera, resto) =
  ( primera {oTexto = T.intercalate "\n" (map oTexto (primera : resto))}
  , T.unwords (map (T.strip . oTexto) (primera : resto))
  )

usar :: Origen -> Instante -> Contenido -> [Problema] -> Lectura
usar o cuando contenido avisos = Usada (Evento (idOrigen o) o cuando contenido []) avisos

descartar :: Origen -> TipoProblema -> [Text] -> Lectura
descartar o tipo motivos = Descartada o (Problema tipo Descartado [o] [] (T.intercalate "; " motivos))

aviso :: TipoProblema -> Severidad -> Origen -> Text -> Problema
aviso tipo severidad o descripcion = Problema tipo severidad [o] [] descripcion

avisoDeAlias :: Origen -> Text -> ModuloId -> Problema
avisoDeAlias o escrito m = aviso AliasModulo Advertencia o ("módulo escrito '" <> escrito <> "'; se interpreta como " <> m)

-- | Deja rastro cuando a alguien se le identificó por algo distinto a su nombre.
avisosDeIdentidad :: Referencias -> Origen -> Text -> Identificacion -> [Problema]
avisosDeIdentidad ref o rol ident = case idConfianza ident of
  Exacta -> []
  Probable -> [aviso AliasPersona Advertencia o (encabezado <> " se asocia a " <> quien <> " de forma NO segura: " <> idMotivo ident)]
  SinResolver -> [aviso AliasPersona Advertencia o (encabezado <> " sin identificar: " <> idMotivo ident)]
  _ -> [aviso AliasPersona Informativo o (encabezado <> " identificado como " <> quien <> ": " <> idMotivo ident)]
  where
    encabezado = rol <> " '" <> idTexto ident <> "'"
    quien = maybe "?" (nombreDe ref) (idPersona ident)

-- ---------------------------------------------------------------------------
-- personal.csv y estacion.csv (referencias)

leerPersonal :: Text -> ([Persona], [Lectura])
leerPersonal txt = ([p | (Just p, _) <- leidas], ignoradas ++ map snd leidas)
  where
    (ignoradas, datos) = filas Personal "tarjeta" txt
    leidas = map leerFila datos
    leerFila o = case separar ',' (oTexto o) of
      [tarjeta, nombre, cargo, accesos]
        | not (T.null tarjeta) && not (T.null nombre) ->
            ( Just (Persona (idDePersona nombre) nombre cargo (T.toUpper tarjeta) (T.words (T.toUpper accesos)))
            , Referencia o
            )
      campos -> (Nothing, descartar o Corrupto ["se esperaban 4 campos y hay " <> mostrar (length campos)])

leerEstacion :: Text -> ([Puerta], [Lectura])
leerEstacion txt = ([p | (Just p, _) <- leidas], ignoradas ++ map snd leidas)
  where
    (ignoradas, datos) = filas Estacion "puerta" txt
    leidas = map leerFila datos
    leerFila o = case separar ',' (oTexto o) of
      [puerta, a, b, lector]
        | normalizar lector `elem` ["si", "no"] ->
            (Just (Puerta (T.toUpper puerta) (T.toUpper a) (T.toUpper b) (normalizar lector == "si")), Referencia o)
      campos -> (Nothing, descartar o Corrupto ["se esperaban 4 campos (puerta,modulo_1,modulo_2,si/no) y hay " <> mostrar (length campos)])

-- ---------------------------------------------------------------------------
-- tarjetas.csv:  fecha_hora,tarjeta,puerta,desde,hacia,resultado

leerTarjetas :: Referencias -> Text -> [Lectura]
leerTarjetas ref txt = ignoradas ++ map (leerLineaTarjeta ref) datos
  where
    (ignoradas, datos) = filas Tarjetas "fecha_hora" txt

leerLineaTarjeta :: Referencias -> Origen -> Lectura
leerLineaTarjeta ref o = case campos of
  [f, tj, pu, de, ha, re] ->
    case (leerISO f, titularDe ref (T.toUpper tj), buscarPuerta ref pu, normalizarModulo ref de, normalizarModulo ref ha, leerResultado re) of
      (Just t, Just dueno, Just puerta, Just (desde, aliasD), Just (hacia, aliasH), Just resultado)
        | conecta puerta desde hacia ->
            usar
              o
              (exacto t)
              (UsoTarjeta (UsoDeTarjeta (perTarjeta dueno) (Just (perId dueno)) (puId puerta) desde hacia resultado Nothing))
              ([avisoDeAlias o de desde | aliasD] ++ [avisoDeAlias o ha hacia | aliasH])
        | otherwise ->
            descartar o Contradiccion ["la puerta " <> puId puerta <> " no conecta " <> desde <> " con " <> hacia <> " según estacion.csv"]
      _ -> descartar o Corrupto (camposConProblemas ref campos)
  _ -> descartar o Corrupto (("se esperaban 6 campos y hay " <> mostrar (length campos)) : camposConProblemas ref campos ++ faltantes)
  where
    campos = separar ',' (oTexto o)
    faltantes = ["faltan: " <> T.intercalate ", " (drop (length campos) nombresDeCampos) | length campos < 6]

nombresDeCampos :: [Text]
nombresDeCampos = ["fecha_hora", "tarjeta", "puerta", "desde", "hacia", "resultado"]

leerResultado :: Text -> Maybe ResultadoPuerta
leerResultado r = case T.toUpper (T.strip r) of
  "OK" -> Just Autorizado
  "DENEGADO" -> Just Denegado
  _ -> Nothing

-- | Para un registro dañado, explica qué tiene mal cada campo que sí llegó.
camposConProblemas :: Referencias -> [Text] -> [Text]
camposConProblemas ref campos = catMaybes (zipWith ($) revisiones campos)
  where
    revisiones = [revisarFecha, revisarTarjeta, revisarPuerta, revisarModulo "desde", revisarModulo "hacia", revisarResultado]
    revisarFecha f
      | isJust (leerISO f) = Nothing
      | otherwise = Just ("fecha_hora ilegible '" <> f <> "'" <> maybe "" rango (rangoIlegible f))
    rango (a, b) = " (compatible con " <> hora a <> " a " <> hora b <> ")"
    revisarTarjeta tj
      | isJust (titularDe ref (T.toUpper tj)) = Nothing
      | otherwise = Just ("tarjeta desconocida '" <> tj <> "'" <> parecidos tj (map perTarjeta (refPersonas ref)))
    revisarPuerta pu
      | isJust (buscarPuerta ref pu) = Nothing
      | otherwise = Just ("puerta desconocida '" <> pu <> "'")
    revisarModulo nombre m
      | isJust (normalizarModulo ref m) = Nothing
      | otherwise = Just (nombre <> " ilegible '" <> m <> "'" <> parecidos m (refModulos ref))
    revisarResultado r
      | isJust (leerResultado r) = Nothing
      | otherwise = Just ("resultado desconocido '" <> r <> "'")
    parecidos x opciones = case compatibles x opciones of
      [] -> ""
      cs -> " (compatible con: " <> T.intercalate ", " cs <> ")"

-- ---------------------------------------------------------------------------
-- radio.txt:  encabezado con la fecha y luego  [HH:MM] Nombre: texto

leerRadio :: Referencias -> Text -> [Lectura]
leerRadio ref txt = map Ignorada (vacias ++ encabezado) ++ map leerMensaje (agrupar esMensaje cuerpo)
  where
    (vacias, llenas) = partition vacia (lineasDe Radio txt)
    esMensaje o = "[" `T.isPrefixOf` T.strip (oTexto o)
    (encabezado, cuerpo) = break esMensaje llenas
    fecha = fromMaybe (refFecha ref) (listToMaybe (concatMap (fechasEn . oTexto) encabezado))

    leerMensaje grupo =
      let (o, completo) = unir grupo
       in case partirMensaje completo of
            Nothing -> descartar o Corrupto ["no tiene la forma '[HH:MM] Nombre: texto'"]
            Just (hm, nombre, dicho) ->
              let quien = identificar ref nombre
               in usar
                    o
                    (deUnMinuto (enElDia fecha hm))
                    (MensajeRadio (analizarTexto (refAlias ref) quien dicho))
                    (avisosDeIdentidad ref o "hablante" quien)

-- | "[01:47] Petrov: Se fue la luz." -> ((1,47), "Petrov", "Se fue la luz.")
partirMensaje :: Text -> Maybe ((Int, Int), Text, Text)
partirMensaje t = do
  sinCorchete <- T.stripPrefix "[" (T.strip t)
  let (horaTxt, resto) = T.breakOn "]" sinCorchete
  hm <- leerHora horaTxt
  despues <- T.stripPrefix "]" resto
  let (nombre, conDosPuntos) = T.breakOn ":" despues
  dicho <- T.stripPrefix ":" conDosPuntos
  if T.null (T.strip nombre) then Nothing else Just (hm, T.strip nombre, T.strip dicho)

-- ---------------------------------------------------------------------------
-- energia.json:  {"registros": [ {ts, tipo, ...}, ... ]}
-- (aquí la "línea" de cada dato es su posición en el arreglo)

leerEnergia :: Referencias -> Text -> [Lectura]
leerEnergia ref txt = case analizar txt >>= registros of
  Left err -> [descartar todoElArchivo Corrupto ["energia.json: " <> err]]
  Right regs -> zipWith leerRegistro [1 ..] regs
  where
    todoElArchivo = Origen Energia Archivo 0 txt
    registros j = maybe (Left "no tiene el arreglo 'registros'") Right (campo "registros" j >>= comoLista)

    leerRegistro i r =
      let o = Origen Energia Archivo i (codificar r)
          textoDe k = campo k r >>= comoTexto
       in case (textoDe "ts" >>= leerISO, normalizar <$> textoDe "tipo") of
            (Nothing, _) -> descartar o Corrupto ["'ts' ausente o ilegible"]
            (Just t, Just "lectura") -> usar o (exacto t) (ConsumoElectrico (consumos r)) []
            (Just t, Just "corte") -> usar o (exacto t) (CorteEnergia (afectados r) (textoDe "nota")) []
            (Just t, Just "restablecido") -> usar o (exacto t) Restablecimiento []
            (Just _, otro) -> descartar o Corrupto ["tipo de registro desconocido: " <> fromMaybe "(sin tipo)" otro]

    consumos r =
      [ (m, kw)
      | Just pares <- [campo "consumo_kw" r >>= comoObjeto]
      , (k, v) <- pares
      , Just (m, _) <- [normalizarModulo ref k]
      , Just kw <- [comoNumero v]
      ]
    afectados r =
      [ m
      | Just xs <- [campo "afecta" r >>= comoLista]
      , x <- xs
      , Just nombre <- [comoTexto x]
      , Just (m, _) <- [normalizarModulo ref nombre]
      ]

-- ---------------------------------------------------------------------------
-- presencia.csv:  hora;modulo;personas   (01.30;LAB_B;0  ó  01.40;*;SIN_ENERGIA)

leerPresencia :: Referencias -> Text -> [Lectura]
leerPresencia ref txt = ignoradas ++ map (leerLineaPresencia ref) datos
  where
    (ignoradas, datos) = filas Presencia "hora" txt

leerLineaPresencia :: Referencias -> Origen -> Lectura
leerLineaPresencia ref o = case separar ';' (oTexto o) of
  [h, m, n] -> case leerHora h of
    Nothing -> descartar o Corrupto ["hora ilegible '" <> h <> "'"]
    Just hm ->
      let cuando = deUnMinuto (enElDia (refFecha ref) hm) -- la fecha se infiere
          sinEnergia = T.toUpper n == "SIN_ENERGIA"
       in case (m == "*", normalizarModulo ref m, leerEntero n) of
            (True, _, _) | sinEnergia -> usar o cuando (PresenciaSinEnergia Nothing) []
            (_, Just (modulo, esAlias), _) | sinEnergia -> usar o cuando (PresenciaSinEnergia (Just modulo)) [avisoDeAlias o m modulo | esAlias]
            (_, Just (modulo, esAlias), Just k) | k >= 0 -> usar o cuando (ConteoPersonas modulo k) [avisoDeAlias o m modulo | esAlias]
            (_, Nothing, _) -> descartar o Corrupto ["módulo desconocido '" <> m <> "'"]
            _ -> descartar o Corrupto ["conteo ilegible '" <> n <> "'"]
  campos -> descartar o Corrupto ["se esperaban 3 campos separados por ';' y hay " <> mostrar (length campos)]

-- ---------------------------------------------------------------------------
-- temperatura.csv:  epoch,sensor,celsius   (2254786200,T-ALM,12.0)

leerTemperatura :: Referencias -> Text -> [Lectura]
leerTemperatura ref txt = ignoradas ++ map (leerLineaTemperatura ref) datos
  where
    (ignoradas, datos) = filas Temperatura "epoch" txt

leerLineaTemperatura :: Referencias -> Origen -> Lectura
leerLineaTemperatura ref o = case separar ',' (oTexto o) of
  [e, s, c] -> case (leerEntero e, moduloDelSensor s, leerDecimal c) of
    (Just epoch, Just modulo, Just grados) ->
      usar o (exacto (deEpoch epoch)) (LecturaTemperatura (T.toUpper s) modulo grados) []
    (epoch, modulo, grados) ->
      descartar
        o
        Corrupto
        ( ["epoch ilegible '" <> e <> "'" | epoch == Nothing]
            ++ ["sensor desconocido '" <> s <> "'" | modulo == Nothing]
            ++ ["temperatura ilegible '" <> c <> "'" | grados == Nothing]
        )
  campos -> descartar o Corrupto ["se esperaban 3 campos (epoch,sensor,celsius) y hay " <> mostrar (length campos)]
  where
    -- "T-ALM" -> "ALM"
    moduloDelSensor s = T.stripPrefix "T-" (T.toUpper s) >>= fmap fst . normalizarModulo ref

-- ---------------------------------------------------------------------------
-- bitacora.txt:  encabezado y luego  DD/MM - Día N[, HH:MM [aprox.]]. texto

leerBitacora :: Referencias -> Text -> [Lectura]
leerBitacora ref txt =
  map Ignorada (vacias ++ encabezado)
    ++ zipWith leerEntrada [0 :: Int ..] (agrupar esEntrada cuerpo)
  where
    (vacias, llenas) = partition vacia (lineasDe Bitacora txt)
    esEntrada o = isJust (leerFechaCorta (T.takeWhile (/= ' ') (T.strip (oTexto o))))
    (encabezado, cuerpo) = break esEntrada llenas
    (anio, _, _) = toGregorian (refFecha ref)

    -- "BITÁCORA DEL JEFE DE ESTACIÓN": el autor se deduce del cargo.
    autor = case analizarTexto (refAlias ref) nadie (T.unwords (map oTexto encabezado)) of
      TextoLibre {tlMenciones = (i : _)} -> i
      _ -> nadie
    nadie = Identificacion Nothing "" SinResolver "el encabezado no nombra a nadie"
    -- El aviso sobre el autor se deja una sola vez, señalando el encabezado.
    avisoDelAutor = case encabezado of
      (o : _) -> avisosDeIdentidad ref o "autor de la bitácora" autor
      [] -> []

    leerEntrada i grupo =
      let (o, completo) = unir grupo
       in case partirEntrada anio completo of
            Nothing -> descartar o Corrupto ["no tiene la forma 'DD/MM - Día N[, HH:MM]. texto'"]
            Just (dia, horaEscrita, esAprox, dicho) ->
              let cuando = case horaEscrita of
                    Nothing -> deTodoElDia dia
                    Just hm
                      | esAprox -> aproximado 10 (enElDia dia hm)
                      | otherwise -> deUnMinuto (enElDia dia hm)
                  avisoAprox =
                    [ aviso HoraAproximada Informativo o "hora escrita como 'aprox.'; se toma como un intervalo de ±10 min"
                    | esAprox
                    ]
               in usar
                    o
                    cuando
                    (NotaBitacora (analizarTexto (refAlias ref) autor dicho))
                    (avisoAprox ++ (if i == 0 then avisoDelAutor else []))

-- | "14/06 - Día 6, 01:50 aprox. Sin luz otra vez."
--      -> (2041-06-14, Just (1,50), True, "Sin luz otra vez.")
partirEntrada :: Integer -> Text -> Maybe (Day, Maybe (Int, Int), Bool, Text)
partirEntrada anio t = do
  let (fechaTxt, resto) = T.breakOn " - " t
  (d, m) <- leerFechaCorta fechaTxt
  dia <- fromGregorianValid anio m d
  despues <- T.stripPrefix " - " resto
  let (cabecera, conTexto) = T.breakOn ". " (despues <> " ")
      dicho = T.strip (T.drop 2 conTexto)
      horaTxt = case map T.strip (T.splitOn "," cabecera) of
        (_ : h : _) -> Just h -- "Día 6, 01:50 aprox" -> "01:50 aprox"
        _ -> Nothing
      horaEscrita = horaTxt >>= leerHora . T.takeWhile (/= ' ')
      esAprox = maybe False (T.isInfixOf "aprox" . normalizar) horaTxt
  pure (dia, horaEscrita, esAprox, dicho)
