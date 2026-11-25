{-# LANGUAGE OverloadedStrings #-}

-- | Revisiones que necesitan mirar varias fuentes a la vez.
--   Nada se borra: cada hallazgo queda como un 'Problema' con su evidencia.
module Halley.Validacion
  ( validar
  , ordenar
  , Clave
  , clave
  , duplicado
  , reportesDePerdida
  , marcarSiEstaPerdida
  , revisarAccesos
  ) where

import Data.List (groupBy, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, utctDay)
import Halley.Referencias
import Halley.Texto (mostrar)
import Halley.Tiempo
import Halley.Tipos

-- | Todas las revisiones, en orden. Devuelve la línea de tiempo final
--   (sin duplicados y ordenada) y los problemas encontrados.
validar :: Referencias -> [Evento] -> ([Evento], [Problema])
validar ref leidos = (ordenar eventos, duplicados ++ perdidas ++ concatMap ($ eventos) revisiones)
  where
    (sinDuplicados, duplicados) = quitarDuplicados leidos
    (eventos, perdidas) = marcarTarjetasPerdidas ref sinDuplicados
    revisiones =
      [ revisarAccesos ref
      , revisarRelojes ref
      , revisarLuz ref
      , revisarPresencia ref
      , revisarTemperatura
      ]

-- | Línea de tiempo: por hora (la escrita por la fuente), luego fuente y línea.
ordenar :: [Evento] -> [Evento]
ordenar = sortOn (\e -> (nominal (evCuando e), oFuente (evOrigen e), oLinea (evOrigen e)))

-- ---------------------------------------------------------------------------
-- Duplicados

-- | Dos eventos son el mismo si vienen de la misma fuente, a la misma hora y
--   con el mismo contenido.
type Clave = (Fuente, Instante, Contenido)

clave :: Evento -> Clave
clave e = (oFuente (evOrigen e), evCuando e, evContenido e)

-- | Se queda con el primero de cada clave; los repetidos pasan a ser problemas.
quitarDuplicados :: [Evento] -> ([Evento], [Problema])
quitarDuplicados = recorrer Map.empty
  where
    recorrer _ [] = ([], [])
    recorrer vistos (e : resto) = case Map.lookup (clave e) vistos of
      Just primero ->
        let (es, ps) = recorrer vistos resto in (es, duplicado e primero : ps)
      Nothing ->
        let (es, ps) = recorrer (Map.insert (clave e) (evId e) vistos) resto in (e : es, ps)

duplicado :: Evento -> Text -> Problema
duplicado e primero =
  Problema Duplicado Descartado [evOrigen e] [primero] $
    "registro idéntico a " <> primero <> " (misma hora y mismos datos); se conserva solo el primero"
      <> case evContenido e of
        UsoTarjeta _ -> ". Cruzar la misma puerta en el mismo sentido dos veces en el mismo segundo no es posible"
        _ -> ""

-- ---------------------------------------------------------------------------
-- Tarjetas perdidas: si alguien reportó que perdió su tarjeta, los usos
-- posteriores de esa tarjeta ya NO lo identifican.

-- | (persona, cuándo lo reportó, evento del reporte)
reportesDePerdida :: [Evento] -> [(PersonaId, UTCTime, Text)]
reportesDePerdida es =
  [(p, iDesde (evCuando e), evId e) | e <- es, PerdioTarjeta p <- afirmacionesDelEvento e]

marcarTarjetasPerdidas :: Referencias -> [Evento] -> ([Evento], [Problema])
marcarTarjetasPerdidas ref es = (map fst marcados, concatMap snd marcados)
  where
    marcados = map (marcarSiEstaPerdida ref (reportesDePerdida es)) es

marcarSiEstaPerdida :: Referencias -> [(PersonaId, UTCTime, Text)] -> Evento -> (Evento, [Problema])
marcarSiEstaPerdida ref reportes e = case evContenido e of
  UsoTarjeta uso
    | Just titular <- usTitular uso
    , ((_, cuando, reporte) : _) <- [r | r@(p, t, _) <- reportes, p == titular, t < iDesde (evCuando e)] ->
        let nombre = nombreDe ref titular
         in ( e
                { evContenido = UsoTarjeta uso {usReportadaPerdida = Just reporte}
                , evNotas = ["tarjeta reportada perdida en " <> reporte <> ": este uso no identifica a " <> nombre]
                }
            , [ Problema TarjetaPerdida Advertencia [evOrigen e] [evId e, reporte] $
                  "la tarjeta " <> usTarjeta uso <> " (titular: " <> nombre <> ") se usó en " <> usPuerta uso
                    <> " a las " <> hora (iDesde (evCuando e)) <> ", pero en " <> reporte
                    <> " (" <> mostrar (utctDay cuando) <> ") se reportó perdida. La tarjeta no identifica a quien la usó."
              ]
            )
  _ -> (e, [])

-- ---------------------------------------------------------------------------
-- Accesos: el lector dejó pasar a quien no tiene permiso, o al revés.

revisarAccesos :: Referencias -> [Evento] -> [Problema]
revisarAccesos ref es =
  [ Problema AccesoIrregular Advertencia [evOrigen e] [evId e] explicacion
  | (e, uso) <- usosDeTarjeta es
  , Just pid <- [usTitular uso]
  , Just persona <- [buscarPersona ref pid]
  , let tienePermiso = usPuerta uso `elem` perAccesos persona
  , Just explicacion <- [irregular uso persona tienePermiso]
  ]
  where
    irregular uso persona tienePermiso = case (tienePermiso, usResultado uso) of
      (False, Autorizado) -> Just ("el lector dejó pasar la tarjeta " <> usTarjeta uso <> " por " <> usPuerta uso <> ", pero " <> perNombre persona <> " no tiene permiso")
      (True, Denegado) -> Just ("el lector rechazó la tarjeta " <> usTarjeta uso <> " en " <> usPuerta uso <> ", aunque sí tiene permiso")
      _ -> Nothing

usosDeTarjeta :: [Evento] -> [(Evento, UsoDeTarjeta)]
usosDeTarjeta es = [(e, uso) | e <- es, UsoTarjeta uso <- [evContenido e]]

-- ---------------------------------------------------------------------------
-- Relojes: "se fue la luz" / "volvió la energía" en la radio frente a la hora
-- que registró el medidor. Si todas las diferencias van en el mismo sentido,
-- puede ser un reloj desfasado. NO se corrige: eso lo decide una agencia (C5).

revisarRelojes :: Referencias -> [Evento] -> [Problema]
revisarRelojes ref es = [problemaDeReloj ref f grupo | (f, grupo) <- agruparPor fuenteDe diferencias]
  where
    cortes = [(iDesde (evCuando e), evId e) | e <- es, CorteEnergia _ _ <- [evContenido e]]
    vueltas = [(iDesde (evCuando e), evId e) | e <- es, Restablecimiento <- [evContenido e]]

    contraQueSeCompara af = case af of
      SeFueLaLuz -> [("el corte", cortes)]
      VolvioLaLuz -> [("el restablecimiento", vueltas)]
      _ -> []

    diferencias =
      [ (e, que, idMedidor, minutos)
      | e <- es
      , af <- afirmacionesDelEvento e
      , (que, registros) <- contraQueSeCompara af
      , Just (t, idMedidor) <- [masCercano (iDesde (evCuando e)) registros]
      , let minutos = minutosEntre (iDesde (evCuando e)) t
      , abs minutos > tolerancia (evCuando e)
      ]
    fuenteDe (e, _, _, _) = oFuente (evOrigen e)
    masCercano t registros = listToMaybe (sortOn (\(x, _) -> abs (minutosEntre t x)) registros)

-- | Cuántos minutos de diferencia se aceptan según la precisión de la hora.
tolerancia :: Instante -> Integer
tolerancia i = case iPrecision i of
  AlSegundo -> 0
  AlMinuto -> 1
  Aproximada m -> fromIntegral m
  AlDia -> 24 * 60

problemaDeReloj :: Referencias -> Fuente -> [(Evento, Text, Text, Integer)] -> Problema
problemaDeReloj ref f grupo =
  Problema PosibleDesfase Advertencia [evOrigen e | (e, _, _, _) <- grupo] (concat [[evId e, m] | (e, _, m, _) <- grupo]) $
    "la hora de " <> nombreFuente f <> " no coincide con energia.json: "
      <> T.intercalate "; " (map detalle grupo)
      <> "."
      <> conclusion
  where
    minutos = [d | (_, _, _, d) <- grupo]
    signo d = (if d > 0 then "+" else "") <> mostrar d
    detalle (e, que, idMedidor, d) =
      quienDijo ref e <> " lo dice a las " <> T.take 5 (hora (iDesde (evCuando e)))
        <> ", pero " <> que <> " está en " <> idMedidor <> " (" <> signo d <> " min)"
    conclusion
      | length minutos >= 2 && (all (> 0) minutos || all (< 0) minutos) =
          " Todas las diferencias van en el mismo sentido (entre " <> signo (minimum minutos) <> " y " <> signo (maximum minutos)
            <> " min): puede ser un reloj desfasado o reportes tardíos. No se corrige solo: si una agencia lo confirma, se registra como corrección (C5)."
      | otherwise = " Puede ser un reporte tardío o un error de transcripción."

-- ---------------------------------------------------------------------------
-- Luz: alguien dice "sin luz aquí en X", pero el corte registrado no afectó X.

revisarLuz :: Referencias -> [Evento] -> [Problema]
revisarLuz ref es =
  [ Problema Contradiccion Advertencia [evOrigen e] (evId e : map fst activos) (quienDijo ref e <> " " <> explicacion activos m)
  | e <- es
  , SinLuz donde <- afirmacionesDelEvento e
  , let activos = cortesActivos (evCuando e)
  , Just m <- [contradice activos donde]
  ]
  where
    cortesActivos inst =
      [ (idCorte, (afecta, nota))
      | (idCorte, inicio, fin, afecta, nota) <- intervalosDeCorte es
      , inicio <= iHasta inst
      , maybe True (> iDesde inst) fin
      ]
    -- Hay contradicción si no hay ningún corte, o si el corte no afecta ese módulo.
    contradice [] _ = Just Nothing
    contradice activos (Just m)
      | all (\(_, (afecta, _)) -> m `notElem` afecta) activos = Just (Just m)
    contradice _ _ = Nothing
    explicacion [] _ = "dice que no hay luz, pero energia.json no registra ningún corte en ese momento"
    explicacion activos (Just m) =
      "dice que no hay luz en " <> m <> ", pero el corte registrado (" <> T.intercalate ", " (map fst activos) <> ") no afecta " <> m
        <> T.concat [" (nota del medidor: '" <> n <> "')" | (_, (_, Just n)) <- activos]
        <> ". Las dos cosas pueden ser ciertas (equipo con baterías, luces apagadas); el sistema no lo decide"
    explicacion _ Nothing = ""

-- | Cada corte con su fin (el primer restablecimiento posterior, si lo hay).
intervalosDeCorte :: [Evento] -> [(Text, UTCTime, Maybe UTCTime, [ModuloId], Maybe Text)]
intervalosDeCorte es =
  [ (evId e, inicio, listToMaybe (sort [t | t <- vueltas, t > inicio]), afecta, nota)
  | e <- es
  , CorteEnergia afecta nota <- [evContenido e]
  , let inicio = iDesde (evCuando e)
  ]
  where
    vueltas = [iDesde (evCuando e) | e <- es, Restablecimiento <- [evContenido e]]

-- ---------------------------------------------------------------------------
-- Presencia: en algún momento los sensores cuentan menos personas que el personal.

revisarPresencia :: Referencias -> [Evento] -> [Problema]
revisarPresencia ref es =
  [ Problema ConteoIncompleto Advertencia (map evOrigen grupo) (map evId grupo) $
      "a las " <> T.take 5 (hora t) <> " los sensores cuentan " <> mostrar total <> " personas dentro de la estación y el personal es de "
        <> mostrar personal <> ": falta(n) " <> mostrar (personal - total) <> ". Los sensores no cubren el exterior ni identifican a nadie."
  | (t, grupo) <- agruparPor (iDesde . evCuando) conteos
  , let total = sum [n | e <- grupo, ConteoPersonas _ n <- [evContenido e]]
  , total < personal
  ]
  where
    conteos = [e | e <- es, ConteoPersonas _ _ <- [evContenido e]]
    personal = fromIntegral (length (refPersonas ref)) :: Integer

-- ---------------------------------------------------------------------------
-- Temperatura: saltos de 5 °C o más entre lecturas seguidas del mismo sensor.
-- Los saltos encadenados forman un solo episodio.

revisarTemperatura :: [Evento] -> [Problema]
revisarTemperatura es = concatMap episodios (agruparPor sensorDe lecturas)
  where
    lecturas = [(e, s, g) | e <- es, LecturaTemperatura s _ g <- [evContenido e]]
    sensorDe (_, s, _) = s

    episodios (sensor, delSensor) =
      let enOrden = sortOn (\(e, _, _) -> iDesde (evCuando e)) delSensor
          pares = zip enOrden (drop 1 enOrden)
          brusco ((_, _, a), (_, _, b)) = abs (b - a) >= 5
          tramos = groupBy (\x y -> brusco x == brusco y) pares
       in [ episodio sensor primero tramo
          | tramo@(primero : _) <- tramos
          , brusco primero
          ]

    episodio sensor ((inicio, _, desde), _) tramo =
      let (_, (fin, _, hasta)) = last tramo
          eventos = inicio : map (\(_, (e, _, _)) -> e) tramo
       in Problema CambioBrusco Advertencia (map evOrigen eventos) (map evId eventos) $
            sensor <> " pasó de " <> unDecimal desde <> " °C a " <> unDecimal hasta <> " °C entre "
              <> hora (iDesde (evCuando inicio)) <> " y " <> hora (iDesde (evCuando fin))
              <> " (" <> unDecimal (hasta - desde) <> " °C en " <> mostrar (minutosEntre (iDesde (evCuando fin)) (iDesde (evCuando inicio))) <> " min)"

unDecimal :: Double -> Text
unDecimal x = mostrar (fromIntegral (round (x * 10) :: Integer) / 10 :: Double)

-- ---------------------------------------------------------------------------
-- Ayudas

agruparPor :: Ord k => (a -> k) -> [a] -> [(k, [a])]
agruparPor f xs = Map.toList (Map.fromListWith (flip (++)) [(f x, [x]) | x <- xs])

quienDijo :: Referencias -> Evento -> Text
quienDijo ref e = case evContenido e of
  MensajeRadio t -> nombreDeQuien t
  NotaBitacora t -> nombreDeQuien t
  _ -> evId e
  where
    nombreDeQuien t = maybe (idTexto (tlQuien t)) (nombreDe ref) (idPersona (tlQuien t))
