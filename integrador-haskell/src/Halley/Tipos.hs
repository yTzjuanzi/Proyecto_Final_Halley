{-# LANGUAGE OverloadedStrings #-}

-- | Los tipos del dominio. Aquí solo se describe QUÉ es cada dato.
--   Cómo se lee está en "Halley.Fuentes" y cómo se revisa en "Halley.Validacion".
module Halley.Tipos where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime)

-- ---------------------------------------------------------------------------
-- De dónde viene cada dato

data Fuente
  = Tarjetas
  | Radio
  | Energia
  | Presencia
  | Temperatura
  | Bitacora
  | Personal
  | Estacion
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | "tarjetas", "radio", ...
nombreFuente :: Fuente -> Text
nombreFuente = T.toLower . T.pack . show

-- | "tarjetas.csv", "radio.txt", ...
archivoFuente :: Fuente -> FilePath
archivoFuente f = T.unpack (nombreFuente f) <> extension
  where
    extension = case f of
      Radio -> ".txt"
      Bitacora -> ".txt"
      Energia -> ".json"
      _ -> ".csv"

-- | Un dato llega de un archivo recuperado o de la transmisión en vivo.
data Canal = Archivo | EnVivo
  deriving (Eq, Ord, Show)

-- | La línea exacta de donde salió un dato, con su texto original sin tocar.
data Origen = Origen
  { oFuente :: Fuente
  , oCanal :: Canal
  , oLinea :: Int
  , oTexto :: Text
  }
  deriving (Eq, Ord, Show)

-- | Identificador legible: "tarjetas:8", "vivo:presencia:3".
idOrigen :: Origen -> Text
idOrigen o = prefijo <> nombreFuente (oFuente o) <> ":" <> T.pack (show (oLinea o))
  where
    prefijo = if oCanal o == EnVivo then "vivo:" else ""

-- ---------------------------------------------------------------------------
-- Tiempo: una sola noción de hora. Todo es un intervalo UTC [desde, hasta].

data Precision
  = AlSegundo -- "01:39:47"
  | AlMinuto -- "[01:47]": cualquier segundo de ese minuto
  | Aproximada Int -- "01:50 aprox.": más o menos N minutos
  | AlDia -- "12/06": cualquier hora de ese día
  deriving (Eq, Ord, Show)

data Instante = Instante
  { iDesde :: UTCTime
  , iHasta :: UTCTime
  , iPrecision :: Precision
  }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Personas y estación

type PersonaId = Text -- el apellido normalizado: "valcarcel"
type TarjetaId = Text -- "IV-07"
type ModuloId = Text -- "LAB-B"
type PuertaId = Text -- "P4"

data Persona = Persona
  { perId :: PersonaId
  , perNombre :: Text
  , perCargo :: Text
  , perTarjeta :: TarjetaId
  , perAccesos :: [PuertaId]
  }
  deriving (Eq, Ord, Show)

data Puerta = Puerta
  { puId :: PuertaId
  , puA :: ModuloId
  , puB :: ModuloId
  , puLector :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Qué tan segura es una identificación. El orden importa: 'Exacta' es la mejor.
data Confianza
  = Exacta -- nombre o apellido de personal.csv
  | PorCargo -- "jefe", "doc"
  | PorContexto -- "la doctora": otra fuente dice "la doctora Valcárcel"
  | Probable -- se escribe parecido ("Luka" ~ "Lucas"): NO es seguro
  | SinResolver
  deriving (Eq, Ord, Show)

-- | El resultado de identificar a alguien a partir de un texto.
data Identificacion = Identificacion
  { idPersona :: Maybe PersonaId
  , idTexto :: Text
  , idConfianza :: Confianza
  , idMotivo :: Text
  }
  deriving (Eq, Ord, Show)

-- | Una forma de nombrar a una persona ("jefe", "irene", "doctora").
data Alias = Alias
  { alPalabras :: [Text]
  , alPersona :: PersonaId
  , alConfianza :: Confianza
  , alMotivo :: Text
  }
  deriving (Eq, Show)

-- | Lo que se sabe ANTES de leer la evidencia: quién es quién y cómo es la estación.
data Referencias = Referencias
  { refPersonas :: [Persona]
  , refPuertas :: [Puerta]
  , refModulos :: [ModuloId]
  , refAlias :: [Alias]
  , refFecha :: Day -- fecha del caso, inferida de las fuentes que la traen
  }

-- ---------------------------------------------------------------------------
-- Evidencia

data ResultadoPuerta = Autorizado | Denegado
  deriving (Eq, Ord, Show)

data UsoDeTarjeta = UsoDeTarjeta
  { usTarjeta :: TarjetaId
  , usTitular :: Maybe PersonaId
  , usPuerta :: PuertaId
  , usDesde :: ModuloId
  , usHacia :: ModuloId
  , usResultado :: ResultadoPuerta
  , usReportadaPerdida :: Maybe Text -- id del evento donde se reportó perdida
  }
  deriving (Eq, Ord, Show)

-- | Lo que un texto libre (radio o bitácora) afirma de forma reconocible.
--   Son testimonios: valen menos que un registro de sensor.
data Afirmacion
  = EstaEn PersonaId ModuloId -- "la doctora sigue en el B"
  | SeFueLaLuz -- un cambio: empezó un corte
  | SinLuz (Maybe ModuloId) -- un estado: "sigo sin luz aquí en radio"
  | VolvioLaLuz
  | PerdioTarjeta PersonaId -- "Petrov dice que perdió su tarjeta"
  | PuertaExterior -- se menciona la puerta exterior
  deriving (Eq, Ord, Show)

data TextoLibre = TextoLibre
  { tlQuien :: Identificacion -- quien habla o escribe
  , tlTexto :: Text
  , tlMenciones :: [Identificacion] -- personas nombradas en el texto
  , tlAfirmaciones :: [Afirmacion]
  }
  deriving (Eq, Ord, Show)

data Contenido
  = UsoTarjeta UsoDeTarjeta
  | MensajeRadio TextoLibre
  | NotaBitacora TextoLibre
  | ConsumoElectrico [(ModuloId, Double)]
  | CorteEnergia [ModuloId] (Maybe Text) -- módulos afectados y nota
  | Restablecimiento
  | ConteoPersonas ModuloId Integer
  | PresenciaSinEnergia (Maybe ModuloId) -- Nothing = todos los módulos
  | LecturaTemperatura Text ModuloId Double -- sensor, módulo, °C
  deriving (Eq, Ord, Show)

-- | Un hecho de la noche, con su hora y la línea de donde salió.
data Evento = Evento
  { evId :: Text
  , evOrigen :: Origen
  , evCuando :: Instante
  , evContenido :: Contenido
  , evNotas :: [Text]
  }
  deriving (Eq, Show)

afirmacionesDelEvento :: Evento -> [Afirmacion]
afirmacionesDelEvento e = case evContenido e of
  MensajeRadio t -> tlAfirmaciones t
  NotaBitacora t -> tlAfirmaciones t
  _ -> []

-- ---------------------------------------------------------------------------
-- Problemas: ningún dato se descarta sin dejar rastro

data Severidad
  = Informativo -- el dato se usa; solo se deja constancia
  | Advertencia -- el dato se usa, pero hay que tenerlo en cuenta
  | Descartado -- el dato NO entra a la línea de tiempo
  deriving (Eq, Ord, Show)

data TipoProblema
  = Corrupto
  | Duplicado
  | FormatoInesperado
  | AliasModulo
  | AliasPersona
  | FechaInferida
  | HoraAproximada
  | TarjetaPerdida
  | AccesoIrregular
  | PosibleDesfase
  | Contradiccion
  | ConteoIncompleto
  | CambioBrusco
  deriving (Eq, Ord, Show)

data Problema = Problema
  { prTipo :: TipoProblema
  , prSeveridad :: Severidad
  , prOrigenes :: [Origen] -- las líneas involucradas
  , prEventos :: [Text] -- los eventos involucrados
  , prDescripcion :: Text
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- El patrón central: lo que pasa con cada línea leída

data Lectura
  = Ignorada Origen -- encabezado o línea vacía
  | Referencia Origen -- línea válida de personal.csv o estacion.csv
  | Descartada Origen Problema -- no se pudo usar: queda el motivo
  | Usada Evento [Problema] -- entra a la línea de tiempo (con avisos, si hay)

origenDeLectura :: Lectura -> Origen
origenDeLectura l = case l of
  Ignorada o -> o
  Referencia o -> o
  Descartada o _ -> o
  Usada e _ -> evOrigen e
