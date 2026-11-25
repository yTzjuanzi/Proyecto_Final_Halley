{-# LANGUAGE OverloadedStrings #-}

-- | Borde de IO del integrador. Todo lo demás es puro.
--
-- > integrador [--datos DIR] [--salida DIR] [--silencioso]
-- >     lee los 8 archivos y escribe DIR_SALIDA/evidencia.json
-- >
-- > integrador --vivo [--datos DIR]
-- >     lee los 8 archivos como contexto y luego normaliza lecturas en vivo:
-- >     una línea JSON por stdin -> una línea JSON por stdout
module Main (main) where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Halley.Integracion
import Halley.Json (codificar, codificarPorLineas)
import Halley.Salida
import Halley.Tipos
import Halley.Vivo
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  args <- getArgs
  let datos = opcion "--datos" "datos" args
      salida = opcion "--salida" "salida" args
  if "--vivo" `elem` args
    then modoVivo datos
    else modoArchivos datos salida ("--silencioso" `notElem` args)

opcion :: String -> String -> [String] -> String
opcion nombre defecto args = case dropWhile (/= nombre) args of
  (_ : valor : _) -> valor
  _ -> defecto

-- | Lee un archivo como UTF-8; si no lo es, como Latin-1, y lo deja anotado.
leerTexto :: Fuente -> FilePath -> IO (Maybe (T.Text, Maybe Problema))
leerTexto fuente ruta = do
  existe <- doesFileExist ruta
  if not existe
    then pure Nothing
    else do
      bytes <- BS.readFile ruta
      pure . Just $ case TE.decodeUtf8' bytes of
        Right t -> (t, Nothing)
        Left _ ->
          ( TE.decodeLatin1 bytes
          , Just
              ( Problema
                  FormatoInesperado
                  Informativo
                  []
                  []
                  (T.pack (archivoFuente fuente) <> " no está en UTF-8; se leyó como Latin-1")
              )
          )

leerFuentes :: FilePath -> IO (M.Map Fuente T.Text, [Problema])
leerFuentes dir = do
  leidos <- mapM (\f -> fmap ((,) f) <$> leerTexto f (dir </> archivoFuente f)) [minBound .. maxBound]
  let presentes = catMaybes leidos
  pure (M.fromList [(f, t) | (f, (t, _)) <- presentes], catMaybes [a | (_, (_, a)) <- presentes])

modoArchivos :: FilePath -> FilePath -> Bool -> IO ()
modoArchivos datos salida verboso = do
  (archivos, avisos) <- leerFuentes datos
  when (M.null archivos) $ hPutStrLn stderr ("No se encontró ningún archivo en " <> datos)
  let r = integrar avisos archivos
      destino = salida </> "evidencia.json"
  createDirectoryIfMissing True salida
  BS.writeFile destino (TE.encodeUtf8 (codificarPorLineas (resultadoJSON r)))
  when verboso (TIO.putStr (resumen r))
  hPutStrLn stderr ("Evidencia integrada escrita en " <> destino)

modoVivo :: FilePath -> IO ()
modoVivo datos = do
  (archivos, avisos) <- leerFuentes datos
  hSetBuffering stdout LineBuffering
  bucle (estadoInicial (integrar avisos archivos))
  where
    bucle est = do
      fin <- isEOF
      unless fin $ do
        linea <- TIO.getLine
        if T.null (T.strip linea)
          then bucle est
          else do
            let (est', respuesta) = pasoVivo est linea
            TIO.putStrLn (codificar respuesta)
            hFlush stdout
            bucle est'
