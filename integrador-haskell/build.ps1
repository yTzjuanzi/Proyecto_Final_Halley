# Compila el integrador (C1) con GHC directamente: no requiere descargar paquetes.
# Uso (desde cualquier carpeta):  .\integrador-haskell\build.ps1
Push-Location $PSScriptRoot
try {
    New-Item -ItemType Directory -Force bin | Out-Null
    ghc -O1 -Wall -Wno-name-shadowing -Wno-unused-do-bind -isrc -outputdir build -o bin/integrador app/Main.hs
    if ($LASTEXITCODE -ne 0) { throw "La compilación del integrador falló (ver los errores de arriba)" }
}
finally {
    Pop-Location    # vuelve a la carpeta desde la que se llamó
}
