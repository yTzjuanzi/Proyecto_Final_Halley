# Halley-7 — sistema forense

Proyecto final de Paradigmas del Lenguaje 2026-03.

| Carpeta | Componente | Estado |
|---|---|---|
| `datos/` | Los 8 archivos de la estación, tal como se recuperaron | listo |
| `integrador-haskell/` | **C1** Integrar la evidencia (Haskell) | funciona |

## C1 — Integrar la evidencia (Haskell)

Lee las 6 fuentes y las 2 referencias de `datos/`, valida cada registro y construye una sola línea de tiempo con **una sola noción de hora** (intervalos UTC) y **una sola noción de identidad** (cada alias o tarjeta se resuelve a una persona, con su nivel de confianza). Ningún dato se descarta sin rastro: cada línea original queda registrada con su estado (`usado`, `descartado`, `encabezado`, `vacia`) y el problema que la explica.

Problemas que detecta: duplicados, registros corruptos, alias de módulos y personas, fechas inferidas, horas aproximadas, tarjetas reportadas perdidas, accesos irregulares, posibles desfases de reloj entre fuentes, contradicciones, conteos de presencia incompletos y cambios bruscos de temperatura.

### Requisitos

- GHC 9.10 (vía GHCup). Solo usa paquetes que vienen con GHC: no hay que descargar nada.

### Compilar y correr

Desde la raíz del repositorio:

```powershell
.\integrador-haskell\build.ps1
.\integrador-haskell\bin\integrador.exe --datos datos --salida salida
```

Escribe `salida/evidencia.json` (personas, módulos, puertas, línea de tiempo, problemas y estado de cada línea original) e imprime un resumen en consola.

Modo en vivo, para la capacidad C3: lee una lectura JSON por línea en stdin y escribe una respuesta JSON por línea en stdout.

```powershell
'{"fuente":"tarjetas","linea":"2041-06-14T03:05:00,LP-04,P5,GEN,RAD,OK"}' | .\integrador-haskell\bin\integrador.exe --vivo --datos datos
```

Resultado esperado: 80 líneas leídas, 56 eventos, 14 problemas.

### Organización del código

El núcleo es puro y el IO vive solo en `app/Main.hs`. Cada línea de cada archivo termina en una `Lectura`: ignorada, referencia, descartada (con el motivo) o usada.

| Módulo | Qué hace |
|---|---|
| `Halley.Tipos` | Tipos algebraicos del dominio: `Fuente`, `Instante`, `Evento`, `Problema`, `Lectura`… |
| `Halley.Texto` | Normalizar texto, separar palabras, distancia de edición |
| `Halley.Tiempo` | Una sola noción de hora: todo se convierte a intervalos UTC |
| `Halley.Json` | JSON con combinadores de parsers (`ReadP`, de `base`) |
| `Halley.Referencias` | Consultas sobre el personal y la estación |
| `Halley.Identidad` | Una sola noción de identidad: nombres, cargos, alias; afirmaciones de los textos |
| `Halley.Fuentes` | Un lector por archivo: línea → `Lectura` |
| `Halley.Validacion` | Duplicados, tarjetas perdidas, accesos, relojes, contradicciones, presencia, temperatura |
| `Halley.Integracion` | C1 completa como una sola función pura (4 pasos) |
| `Halley.Salida` | JSON de salida y resumen de consola |
| `Halley.Vivo` | Lecturas en vivo: paso puro `(estado, línea) -> (estado', respuesta)` |

Explicación detallada para la sustentación: [integrador-haskell/README.md](integrador-haskell/README.md).

## Uso de inteligencia artificial

El análisis inicial y el primer borrador del integrador C1 se hicieron con ayuda de IA (Claude). Se declara en el informe, según §7 del enunciado.
