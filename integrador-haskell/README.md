
# Proyecto paradigmas: Juan Simon Patiño, Juan Felipe Hernandez, Karen Sofia Agudelo

# Cómo funciona el integrador Haskell PARTE C1


Lee los 8 archivos tal como se recuperaron y produce en una sola linea de tiempo una sola hora  y una sola identidad por persona ademas tiene el cuestionamiento de porque se uso como se uso etc.


datos/*.csv, *.txt, *.json
        │
        ▼
 1. Referencias   personal.csv + estacion.csv    quién es quién, cómo es la estación
 2. Lectura       cada línea de cada archivo     una Lectura ignorada o usada o descartada
 3. Validación    todos los eventos juntos       duplicados tarjeta perdida relojes 
 4. Salida        eventos y problemas            salida/evidencia.json  (lo lee Prolog, C2)
```

Los cuatro pasos están numerados en integrar (src/Halley/Integracion.hs). Es el que recibe los textos y devuelve el resultado. El único IO (leer y escribir archivos) está en la app/Main.hs

## Lcada línea termina en una Lectura ejemplo:

```haskell
data Lectura
  = Ignorada Origen               -- encabezado o línea vacía
  | Referencia Origen             -- línea válida de personal.csv o estacion.csv
  | Descartada Origen Problema    -- no se pudo usar: queda el motivo
  | Usada Evento [Problema]       -- entra a la línea de tiempo (con avisos, si hay)
```

Todos los lectores de Fuentes.hs hacen lo mismo una linea y una lectura y sucesivamente. ningún dato se descarta sin dejar rastro nada se pierde o desaparece todo termina en uno de esos cuatro casos.

Despues  en Integracion.hs  se separa con listas por comprensión

```haskell
leidos             = [e | Usada e _ <- lecturas]
problemasDeLectura = [p | Descartada _ p <- lecturas] ++ concat [avisos | Usada _ avisos <- lecturas]
```

## modulos:

| Módulo | Qué tiene | Para explicar |
|---|---|---
| `Tipos.hs` | Todos los tipos: Fuente, Instante, Persona, Evento, Problema, Lectura | Tipos algebraicos: cada caso del dominio es un constructor 
| `Texto.hs` | normalizar, palabras, distancia La distancia es una tabla que se define celda por celda  
| `Tiempo.hs` | Los 5 formatos de hora → intervalos UTC | "01:47" = [01:47:00, 01:47:59]; "aprox." = ±10 min
| `Json.hs` | Escribir y leer JSON sin bibliotecas | El lector está hecho con combinadores de parsers (`ReadP`) 
| `Referencias.hs` | Buscar personas, puertas y módulos (LAB_B o LAB-B) | Consultas puras sobre listas |
| `Identidad.hs` | Alias ("jefe", "la doctora"), quién habla, qué afirma un texto | Reglas explícitas, en una tabla comentada 
| `Fuentes.hs` | Un lector por archivo | Todos con la misma forma: linea y lectura 
| `Validacion.hs` | Revisiones entre fuentes | validar encadena funciones como eventos y problemas 
| `Integracion.hs` | La función integrar  | Composición de todo lo anterior 
| `Salida.hs` | JSON de salida y resumen de consola | Los nombres de campos los usa Prolog 
| `Vivo.hs` | Lecturas en vivo para C3 | pasoVivo :: estado -> línea -> (estado', respuesta)`, pura 
| `app/Main.hs` | Leer archivos, escribir el JSON, bucle en vivo | El único lugar con IO


| Fuente  Cómo viene  Cómo queda 

| tarjetas, energía | `2041-06-14T01:39:47` | instante exacto 
| radio | `[01:47]` + fecha en el encabezado | todo ese minuto 
| presencia | `01.30` (sin fecha) | todo ese minuto, con la fecha del caso inferida 
| temperatura | `2254786200` (epoch Unix) | instante exacto (cae el mismo día: confirma la interpretación) 
| bitácora | `14/06`, `01:50 aprox.` (sin año) | ±10 min, o el día completo si no trae hora 

## Una sola identidad

Cada persona se identifica por su apellido (`valcarcel`). Un texto se resuelve así, en orden de confianza:

1. Exacta: nombre completo, nombre de pila o apellido (`Petrov`, `Irene`).
2. Por cargo: el cargo o un apodo del cargo (`jefe`, `doc`, `glaciologa`).
3. Por contexto: un título visto junto a un apellido. La bitácora dice "la doctora Valcárcel", así que "la doctora" es Valcárcel.
4. Probable: se escribe parecido (`Luka` ~ `Lucas`, distancia 2). Se usa, pero queda marcado como no seguro

## Problemas que pueden suceder

| Tipo | Ejemplo en los datos |
|---|---|
| duplicado | `tarjetas:6` es idéntica a `tarjetas:5` |
| corrupto | `tarjetas:8` (`0#:4?:1?,AK-0?,P2,DO`): diagnostica cada campo y propone `AK-05`, `DOR` |
| tarjeta perdida | LP-04 se usa a las 01:39:47; Petrov la reportó perdida el 12/06 |
| posible desfase | la radio va +7/+8 min respecto al medidor de energía |
| contradicción | "sin luz aquí en radio", pero el corte no afectó RAD |
| alias de persona | `jefe` → Okafor; `Luka` → Petrov (no seguro) |
| alias de módulo | `LAB_B` → `LAB-B` |
| conteo incompleto | a las 02:20 los sensores cuentan 5 de 6 personas |
| cambio brusco | el almacén pasa de 11.8 °C a −6.3 °C entre 01:50 y 02:00 |
| fecha inferida / hora aproximada | presencia sin fecha, bitácora sin año, "01:50 aprox." |


## como se ejecuta

```powershell
.\integrador-haskell\build.ps1
.\integrador-haskell\bin\integrador.exe --datos datos --salida salida
