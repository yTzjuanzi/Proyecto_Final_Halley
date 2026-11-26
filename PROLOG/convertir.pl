:- encoding(utf8).
/*  convertir.pl — ENTRADA de C2.

    Lee la evidencia integrada por C1 (salida/evidencia.json) y la convierte
    en HECHOS de Prolog. Los hechos se cargan una sola vez, como si fueran un
    archivo fuente: ninguna regla del motor agrega ni borra hechos
    (no hay assert ni retract en todo el motor).

    Hechos que produce:
      fecha_caso(Epoch)                         medianoche UTC del día del caso
      modulo(Id, Nombre, Sensor)                Sensor = si | no
      puerta(Id, ModuloA, ModuloB, Lector)      Lector = si | no
      persona(Id, Nombre, Tarjeta)
      permiso(Persona, Puerta)
      alias(Texto, Persona)                     "jefe", "doctora", "irene"...
      paso_tarjeta(Id, T, Tarjeta, Quien, Puerta, Desde, Hacia, Resultado)
                                                Quien = desconocido si la tarjeta
                                                no identifica a nadie (p. ej. perdida)
      tarjeta_perdida(Tarjeta, IdReporte)
      corte_inicio(Id, T, ModulosAfectados)
      restablecimiento(Id, T)
      conteo(Id, T, Modulo, Personas)           sensores de presencia
      testimonio(Id, T0, T1, QuienLoDice, Persona, Modulo)
      original(Id, TextoOriginal)               para mostrar la evidencia
      momento(Id, T)                            hora de cada evento
      origen_puerta(Puerta, Id)                 línea de estacion.csv de cada puerta
*/
:- if(exists_source(library(json))).       % SWI-Prolog 10
:- use_module(library(json)).
:- else.                                    % SWI-Prolog 9
:- use_module(library(http/json)).
:- endif.
:- use_module(library(date)).

predicados_de_hechos([
    fecha_caso/1, modulo/3, puerta/4, persona/3, permiso/2, alias/2,
    paso_tarjeta/8, tarjeta_perdida/2, corte_inicio/3, restablecimiento/2,
    conteo/4, testimonio/6, original/2, momento/2, origen_puerta/2
]).

%!  cargar_evidencia(+ArchivoJson)
%   Convierte la evidencia y carga los hechos (reemplaza los anteriores).
cargar_evidencia(ArchivoJson) :-
    texto_de_hechos(ArchivoJson, Texto),
    setup_call_cleanup(
        open_string(Texto, Flujo),
        load_files(hechos_de_evidencia, [stream(Flujo), silent(true)]),
        close(Flujo)).

%!  exportar_hechos(+ArchivoJson, +ArchivoPl)
%   Escribe los mismos hechos en un archivo, para leerlos con calma.
exportar_hechos(ArchivoJson, ArchivoPl) :-
    texto_de_hechos(ArchivoJson, Texto),
    setup_call_cleanup(
        open(ArchivoPl, write, S, [encoding(utf8)]),
        format(S, ":- encoding(utf8).~n~s", [Texto]),
        close(S)).

texto_de_hechos(ArchivoJson, Texto) :-
    setup_call_cleanup(
        open(ArchivoJson, read, Entrada, [encoding(utf8)]),
        json_read_dict(Entrada, Evidencia, [value_string_as(atom)]),
        close(Entrada)),
    findall(H, hecho(Evidencia, H), Hechos0),
    sort(Hechos0, Hechos),                 % quita repetidos y agrupa por predicado
    predicados_de_hechos(Predicados),
    with_output_to(string(Texto),
        (   format("% Hechos generados desde ~w por convertir.pl. No editar.~n", [ArchivoJson]),
            forall(member(PI, Predicados), format(":- discontiguous(~q).~n", [PI])),
            forall(member(H, Hechos), portray_clause(H))
        )).

% ---------------------------------------------------------------------------
% Un hecho por cada dato del JSON. Backtracking: findall/3 los recoge todos.

hecho(Ev, fecha_caso(Base)) :-
    get_dict(fecha_caso, Ev, Fecha),
    atom_concat(Fecha, 'T00:00:00Z', Iso),
    parse_time(Iso, iso_8601, Stamp),
    Base is truncate(Stamp).
hecho(Ev, modulo(Id, Nombre, Sensor)) :-
    get_dict(modulos, Ev, Ms), member(M, Ms),
    get_dict(id, M, Id), get_dict(nombre, M, Nombre),
    get_dict(sensor_presencia, M, B), si_no(B, Sensor).
hecho(Ev, puerta(Id, A, B, Lector)) :-
    get_dict(puertas, Ev, Ps), member(P, Ps),
    get_dict(id, P, Id), get_dict(a, P, A), get_dict(b, P, B),
    get_dict(lector, P, L), si_no(L, Lector).
hecho(Ev, persona(Id, Nombre, Tarjeta)) :-
    persona_json(Ev, P),
    get_dict(id, P, Id), get_dict(nombre, P, Nombre), get_dict(tarjeta, P, Tarjeta).
hecho(Ev, permiso(Id, Puerta)) :-
    persona_json(Ev, P),
    get_dict(id, P, Id), get_dict(accesos, P, Accesos), member(Puerta, Accesos).
hecho(Ev, alias(Texto, Id)) :-
    persona_json(Ev, P),
    get_dict(id, P, Id), get_dict(alias, P, Alias), member(A, Alias),
    get_dict(texto, A, Texto).
hecho(Ev, paso_tarjeta(Id, T, Tarjeta, Quien, Puerta, Desde, Hacia, Resultado)) :-
    evento(Ev, uso_tarjeta, E),
    get_dict(id, E, Id), get_dict(t0, E, T), get_dict(tarjeta, E, Tarjeta),
    get_dict(puerta, E, Puerta),
    get_dict(modulo_desde, E, Desde), get_dict(modulo_hacia, E, Hacia),
    get_dict(resultado, E, R), resultado(R, Resultado),
    quien_uso(E, Quien).
hecho(Ev, tarjeta_perdida(Tarjeta, Reporte)) :-
    evento(Ev, uso_tarjeta, E),
    get_dict(tarjeta_reportada_perdida, E, Reporte), Reporte \== null,
    get_dict(tarjeta, E, Tarjeta).
hecho(Ev, corte_inicio(Id, T, Afecta)) :-
    evento(Ev, corte_energia, E),
    get_dict(id, E, Id), get_dict(t0, E, T), get_dict(afecta, E, Afecta).
hecho(Ev, restablecimiento(Id, T)) :-
    evento(Ev, restablecimiento_energia, E),
    get_dict(id, E, Id), get_dict(t0, E, T).
hecho(Ev, conteo(Id, T, Modulo, N)) :-
    evento(Ev, conteo_presencia, E),
    get_dict(id, E, Id), get_dict(t0, E, T),
    get_dict(modulo, E, Modulo), get_dict(personas, E, N).
hecho(Ev, testimonio(Id, T0, T1, Quien, Persona, Modulo)) :-
    get_dict(eventos, Ev, Es), member(E, Es),
    get_dict(afirmaciones, E, Afirmaciones), member(A, Afirmaciones),
    get_dict(tipo, A, ubicacion),
    get_dict(persona, A, Persona), get_dict(modulo, A, Modulo),
    get_dict(id, E, Id), get_dict(t0, E, T0), get_dict(t1, E, T1),
    quien_habla(E, Quien).
hecho(Ev, original(Id, Texto)) :-
    get_dict(eventos, Ev, Es), member(E, Es),
    get_dict(id, E, Id), get_dict(original, E, Texto).
hecho(Ev, origen_puerta(Puerta, Id)) :-      % "P4,LAB-B,GEN,si" -> origen_puerta('P4', 'estacion:5')
    linea_de_estacion(Ev, Id, Texto),
    atomic_list_concat([Primero|_], ',', Texto),
    upcase_atom(Primero, Puerta).
hecho(Ev, original(Id, Texto)) :-
    linea_de_estacion(Ev, Id, Texto).
hecho(Ev, momento(Id, T)) :-                 % la hora tal como la escribió la fuente
    get_dict(eventos, Ev, Es), member(E, Es),
    get_dict(id, E, Id), get_dict(nominal, E, Iso),
    parse_time(Iso, iso_8601, Stamp),
    T is truncate(Stamp).

persona_json(Ev, P) :- get_dict(personas, Ev, Ps), member(P, Ps).

% Las líneas válidas de estacion.csv (C1 las deja en "registros").
linea_de_estacion(Ev, Id, Texto) :-
    get_dict(registros, Ev, Rs), member(R, Rs),
    get_dict(fuente, R, estacion), get_dict(estado, R, usado),
    get_dict(origen, R, Id), get_dict(texto, R, Texto).

evento(Ev, Tipo, E) :-
    get_dict(eventos, Ev, Es), member(E, Es), get_dict(tipo, E, Tipo).

si_no(true, si).
si_no(false, no).

resultado('OK', ok).
resultado('DENEGADO', denegado).

% Una tarjeta identifica a su titular, salvo que no tenga titular o que se
% haya reportado perdida antes de usarse.
quien_uso(E, desconocido) :- get_dict(titular, E, null), !.
quien_uso(E, desconocido) :- get_dict(tarjeta_reportada_perdida, E, R), R \== null, !.
quien_uso(E, Titular) :- get_dict(titular, E, Titular).

quien_habla(E, Quien) :-
    (   get_dict(hablante, E, Ident) -> true ; get_dict(autor, E, Ident) ),
    get_dict(persona, Ident, Q),
    (   Q == null -> Quien = desconocido ; Quien = Q ).
