:- encoding(utf8).
% Muestra la respuesta
:- if(exists_source(library(json))).
:- use_module(library(json)).
:- else.
:- use_module(library(http/json)).
:- endif.

% Pregunta en consola
preguntar(Pregunta) :-
    responder(Pregunta, Respuesta),
    imprimir_texto(Pregunta, Respuesta).

imprimir_texto(Pregunta, respuesta(V, Texto, Detalle, Evidencia, Supuestos)) :-
    veredicto_legible(V, VL),
    format("~n? ~q~n", [Pregunta]),
    format("  Veredicto: ~w~n", [VL]),
    format("  ~w~n", [Texto]),
    forall(( member(Clave-Valores, Detalle), lista_de_textos(Valores) ),
           ( format("  ~w:~n", [Clave]),
             forall(member(X, Valores), format("    - ~w~n", [X])) )),
    (   Evidencia == [] -> true
    ;   format("  Evidencia:~n"),
        imprimir_evidencia(Evidencia)
    ),
    (   Supuestos == [] -> true
    ;   format("  Supuestos:~n"),
        forall(member(S, Supuestos), format("    * ~w~n", [S]))
    ).

veredicto_legible(si, 'SÍ').
veredicto_legible(no, 'NO').
veredicto_legible(no_se_puede_saber, 'NO SE PUEDE SABER').
veredicto_legible(pregunta_no_valida, 'PREGUNTA NO VÁLIDA').

lista_de_textos([X|_]) :- string(X).

% Imprime la evidencia
imprimir_evidencia(Ids) :-
    findall(T-Id, ( member(Id, Ids), ( momento(Id, T) -> true ; T = 0 ) ), Pares0),
    keysort(Pares0, Pares),
    pairs_values(Pares, Ordenados),
    maplist(clave_evidencia, Ordenados, Claves0),
    list_to_set(Claves0, Claves),
    forall(member(C, Claves), linea_evidencia(C, Ordenados)).

clave_evidencia(Id, foto(T)) :- conteo(Id, T, _, _), !.
clave_evidencia(Id, id(Id)).

linea_evidencia(foto(T), Ids) :-
    findall(S, ( member(Id, Ids), conteo(Id, T, M, N), format(atom(S), "~w=~w", [M, N]) ), Conteos),
    atomic_list_concat(Conteos, ' ', Texto),
    epoch_hora(T, H),
    format(atom(Etiqueta), "presencia ~w", [H]),
    format("    ~w~t~26|~w~n", [Etiqueta, Texto]).
linea_evidencia(id(Id), _) :-
    (   original(Id, Original)
    ->  split_string(Original, "\n", "", Partes),
        atomic_list_concat(Partes, ' / ', Una),
        format("    ~w~t~26|~w~n", [Id, Una])
    ;   format("    ~w~n", [Id])
    ).

% Respuesta en JSON
imprimir_json(Pregunta, Respuesta) :-
    respuesta_json(Pregunta, Respuesta, Dict),
    json_write_dict(current_output, Dict, [width(0)]),
    nl.

respuesta_json(Pregunta, respuesta(V, Texto, Detalle, Evidencia, Supuestos), Dict) :-
    format(string(P), "~q", [Pregunta]),
    dict_pairs(Det, detalle, Detalle),
    maplist(evidencia_json, Evidencia, Ev),
    Dict = _{pregunta: P, veredicto: V, respuesta: Texto, detalle: Det,
             evidencia: Ev, supuestos: Supuestos}.

evidencia_json(Id, _{id: Id, original: O}) :-
    ( original(Id, O0) -> O = O0 ; O = "" ).
