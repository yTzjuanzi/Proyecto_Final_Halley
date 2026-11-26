:- encoding(utf8).
% Programa principal
% swipl PROLOG/main.pl --pregunta="quien_pudo_usar('LP-04','01:39:47')"
% swipl PROLOG/main.pl --servicio     (para Java)
% swipl PROLOG/main.pl --demo
:- ensure_loaded(motor).
:- use_module(library(main)).
:- initialization(main, main).

main(Argv) :-
    set_stream(user_input, encoding(utf8)),
    set_stream(user_output, encoding(utf8)),
    argv_options(Argv, _, Opciones),
    option(evidencia(Json), Opciones, 'salida/evidencia.json'),
    cargar_evidencia(Json),
    (   option(pregunta(Texto), Opciones) -> leer_pregunta(Texto, P), preguntar(P)
    ;   option(servicio(true), Opciones)  -> servicio
    ;   option(demo(true), Opciones)      -> demo
    ;   uso
    ).

leer_pregunta(Texto, Pregunta) :-
    catch(term_string(Pregunta, Texto), _, Pregunta = sintaxis_invalida(Texto)).

% ---------------------------------------------------------------------------
% Modo servicio

servicio :-
    read_line_to_string(user_input, Linea),
    (   Linea == end_of_file
    ->  true
    ;   (   Linea == "" -> true ; responder_linea(Linea) ),
        flush_output,
        servicio
    ).

responder_linea(Linea0) :-
    quitar_bom(Linea0, Linea),
    (   catch(atom_json_dict(Linea, Dict, [value_string_as(atom)]), _, fail)
    ->  pregunta_de_json(Dict, Pregunta)
    ;   Pregunta = json_invalido(Linea)
    ),
    responder(Pregunta, Respuesta),
    imprimir_json(Pregunta, Respuesta).

% Quita el BOM
quitar_bom(Linea0, Linea) :-
    (   sub_string(Linea0, 0, 1, _, "﻿") -> sub_string(Linea0, 1, _, 0, Linea) ; Linea = Linea0 ).

% Campos por pregunta
forma(donde_pudo_estar,       [persona, hora]).
forma(quien_pudo_estar,       [modulo, desde, hasta]).
forma(recorrido_posible,      [desde, hasta, minutos]).
forma(recorrido_sin_registro, [desde, hasta, minutos, hora]).
forma(quien_pudo_usar,        [tarjeta, hora]).

% JSON a pregunta
pregunta_de_json(Dict, Pregunta) :-
    get_dict(tipo, Dict, Tipo),
    forma(Tipo, Campos),
    valores_de_campos(Campos, Dict, Valores),
    !,
    Pregunta =.. [Tipo|Valores].          % arma la pregunta
pregunta_de_json(Dict, pregunta_incompleta(Dict)).

valores_de_campos([], _, []).
valores_de_campos([Campo|Campos], Dict, [Valor|Valores]) :-
    get_dict(Campo, Dict, Valor),
    valores_de_campos(Campos, Dict, Valores).

% ---------------------------------------------------------------------------
% Demo

preguntas_demo([
    quien_pudo_usar('LP-04', '01:39:47'),
    donde_pudo_estar('Irene', '01:45'),
    quien_pudo_estar('GEN', '01:36', '01:39'),
    quien_pudo_estar('ALM', '01:50', '02:00'),
    donde_pudo_estar(vidal, '02:19'),
    recorrido_posible('LAB-B', 'ALM', 3),
    recorrido_sin_registro('GEN', 'DOR', 5, '01:38'),
    quien_pudo_usar('DV-02', '02:40:05'),
    quien_pudo_usar('AK-06', '01:00'),
    donde_pudo_estar(jefe, '01:50')
]).

demo :-
    preguntas_demo(Ps),
    forall(member(P, Ps), preguntar(P)).

uso :-
    format("Uso:~n"),
    format("  swipl PROLOG/main.pl --pregunta=\"quien_pudo_usar('LP-04','01:39:47')\"~n"),
    format("  swipl PROLOG/main.pl --servicio     (JSON por stdin/stdout)~n"),
    format("  swipl PROLOG/main.pl --demo~n"),
    preguntas_disponibles(Ds),
    format("Preguntas:~n"),
    forall(member(D, Ds), format("  ~w~n", [D])).
