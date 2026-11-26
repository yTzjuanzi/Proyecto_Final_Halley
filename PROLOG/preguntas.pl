:- encoding(utf8).
% Preguntas y veredictos
:- use_module(library(time)).

preguntas_disponibles([
    "donde_pudo_estar(Persona, Hora)",
    "quien_pudo_estar(Modulo, Desde, Hasta)",
    "recorrido_posible(Desde, Hasta, Minutos)",
    "recorrido_sin_registro(Desde, Hasta, Minutos, Hora)",
    "quien_pudo_usar(Tarjeta, Hora)"
]).

limite_segundos(10).

supuesto(lectores,    "Con energía, quien cruza una puerta con lector deja un registro con su tarjeta.").
supuesto(corte,       "Sin energía, las puertas internas se desbloquean y los lectores no registran.").
supuesto(palanca,     "La puerta exterior P8 se puede abrir con la palanca de emergencia, sin registro.").
supuesto(pasillo,     "Pasar de un módulo a otro toma 2 minutos; un paso con tarjeta ocurre en el instante del registro.").
supuesto(presencia,   "Los conteos de presencia son correctos pero no identifican a nadie; el exterior no tiene sensor.").
supuesto(titular,     "Una tarjeta que no fue reportada perdida la usa su titular.").
supuesto(perdida,     "Una tarjeta reportada perdida no identifica a quien la usa.").
supuesto(testimonios, "Lo que dicen la radio y la bitácora se muestra, pero no se usa para descartar a nadie.").

supuestos(Claves, Textos) :- maplist(supuesto, Claves, Textos).

% ---------------------------------------------------------------------------
% Siempre responde
responder(Pregunta, Respuesta) :-
    limite_segundos(L),
    catch(call_with_time_limit(L, responder_(Pregunta, R)),
          Error,
          respuesta_de_error(Error, R)),
    !,
    Respuesta = R.
responder(_, R) :-
    no_reconocida(R).

respuesta_de_error(time_limit_exceeded,
    respuesta(no_se_puede_saber,
              "La consulta superó el límite de tiempo: no se pudo terminar el razonamiento.",
              [], [], [])) :- !.
respuesta_de_error(Error, respuesta(pregunta_no_valida, Texto, [], [], [])) :-
    format(string(Texto), "No se pudo interpretar la pregunta (~q).", [Error]).

no_reconocida(respuesta(pregunta_no_valida, Texto, [disponibles-Ds], [], [])) :-
    preguntas_disponibles(Ds),
    atomic_list_concat(Ds, '; ', Lista),
    format(string(Texto), "No reconozco esa pregunta. Preguntas disponibles: ~w.", [Lista]).

% Valida y responde
responder_(donde_pudo_estar(PT, HT), R) :- !,
    leer(persona, PT, LP), leer(hora, HT, LH),
    (   LP = ok(P), LH = ok(T-_)
    ->  donde_pudo_estar(P, T, R)
    ;   no_valida([LP, LH], R)
    ).
responder_(quien_pudo_estar(MT, DT, HT), R) :- !,
    leer(modulo, MT, LM), leer(hora, DT, LD), leer(hora, HT, LH),
    (   LM = ok(M), LD = ok(T0-_), LH = ok(_-T1)
    ->  (   T0 =< T1
        ->  quien_pudo_estar(M, T0, T1, R)
        ;   no_valida([error("El intervalo termina antes de empezar.")], R)
        )
    ;   no_valida([LM, LD, LH], R)
    ).
responder_(recorrido_posible(DT, HT, MinT), R) :- !,
    leer(modulo, DT, LD), leer(modulo, HT, LH), leer(minutos, MinT, LM),
    (   LD = ok(D), LH = ok(H), LM = ok(Min)
    ->  recorrido_posible(D, H, Min, R)
    ;   no_valida([LD, LH, LM], R)
    ).
responder_(recorrido_sin_registro(DT, HT, MinT, HoraT), R) :- !,
    leer(modulo, DT, LD), leer(modulo, HT, LH), leer(minutos, MinT, LM), leer(hora, HoraT, LHo),
    (   LD = ok(D), LH = ok(H), LM = ok(Min), LHo = ok(T-_)
    ->  recorrido_sin_registro(D, H, Min, T, R)
    ;   no_valida([LD, LH, LM, LHo], R)
    ).
responder_(quien_pudo_usar(KT, HT), R) :- !,
    leer(tarjeta, KT, LK), leer(hora, HT, LH),
    (   LK = ok(K), LH = ok(T0-T1)
    ->  quien_pudo_usar(K, T0, T1, R)
    ;   no_valida([LK, LH], R)
    ).

leer(persona, X, L) :-
    (   persona_canonica(X, P) -> L = ok(P)
    ;   error_texto("No hay ninguna persona '~w' en personal.csv.", [X], L) ).
leer(modulo, X, L) :-
    (   modulo_canonico(X, M) -> L = ok(M)
    ;   error_texto("No existe el módulo '~w' en estacion.csv.", [X], L) ).
leer(hora, X, L) :-
    (   hora_intervalo(X, I) -> L = ok(I)
    ;   error_texto("La hora '~w' no se entiende; use HH:MM o HH:MM:SS.", [X], L) ).
leer(minutos, X, L) :-
    (   a_numero(X, N), N >= 0 -> L = ok(N)
    ;   error_texto("'~w' no es una cantidad de minutos.", [X], L) ).
leer(tarjeta, X, L) :-
    (   atomic(X) -> mayusculas(X, K), L = ok(K)
    ;   error_texto("'~w' no es una tarjeta.", [X], L) ).

error_texto(Formato, Args, error(Texto)) :- format(string(Texto), Formato, Args).

no_valida(Lecturas, respuesta(pregunta_no_valida, Texto, [], [], [])) :-
    findall(M, member(error(M), Lecturas), Ms),
    atomic_list_concat(Ms, ' ', Texto).

% ---------------------------------------------------------------------------
% 1. Donde pudo estar

donde_pudo_estar(P, T, respuesta(V, Texto, Detalle, Evidencia, Sup)) :-
    posibles_en(P, T, Posibles, Evidencia),
    todos_los_modulos(Todos),
    subtract(Todos, Posibles, Descartados),
    nombre(P, N), epoch_hora(T, H),
    testimonios_cerca(P, T, Testimonios),
    (   Posibles = [M]
    ->  V = si,
        format(string(Texto), "~w estaba en ~w a las ~w.", [N, M, H])
    ;   Posibles = []
    ->  V = no_se_puede_saber,
        format(string(Texto), "La evidencia sobre ~w a las ~w es contradictoria.", [N, H])
    ;   Descartados = []
    ->  V = no_se_puede_saber,
        format(string(Texto), "No se puede saber: la evidencia no descarta ningún módulo para ~w a las ~w.", [N, H])
    ;   V = no_se_puede_saber,
        lista_texto(Posibles, LP), lista_texto(Descartados, LD),
        format(string(Texto), "No se puede saber con certeza. A las ~w, ~w pudo estar en: ~w. No pudo estar en: ~w.", [H, N, LP, LD])
    ),
    Detalle = [persona-P, hora-H, pudo_estar-Posibles, no_pudo_estar-Descartados, testimonios-Testimonios],
    supuestos([lectores, corte, palanca, pasillo, presencia, testimonios], Sup).

% ---------------------------------------------------------------------------
% 2. Quien pudo estar

quien_pudo_estar(M, T0, T1, respuesta(V, Texto, Detalle, Evidencia, Sup)) :-
    personas(Ps),
    maplist(situacion(M, T0, T1), Ps, Situaciones),
    findall(P, member(sit(P, si, _), Situaciones), Seguros),
    findall(P, member(sit(P, no_se_puede_saber, _), Situaciones), Posibles),
    findall(P, member(sit(P, no, _), Situaciones), Descartados),
    findall(E, ( member(sit(_, _, Es), Situaciones), member(E, Es) ), Ev0),
    sort(Ev0, Evidencia),
    (   Seguros \== []  -> V = si
    ;   Posibles \== [] -> V = no_se_puede_saber
    ;   V = no
    ),
    nombres(Seguros, NS), nombres(Posibles, NP), nombres(Descartados, ND),
    epoch_hora(T0, H0), epoch_hora(T1, H1),
    format(string(Texto),
           "En ~w entre ~w y ~w. Estuvo con seguridad: ~w. Pudo estar (no se puede saber): ~w. No pudo estar: ~w.",
           [M, H0, H1, NS, NP, ND]),
    Detalle = [modulo-M, desde-H0, hasta-H1, estuvo-Seguros, pudo_estar-Posibles, no_pudo_estar-Descartados],
    supuestos([lectores, corte, palanca, pasillo, presencia, testimonios], Sup).

situacion(M, T0, T1, P, sit(P, Estado, Evidencia)) :-
    (   seguro_entre(P, M, T0, T1, Evidencia)     -> Estado = si
    ;   pudo_estar_entre(P, M, T0, T1, Evidencia) -> Estado = no_se_puede_saber
    ;   Estado = no, posibles_en(P, T0, _, Evidencia)
    ).

% ---------------------------------------------------------------------------
% 3. Recorridos

% 3a. Solo distancia
recorrido_posible(D, H, Min, respuesta(V, Texto, Detalle, Evidencia, Sup)) :-
    camino_mas_corto(D, H, Pasos, Necesarios),
    lineas_de_puertas(Pasos, Evidencia),
    texto_camino(D, Pasos, Camino),
    length(Pasos, N),
    (   Necesarios =< Min
    ->  V = si,
        format(string(Texto), "Sí. El camino más corto (~w) tiene ~w pasillos: ~w min, y hay ~w min.", [Camino, N, Necesarios, Min])
    ;   V = no,
        format(string(Texto), "No. El camino más corto (~w) tiene ~w pasillos: ~w min, más que los ~w min disponibles.", [Camino, N, Necesarios, Min])
    ),
    Detalle = [camino-Camino, pasillos-N, minutos_necesarios-Necesarios],
    supuestos([pasillo], Sup).

% 3b. Sin registro
recorrido_sin_registro(D, H, Min, T, respuesta(V, Texto, Detalle, Evidencia, Sup)) :-
    epoch_hora(T, Hora),
    supuestos([lectores, corte, palanca, pasillo], Sup),
    (   primera_llegada(D, T, H, Llegada, Pasos, Motivos)
    ->  Segundos is Llegada - T,
        Duracion is round(Segundos / 6) / 10,          % minutos con un decimal
        texto_camino(D, Pasos, Camino),
        texto_puertas(Pasos, Motivos, Puertas),
        epoch_hora(Llegada, HoraLlegada),
        ids_de_motivos(Motivos, IdsMotivos),
        lineas_de_puertas(Pasos, IdsPuertas),
        append(IdsMotivos, IdsPuertas, Ev0), sort(Ev0, Evidencia),
        (   Segundos =< Min * 60
        ->  V = si,
            format(string(Texto), "Sí. Saliendo de ~w a las ~w se llega a ~w a las ~w (~w min) por ~w. Puertas: ~w.",
                   [D, Hora, H, HoraLlegada, Duracion, Camino, Puertas])
        ;   V = no,
            format(string(Texto), "No. Saliendo de ~w a las ~w, lo más pronto que se llega a ~w es a las ~w (~w min), por ~w. Puertas: ~w.",
                   [D, Hora, H, HoraLlegada, Duracion, Camino, Puertas])
        ),
        Detalle = [camino-Camino, llegada-HoraLlegada, minutos-Duracion]
    ;   V = no, Evidencia = [], Detalle = [],
        format(string(Texto), "No. Sin dejar registro no hay forma de ir de ~w a ~w después de las ~w: las puertas con lector siguen cerradas.",
               [D, H, Hora])
    ).

% Lineas de las puertas
lineas_de_puertas(Pasos, Ids) :-
    findall(Id, ( member(paso(P, _, _), Pasos), origen_puerta(P, Id) ), Ids).

texto_camino(Desde, Pasos, Texto) :-
    findall(A, member(paso(_, _, A), Pasos), Siguientes),
    atomic_list_concat([Desde|Siguientes], ' -> ', Texto).

texto_puertas(Pasos, Motivos, Texto) :-
    maplist(texto_puerta, Pasos, Motivos, Partes),
    atomic_list_concat(Partes, ', ', Texto).

texto_puerta(paso(P, _, _), Motivo, Texto) :-
    motivo_legible(Motivo, M),
    format(atom(Texto), "~w (~w)", [P, M]).

motivo_legible(sin_lector, 'sin lector').
motivo_legible(palanca,    'palanca de emergencia').
motivo_legible(corte(Id),  M) :- format(atom(M), "desbloqueada por el corte ~w", [Id]).
motivo_legible(paso(Id),   M) :- format(atom(M), "abierta con una tarjeta que no identifica a nadie, ~w", [Id]).

% ---------------------------------------------------------------------------
% 4. Quien uso la tarjeta

quien_pudo_usar(K, T0, T1, R) :-
    (   paso_tarjeta(Id, T, K, Quien, Puerta, Desde, Hacia, Res), T >= T0, T =< T1
    ->  uso_registrado(Id, T, K, Quien, Puerta, Desde, Hacia, Res, R)
    ;   sin_registro(K, T0, R)
    ).

% Tarjeta con dueño
uso_registrado(Id, T, K, Quien, Puerta, Desde, Hacia, Res, respuesta(si, Texto, Detalle, [Id], Sup)) :-
    Quien \== desconocido, !,
    nombre(Quien, N), epoch_hora(T, H), resultado_legible(Res, RL),
    format(string(Texto), "La usó su titular, ~w: ~w en ~w (~w -> ~w) a las ~w.", [N, RL, Puerta, Desde, Hacia, H]),
    Detalle = [tarjeta-K, candidatos-[Quien]],
    supuestos([titular], Sup).
% Tarjeta sin dueño
uso_registrado(Id, T, K, desconocido, Puerta, Desde, Hacia, _, respuesta(V, Texto, Detalle, Evidencia, Sup)) :-
    personas(Ps),
    findall(P-Ms-Ev, ( member(P, Ps), posibles_en(P, T, Ms, Ev) ), Ubicaciones),
    findall(P, ( member(P-Ms-_, Ubicaciones), memberchk(Desde, Ms) ), Candidatos),
    findall(P, ( member(P-Ms-_, Ubicaciones), \+ memberchk(Desde, Ms) ), Descartados),
    findall(S, ( member(P-Ms-_, Ubicaciones), nombre(P, NP), lista_texto(Ms, LM),
                 format(string(S), "~w pudo estar en: ~w", [NP, LM]) ), PorPersona),
    findall(E, ( member(_-_-Es, Ubicaciones), member(E, Es) ), Ev0),
    ( tarjeta_perdida(K, Reporte) -> Extra = [Reporte] ; Extra = [] ),
    append([[Id], Extra, Ev0], Ev1), sort(Ev1, Evidencia),
    epoch_hora(T, H),
    origen_tarjeta(K, Origen),
    (   Candidatos = [Unica]
    ->  V = si, nombre(Unica, NU),
        format(string(Texto),
               "La tarjeta ~w ~w. Se usó a las ~w en ~w (~w -> ~w). Solo ~w pudo estar en ~w en ese momento: fue ~w.",
               [K, Origen, H, Puerta, Desde, Hacia, NU, Desde, NU])
    ;   Candidatos = []
    ->  V = no_se_puede_saber,
        format(string(Texto),
               "La tarjeta ~w ~w. Se usó a las ~w en ~w (~w -> ~w), pero según la evidencia nadie del personal pudo estar en ~w: hay una contradicción o alguien no registrado.",
               [K, Origen, H, Puerta, Desde, Hacia, Desde])
    ;   V = no_se_puede_saber, nombres(Candidatos, NC),
        format(string(Texto),
               "La tarjeta ~w ~w. Se usó a las ~w en ~w (~w -> ~w). Pudieron usarla: ~w. No se puede saber cuál.",
               [K, Origen, H, Puerta, Desde, Hacia, NC])
    ),
    Detalle = [tarjeta-K, candidatos-Candidatos, descartados-Descartados, ubicaciones-PorPersona],
    supuestos([perdida, lectores, corte, pasillo, presencia], Sup).

origen_tarjeta(K, Texto) :-
    (   persona(_, N, K), tarjeta_perdida(K, Rep)
    ->  format(string(Texto), "es de ~w, que la reportó perdida (~w)", [N, Rep])
    ;   persona(_, N, K)
    ->  format(string(Texto), "es de ~w", [N])
    ;   Texto = "no aparece en personal.csv"
    ).

resultado_legible(ok, 'pasó').
resultado_legible(denegado, 'le negaron el paso').

% Sin registro
sin_registro(K, T0, respuesta(V, Texto, [tarjeta-K], Evidencia, Sup)) :-
    epoch_hora(T0, H),
    (   \+ persona(_, _, K), \+ paso_tarjeta(_, _, K, _, _, _, _, _)
    ->  V = no, Evidencia = [], Sup = [],
        format(string(Texto), "No existe la tarjeta ~w: no aparece en personal.csv ni en tarjetas.csv.", [K])
    ;   corte(Id, Inicio, Fin, _), Inicio =< T0, T0 < Fin
    ->  V = no_se_puede_saber, Evidencia = [Id], supuestos([corte], Sup),
        format(string(Texto), "No se puede saber: a las ~w había un corte de energía (~w) y los lectores no registraban.", [H, Id])
    ;   V = no, Evidencia = [], supuestos([lectores], Sup),
        format(string(Texto), "No hay ningún registro de la tarjeta ~w a las ~w.", [K, H])
    ).
