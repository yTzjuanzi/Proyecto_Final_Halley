:- encoding(utf8).
% Ubicacion de cada persona

todos_los_modulos(Ms) :- findall(M, modulo(M, _, _), Ms0), sort(Ms0, Ms).
personas(Ps)          :- findall(P, persona(P, _, _), Ps0), sort(Ps0, Ps).

% Tarjeta con dueño
ancla(P, T, Desde, Hacia, Resultado, Id) :-
    paso_tarjeta(Id, T, _, P, _, Desde, Hacia, Resultado),
    P \== desconocido.

% Horas de las fotos
foto_presencia(T) :-
    findall(T0, conteo(_, T0, _, _), Ts0),
    sort(Ts0, Ts),
    member(T, Ts).

% Puertas abiertas sin registro
aperturas_entre(T0, T1, Ids) :-
    findall(Id, ( corte(Id, Inicio, Fin, _), Inicio =< T1, Fin > T0 ), Cortes),
    findall(Id, ( paso_tarjeta(Id, T, _, desconocido, _, _, _, ok), T > T0, T =< T1 ), Pasos),
    append(Cortes, Pasos, Ids0),
    sort(Ids0, Ids).

% Ordena los puntos
puntos(Despues, Hasta, Puntos) :-
    findall(k(T, 0, ancla(P, D, H, R, Id)),
            ( ancla(P, T, D, H, R, Id), T > Despues, T =< Hasta ), Anclas),
    findall(k(T, 1, presencia),
            ( foto_presencia(T), T > Despues, T =< Hasta ), Fotos),
    append(Anclas, Fotos, Todos),
    msort(Todos, Puntos).

% ---------------------------------------------------------------------------
% Estado de todos

estado_en(T, Estado) :-
    personas(Ps),
    todos_los_modulos(Todos),
    findall(P-e(0, Todos, []), member(P, Ps), Inicial),   % al inicio: todos
    puntos(0, T, Puntos),
    foldl(aplicar, Puntos, Inicial, Estado).

% Tarjeta: fija el modulo
aplicar(k(T, _, ancla(P, Desde, Hacia, Resultado, Id)), E0, E) :-
    ( Resultado == ok -> Donde = Hacia ; Donde = Desde ),
    select(P-_, E0, P-e(T, [Donde], [Id]), E).
% Foto: reduce modulos
aplicar(k(T, _, presencia), E0, E) :-
    maplist(alcanzable_en(T), E0, Alcanzables),
    findall(Id, conteo(Id, T, _, _), IdsFoto),
    maplist(limitar(T, IdsFoto, Alcanzables), Alcanzables, E).

alcanzable_en(T, P-e(Tp, Conjunto, Ids), P-a(Modulos, Evidencia)) :-
    alcanzables(Conjunto, Tp, T, Modulos),
    aperturas_entre(Tp, T, Aperturas),
    append(Ids, Aperturas, Evidencia).

limitar(T, IdsFoto, Todos, P-a(Modulos, Ids), P-e(T, Conjunto, Evidencia)) :-
    include(hay_lugar(T, P, Todos), Modulos, Caben),
    (   Caben == []                  % contradiccion: no descarta
    ->  Conjunto = Modulos
    ;   Conjunto = Caben
    ),
    append(Ids, IdsFoto, Ev0),
    sort(Ev0, Evidencia).

% Cabe en el modulo?
hay_lugar(_, _, _, M) :- exterior(M), !.
hay_lugar(T, _, _, M) :- \+ conteo(_, T, M, _), !.
hay_lugar(T, P, Todos, M) :-
    conteo(_, T, M, N),
    aggregate_all(count, ( member(Q-a([M], _), Todos), Q \== P ), Seguros),  % seguros en M
    N > Seguros.

% Modulos alcanzables
alcanzables(Modulos, Tp, T, Alcanzables) :-
    findall(M,
            ( modulo(M, _, _),
              once(( member(A, Modulos), llegada(A, Tp, M, Tl, _, _), Tl =< T )) ),
            Ms),
    sort(Ms, Alcanzables).

% ---------------------------------------------------------------------------
% Donde pudo estar

% Modulos posibles a la hora T
posibles_en(P, T, Modulos, Evidencia) :-
    estado_en(T, Estado),
    memberchk(P-e(Tp, Conjunto, Ids), Estado),
    alcanzables(Conjunto, Tp, T, Alcanzables),
    proximo(P, T, Proximo),
    include(llega_al_proximo(T, Proximo), Alcanzables, Modulos),
    aperturas_entre(Tp, T, Aperturas),
    proximo_ids(Proximo, IdsProximo),
    append([Ids, Aperturas, IdsProximo], Ev0),
    sort(Ev0, Evidencia).

% Siguiente punto
proximo(P, T, Proximo) :-
    findall(k(T1, Orden, Tipo), proximo_punto(P, T, T1, Orden, Tipo), Ks),
    msort(Ks, Ordenados),
    (   Ordenados = [k(T1, _, Tipo)|_]
    ->  restriccion(P, T1, Tipo, Proximo)
    ;   Proximo = ninguno
    ).

proximo_punto(P, T, T1, 0, ancla(Desde, Id)) :- ancla(P, T1, Desde, _, _, Id), T1 > T.
proximo_punto(_, T, T1, 1, presencia)        :- foto_presencia(T1), T1 > T.

restriccion(_, T1, ancla(Desde, Id), proximo(T1, [Desde], [Id])).
restriccion(P, T1, presencia, proximo(T1, Modulos, Ids)) :-
    estado_en(T1, Estado),
    memberchk(P-e(_, Modulos, Ids), Estado).

llega_al_proximo(_, ninguno, _).
llega_al_proximo(T, proximo(T1, Lugares, _), M) :-
    once(( member(B, Lugares), llegada(M, T, B, Tl, _, _), Tl =< T1 )).

proximo_ids(ninguno, []).
proximo_ids(proximo(_, _, Ids), Ids).

% ---------------------------------------------------------------------------
% Intervalos

% Horas clave
instantes(P, T0, T1, Instantes) :-
    findall(T, ( ancla(P, T, _, _, _, _), T > T0, T =< T1 ), As),
    findall(T, ( foto_presencia(T), T > T0, T =< T1 ), Fs),
    append([[T0], As, Fs], Ts),
    sort(Ts, Instantes).

% Parte en tramos
tramos([U], Fin, [U-Fin]).
tramos([U, V|Resto], Fin, [U-V|Tramos]) :- tramos([V|Resto], Fin, Tramos).

% Pudo estar en M
pudo_estar_entre(P, M, T0, T1, Evidencia) :-
    instantes(P, T0, T1, Is),
    tramos(Is, T1, Tramos),
    member(U-V, Tramos),
    estado_en(U, Estado),
    memberchk(P-e(Tp, Conjunto, _), Estado),
    aggregate_all(min(Tl), ( member(A, Conjunto), llegada(A, Tp, M, Tl, _, _) ), Llegada),
    Tx is max(Llegada, U),
    Tx =< V,
    posibles_en(P, Tx, Modulos, Evidencia),
    memberchk(M, Modulos),
    !.

% Seguro en M
seguro_entre(P, M, T0, T1, Evidencia) :-
    instantes(P, T0, T1, Is),
    member(T, Is),
    posibles_en(P, T, [M], Evidencia),
    !.

% Testimonios cercanos
testimonios_cerca(P, T, Textos) :-
    findall(Texto,
            ( testimonio(Id, T0, T1, Quien, P, M),
              T0 =< T + 600, T1 >= T - 600,
              momento(Id, Tn), epoch_hora(Tn, H),
              texto_testimonio(Id, H, Quien, P, M, Texto) ),
            Textos).

texto_testimonio(Id, H, P, P, M, Texto) :- !,
    nombre(P, N),
    format(string(Texto), "~w (~w): ~w dice que estaba en ~w", [Id, H, N, M]).
texto_testimonio(Id, H, Quien, P, M, Texto) :-
    nombre(Quien, NQ), nombre(P, NP),
    format(string(Texto), "~w (~w): según ~w, ~w estaba en ~w", [Id, H, NQ, NP, M]).
