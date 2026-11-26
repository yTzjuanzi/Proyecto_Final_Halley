:- encoding(utf8).
/*  estacion.pl — El plano de Halley-7 y cuánto se tarda en recorrerlo.

    Reglas del enunciado (§2) que se modelan aquí:
      - pasar de un módulo a otro conectado toma 2 minutos;
      - P1 y P6 no tienen lector: se cruzan sin dejar registro;
      - con energía, una puerta con lector solo abre con tarjeta y lo registra;
      - sin energía, las puertas internas se desbloquean y nada se registra;
      - P8 (exterior) tiene una palanca de emergencia: abre sin tarjeta ni registro.
*/

segundos_por_pasillo(120).

palanca('P8').

exterior(M) :- modulo(M, _, no).            % el exterior no tiene sensor

interna(P) :- puerta(P, A, B, _), \+ exterior(A), \+ exterior(B).

% Las puertas se cruzan en los dos sentidos.
conecta(P, A, B) :- puerta(P, A, B, _).
conecta(P, A, B) :- puerta(P, B, A, _).

%!  corte(?Id, ?Inicio, ?Fin, ?Afecta)
%   Intervalo [Inicio, Fin) sin energía en los módulos de Afecta.
corte(Id, Inicio, Fin, Afecta) :-
    corte_inicio(Id, Inicio, Afecta),
    (   aggregate_all(min(T), (restablecimiento(_, T), T > Inicio), Fin0)
    ->  Fin = Fin0
    ;   Fin = 99999999999                    % nunca se restableció
    ).

% ---------------------------------------------------------------------------
% Caminos. La lista de módulos visitados hace que la búsqueda termine aunque
% el anillo COM-DOR-LAB-A-LAB-B-GEN-RAD-COM sea un ciclo.

%!  camino(+Desde, +Hasta, -Pasos)   Pasos = [paso(Puerta, De, A), ...]
camino(Desde, Hasta, Pasos) :-
    camino(Desde, Hasta, [Desde], Pasos).

camino(M, M, _, []).
camino(Desde, Hasta, Visitados, [paso(P, Desde, Siguiente)|Resto]) :-
    Desde \== Hasta,
    conecta(P, Desde, Siguiente),
    \+ memberchk(Siguiente, Visitados),
    camino(Siguiente, Hasta, [Siguiente|Visitados], Resto).

%!  camino_mas_corto(+Desde, +Hasta, -Pasos, -Minutos)
%   Solo la distancia física: no mira horas, puertas ni tarjetas.
camino_mas_corto(Desde, Hasta, Pasos, Minutos) :-
    aggregate_all(min(N, Ps), (camino(Desde, Hasta, Ps), length(Ps, N)), min(N, Pasos)),
    segundos_por_pasillo(S),
    Minutos is N * S / 60.

% ---------------------------------------------------------------------------
% Cruzar una puerta SIN que la tarjeta propia quede registrada.
% Cada cláusula es una forma posible; el backtracking las prueba todas.

%!  cruce(+Puerta, +De, +A, +T, -Llegada, -Motivo)
%   Alguien que está en De a la hora T puede estar en A a la hora Llegada.
cruce(P, _, _, T, Llegada, sin_lector) :-
    puerta(P, _, _, no),
    segundos_por_pasillo(S), Llegada is T + S.
cruce(P, _, _, T, Llegada, palanca) :-
    palanca(P),
    segundos_por_pasillo(S), Llegada is T + S.
cruce(P, De, A, T, Llegada, corte(Id)) :-          % puerta interna desbloqueada
    puerta(P, _, _, si), interna(P),
    corte(Id, Inicio, Fin, Afecta),
    ( memberchk(De, Afecta) -> true ; memberchk(A, Afecta) ),
    Fin > T,
    segundos_por_pasillo(S), Llegada is max(T, Inicio) + S.
cruce(P, De, A, T, Momento, paso(Id)) :-           % alguien pasó con una tarjeta
    paso_tarjeta(Id, Momento, _, desconocido, P, De, A, ok),   % que no lo identifica
    Momento >= T.

% La evidencia detrás de un cruce: el corte o el registro que lo permitió.
motivo_id(corte(Id), Id).
motivo_id(paso(Id), Id).

ids_de_motivos(Motivos, Ids) :-
    findall(Id, ( member(M, Motivos), motivo_id(M, Id) ), Ids).

%!  llegada(+Desde, +T0, +Hasta, -T, -Pasos, -Motivos)
%   Un recorrido posible y la hora a la que se llega.
llegada(Desde, T0, Hasta, T, Pasos, Motivos) :-
    camino(Desde, Hasta, Pasos),
    recorrer(Pasos, T0, T, Motivos).

recorrer([], T, T, []).
recorrer([paso(P, De, A)|Resto], T0, T, [Motivo|Motivos]) :-
    cruce(P, De, A, T0, T1, Motivo),
    recorrer(Resto, T1, T, Motivos).

%!  primera_llegada(+Desde, +T0, +Hasta, -T, -Pasos, -Motivos)
%   La llegada más temprana. Basta con caminos sin módulos repetidos:
%   como se puede esperar en cualquier módulo, volver a pasar nunca adelanta.
primera_llegada(Desde, T0, Hasta, T, Pasos, Motivos) :-
    aggregate_all(min(T1, r(Ps, Ms)), llegada(Desde, T0, Hasta, T1, Ps, Ms), min(T, r(Pasos, Motivos))).
