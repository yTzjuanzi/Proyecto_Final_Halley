:- encoding(utf8).
/*  util.pl — Traducir horas y nombres. Aquí no hay razonamiento del caso.
*/

%!  hora_intervalo(+Texto, -Desde-Hasta)
%   'HH:MM' cubre todo ese minuto; 'HH:MM:SS' es un instante.
%   Las horas son del día del caso, en UTC.
hora_intervalo(X, T-T) :- integer(X), !.
hora_intervalo(X, Desde-Hasta) :-
    atomic(X),
    atomic_list_concat(Partes, ':', X),
    maplist(atom_number, Partes, Numeros),
    (   Numeros = [H, M]    -> S = 0, Ancho = 59
    ;   Numeros = [H, M, S] -> Ancho = 0
    ),
    integer(H), integer(M), integer(S),
    between(0, 23, H), between(0, 59, M), between(0, 59, S),
    fecha_caso(Base),
    Desde is Base + H*3600 + M*60 + S,
    Hasta is Desde + Ancho.

%!  epoch_hora(+Epoch, -Texto)   2254786787 -> '01:39:47'
epoch_hora(Epoch, Texto) :-
    stamp_date_time(Epoch, Fecha, 'UTC'),
    format_time(atom(Texto), '%H:%M:%S', Fecha).

%!  normalizar(+Texto, -Atomo)   'Irene Valcárcel' -> 'irene valcarcel'
normalizar(Texto, Normal) :-
    text_to_string(Texto, S0),
    string_lower(S0, S1),
    normalize_space(string(S2), S1),
    string_chars(S2, Cs0),
    maplist(sin_tilde, Cs0, Cs),
    atom_chars(Normal, Cs).

sin_tilde(C0, C) :- ( tilde(C0, C1) -> C = C1 ; C = C0 ).
tilde('á', a).  tilde('é', e).  tilde('í', i).  tilde('ó', o).
tilde('ú', u).  tilde('ü', u).  tilde('ñ', n).

mayusculas(Texto, Atomo) :-
    text_to_string(Texto, S0),
    string_upper(S0, S1),
    normalize_space(atom(Atomo), S1).

%!  persona_canonica(+Texto, -Persona)
%   Acepta el id ('valcarcel'), el nombre ('Irene Valcárcel') o un alias
%   conocido ('Irene', 'la doctora', 'jefe').
persona_canonica(X, P) :-
    atomic(X),
    normalizar(X, N0),
    quitar_articulo(N0, N),
    (   persona(N, _, _)                             -> P = N
    ;   persona(P, Nombre, _), normalizar(Nombre, N) -> true
    ;   alias(N, P)                                  -> true
    ).

quitar_articulo(N0, N) :-
    (   member(Art, ['el ', 'la ']), atom_concat(Art, N1, N0) -> N = N1 ; N = N0 ).

%!  modulo_canonico(+Texto, -Modulo)   'lab_b' -> 'LAB-B', 'Almacén' -> 'ALM'
modulo_canonico(X, M) :-
    atomic(X),
    mayusculas(X, U),
    atomic_list_concat(Partes, '_', U),
    atomic_list_concat(Partes, '-', M0),
    (   modulo(M0, _, _)                                          -> M = M0
    ;   normalizar(X, N), modulo(M, Nombre, _), normalizar(Nombre, N) -> true
    ).

a_numero(X, N) :- number(X), !, N = X.
a_numero(X, N) :- atomic(X), atom_number(X, N).

nombre(P, Nombre) :- ( persona(P, Nombre, _) -> true ; Nombre = P ).

nombres(Personas, Texto) :-
    maplist(nombre, Personas, Nombres),
    lista_texto(Nombres, Texto).

lista_texto([], ninguno) :- !.
lista_texto(Xs, Texto) :- atomic_list_concat(Xs, ', ', Texto).
