%% Lo que C3 necesita de C1 (salida/evidencia.json). Funciones puras.
-module(contexto).
-export([desde_json/1]).

desde_json(#{<<"fecha_caso">> := Fecha, <<"personas">> := Personas,
             <<"modulos">> := Modulos, <<"puertas">> := Puertas,
             <<"eventos">> := Eventos}) ->
    <<A:4/binary, "-", M:2/binary, "-", D:2/binary>> = Fecha,
    #{fecha => {binary_to_integer(A), binary_to_integer(M), binary_to_integer(D)},
      tarjetas => maps:from_list(
                    [{maps:get(<<"tarjeta">>, P), P} || P <- Personas]),
      modulos => [maps:get(<<"id">>, X) || X <- Modulos],
      puertas => maps:from_list(
                   [{maps:get(<<"id">>, P),
                     {maps:get(<<"a">>, P), maps:get(<<"b">>, P)}} || P <- Puertas]),
      perdidas => perdidas(Eventos),
      archivados => claves_archivadas(Eventos)}.

%% [{Persona, DesdeCuando, IdDelReporte}]
perdidas(Eventos) ->
    [{maps:get(<<"persona">>, A), maps:get(<<"t0">>, E), maps:get(<<"id">>, E)}
     || E <- Eventos,
        A <- maps:get(<<"afirmaciones">>, E, []),
        maps:get(<<"tipo">>, A) =:= <<"tarjeta_perdida">>].

%% Para reconocer una lectura en vivo que ya estaba en los archivos.
claves_archivadas(Eventos) ->
    maps:from_list(
      [{Clave, maps:get(<<"id">>, E)}
       || E <- Eventos,
          Clave <- [normalizador:clave(E)],
          Clave =/= sin_clave]).
