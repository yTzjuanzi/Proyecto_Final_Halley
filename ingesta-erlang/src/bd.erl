%% Único módulo que habla con PostgreSQL.
-module(bd).
-export([conectar/0, cerrar/1, recibir/3, pendientes/2, registrar/3]).

conectar() ->
    epgsql:connect(#{host => os:getenv("HALLEY_BD_HOST", "127.0.0.1"),
                     port => list_to_integer(os:getenv("HALLEY_BD_PUERTO", "5432")),
                     username => os:getenv("HALLEY_BD_USUARIO", "halley"),
                     password => os:getenv("HALLEY_BD_CLAVE", ""),
                     database => os:getenv("HALLEY_BD_NOMBRE", "halley"),
                     timeout => 5000}).

cerrar(Bd) -> epgsql:close(Bd).

%% Cada llamada es una transacción: al volver, ya quedó confirmada.
recibir(Bd, Id, Crudo) ->
    uno(epgsql:equery(Bd, "SELECT recibir_lectura($1::text, $2::bytea)", [Id, Crudo])).

pendientes(Bd, Limite) ->
    Sql = "SELECT l.seq, l.crudo FROM lectura_viva l "
          "LEFT JOIN resultado_vivo r ON r.seq = l.seq "
          "WHERE r.seq IS NULL ORDER BY l.seq LIMIT $1::int4",
    case epgsql:equery(Bd, Sql, [Limite]) of
        {ok, _Columnas, Filas} -> {ok, Filas};
        {error, Razon} -> {error, Razon}
    end.

%% Resultado = {usada, Clave, Evento, Avisos} | {descartada, Motivo}
registrar(Bd, Seq, {usada, Clave, Evento, Avisos}) ->
    guardar(Bd, [Seq, <<"usada">>, null, texto_json(Avisos), Clave, texto_json(Evento)]);
registrar(Bd, Seq, {duplicada, Motivo}) ->
    guardar(Bd, [Seq, <<"duplicada">>, Motivo, <<"[]">>, null, null]);
registrar(Bd, Seq, {descartada, Motivo}) ->
    guardar(Bd, [Seq, <<"descartada">>, Motivo, <<"[]">>, null, null]).

guardar(Bd, Parametros) ->
    uno(epgsql:equery(Bd,
        "SELECT registrar_resultado($1::int8, $2::text, $3::text, $4::jsonb, $5::text, $6::jsonb)",
        Parametros)).

texto_json(Termino) -> iolist_to_binary(json:encode(Termino)).

uno({ok, _Columnas, [{Valor}]}) -> {ok, Valor};
uno({error, Razon}) -> {error, Razon}.
