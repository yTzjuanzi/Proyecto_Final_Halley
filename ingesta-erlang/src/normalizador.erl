%% La lógica de C3. Todo es PURO: no hay base de datos, red ni reloj.
%%
%%   evaluar(Contexto, Seq, Crudo) ->
%%       {usada, Clave, Evento, Avisos} | {duplicada, Motivo} | {descartada, Motivo}
%%
%% Las reglas deben coincidir con las de C1 (Fuentes.hs y Validacion.hs), y el
%% Evento usa los mismos nombres de campo que Salida.hs.
-module(normalizador).
-export([evaluar/3, clave/1]).

%% Segundos entre el año 0 y 1970 en el calendario gregoriano.
-define(EPOCA_UNIX, 62167219200).
-define(EPOCH_MAXIMO, 100000000000).

evaluar(Ctx, Seq, Crudo) ->
    try json:decode(Crudo) of
        #{<<"id">> := Id, <<"fuente">> := Fuente, <<"linea">> := Linea}
          when is_binary(Id), Id =/= <<>>, is_binary(Fuente), is_binary(Linea) ->
            case binary:match(<<Id/binary, Linea/binary>>, <<0>>) of
                nomatch -> interpretar(Ctx, Seq, Fuente, Linea);
                _ -> {descartada, <<"el mensaje contiene el caracter nulo">>}
            end;
        _ ->
            {descartada, <<"faltan los campos de texto 'id', 'fuente' y 'linea'">>}
    catch
        error:_ -> {descartada, <<"el mensaje no es JSON valido">>}
    end.

interpretar(Ctx, Seq, Fuente, Linea) ->
    try leer(Ctx, Fuente, Linea) of
        {Contenido, Tiempo, Avisos0} ->
            Evento0 = maps:merge(Contenido, comunes(Seq, Fuente, Linea, Tiempo)),
            Clave = clave(Evento0),
            case maps:find(Clave, maps:get(archivados, Ctx)) of
                {ok, Original} ->
                    {duplicada, <<"ya estaba en los archivos: ", Original/binary>>};
                error ->
                    {Evento, Avisos} = revisar(Ctx, Evento0, Avisos0),
                    {usada, Clave, Evento, Avisos}
            end
    catch
        throw:{descartar, Motivo} -> {descartada, Motivo}
    end.

%% ---------------------------------------------------------------------------
%% Un lector por fuente (solo las tres que transmiten en vivo)

leer(Ctx, <<"tarjetas">>, Linea) ->
    case campos(Linea, <<",">>) of
        [Fecha, Tarjeta0, Puerta0, Desde0, Hacia0, Resultado0] ->
            T = fecha_iso(Fecha),
            Tarjeta = string:uppercase(Tarjeta0),
            Persona = buscar(Tarjeta, maps:get(tarjetas, Ctx), <<"tarjeta desconocida">>),
            Puerta = string:uppercase(Puerta0),
            {A, B} = buscar(Puerta, maps:get(puertas, Ctx), <<"puerta desconocida">>),
            {Desde, Av1} = modulo(Ctx, Desde0),
            {Hacia, Av2} = modulo(Ctx, Hacia0),
            exigir(lists:sort([A, B]) =:= lists:sort([Desde, Hacia]),
                   <<"la puerta no conecta esos modulos">>),
            Resultado = string:uppercase(Resultado0),
            exigir(Resultado =:= <<"OK">> orelse Resultado =:= <<"DENEGADO">>,
                   <<"resultado desconocido">>),
            {#{<<"tipo">> => <<"uso_tarjeta">>, <<"tarjeta">> => Tarjeta,
               <<"titular">> => maps:get(<<"id">>, Persona),
               <<"puerta">> => Puerta, <<"modulo_desde">> => Desde,
               <<"modulo_hacia">> => Hacia, <<"resultado">> => Resultado,
               <<"tarjeta_reportada_perdida">> => null},
             {T, T, <<"segundo">>}, Av1 ++ Av2};
        _ ->
            descartar(<<"se esperaban 6 campos separados por ','">>)
    end;
leer(Ctx, <<"presencia">>, Linea) ->
    case campos(Linea, <<";">>) of
        [Hora, Modulo0, Valor0] ->
            T = hora_del_caso(Ctx, Hora),
            Tiempo = {T, T + 59, <<"minuto">>},
            Valor = string:uppercase(Valor0),
            %% La linea no trae fecha: se asume la del caso y se deja constancia.
            Fecha = [aviso(<<"fecha_inferida">>, <<"sin fecha; se asume la fecha del caso">>)],
            case {Modulo0, Valor} of
                {<<"*">>, <<"SIN_ENERGIA">>} ->
                    {sin_energia(null), Tiempo, Fecha};
                {_, <<"SIN_ENERGIA">>} ->
                    {M, Av} = modulo(Ctx, Modulo0),
                    {sin_energia(M), Tiempo, Fecha ++ Av};
                _ ->
                    {M, Av} = modulo(Ctx, Modulo0),
                    N = entero(Valor, 0, 1000, <<"conteo ilegible">>),
                    {#{<<"tipo">> => <<"conteo_presencia">>,
                       <<"modulo">> => M, <<"personas">> => N},
                     Tiempo, Fecha ++ Av}
            end;
        _ ->
            descartar(<<"se esperaban 3 campos separados por ';'">>)
    end;
leer(Ctx, <<"temperatura">>, Linea) ->
    case campos(Linea, <<",">>) of
        [Epoch, Sensor0, Grados] ->
            T = entero(Epoch, 0, ?EPOCH_MAXIMO, <<"epoch ilegible">>),
            Sensor = string:uppercase(Sensor0),
            {M, _} = case Sensor of
                         <<"T-", Nombre/binary>> -> modulo(Ctx, Nombre);
                         _ -> descartar(<<"sensor desconocido">>)
                     end,
            {#{<<"tipo">> => <<"temperatura">>, <<"sensor">> => Sensor,
               <<"modulo">> => M, <<"celsius">> => decimal(Grados)},
             {T, T, <<"segundo">>}, []};
        _ ->
            descartar(<<"se esperaban 3 campos separados por ','">>)
    end;
leer(_Ctx, _Otra, _Linea) ->
    descartar(<<"fuente no admitida en vivo">>).

sin_energia(Modulo) ->
    #{<<"tipo">> => <<"presencia_sin_energia">>, <<"modulo">> => Modulo}.

comunes(Seq, Fuente, Linea, {T0, T1, Precision}) ->
    Id = <<"vivo:", Fuente/binary, ":", (integer_to_binary(Seq))/binary>>,
    #{<<"id">> => Id, <<"fuente">> => Fuente, <<"canal">> => <<"vivo">>,
      <<"linea">> => Seq, <<"t0">> => T0, <<"t1">> => T1,
      <<"desde">> => iso(T0), <<"hasta">> => iso(T1), <<"nominal">> => iso(T0),
      <<"precision">> => Precision, <<"notas">> => [], <<"original">> => Linea}.

%% ---------------------------------------------------------------------------
%% Revisiones sobre una tarjeta: pérdida reportada y accesos irregulares

revisar(Ctx, E = #{<<"tipo">> := <<"uso_tarjeta">>, <<"tarjeta">> := Tarjeta}, Avisos) ->
    Persona = maps:get(Tarjeta, maps:get(tarjetas, Ctx)),
    #{<<"id">> := Pid, <<"accesos">> := Accesos} = Persona,
    #{<<"t0">> := T, <<"puerta">> := Puerta, <<"resultado">> := Resultado} = E,
    Reportes = [Id || {P, Desde, Id} <- maps:get(perdidas, Ctx), P =:= Pid, Desde < T],
    {E1, Av1} =
        case Reportes of
            [Reporte | _] ->
                Nota = <<"tarjeta reportada perdida en ", Reporte/binary,
                         ": este uso no identifica a su titular">>,
                {E#{<<"tarjeta_reportada_perdida">> := Reporte, <<"notas">> := [Nota]},
                 [aviso(<<"tarjeta_perdida">>, Nota)]};
            [] ->
                {E, []}
        end,
    Av2 = case {lists:member(Puerta, Accesos), Resultado} of
              {false, <<"OK">>} ->
                  [aviso(<<"acceso_irregular">>, <<"el lector dejo pasar una tarjeta sin permiso">>)];
              {true, <<"DENEGADO">>} ->
                  [aviso(<<"acceso_irregular">>, <<"el lector rechazo una tarjeta con permiso">>)];
              _ ->
                  []
          end,
    {E1, Avisos ++ Av1 ++ Av2};
revisar(_Ctx, Evento, Avisos) ->
    {Evento, Avisos}.

aviso(Tipo, Descripcion) ->
    #{<<"tipo">> => Tipo, <<"descripcion">> => Descripcion}.

%% ---------------------------------------------------------------------------
%% Cuándo dos eventos son el mismo hecho: fuente, hora y contenido.
%% Sirve para eventos de C1 y para los de C3 porque usan los mismos campos.

clave(E = #{<<"tipo">> := Tipo, <<"t0">> := T}) ->
    case campos_de_clave(Tipo) of
        sin_clave -> sin_clave;
        Campos ->
            Partes = [Tipo, integer_to_binary(T)
                      | [como_texto(maps:get(C, E)) || C <- Campos]],
            iolist_to_binary(lists:join($|, Partes))
    end.

campos_de_clave(<<"uso_tarjeta">>) ->
    [<<"tarjeta">>, <<"puerta">>, <<"modulo_desde">>, <<"modulo_hacia">>, <<"resultado">>];
campos_de_clave(<<"conteo_presencia">>) -> [<<"modulo">>, <<"personas">>];
campos_de_clave(<<"presencia_sin_energia">>) -> [<<"modulo">>];
campos_de_clave(<<"temperatura">>) -> [<<"sensor">>, <<"celsius">>];
campos_de_clave(_) -> sin_clave.

como_texto(null) -> <<"*">>;
como_texto(X) when is_binary(X) -> X;
como_texto(X) when is_integer(X) -> integer_to_binary(X);
como_texto(X) when is_float(X) -> float_to_binary(X, [short]).

%% ---------------------------------------------------------------------------
%% Ayudas de lectura. 'descartar' corta con throw y 'interpretar' lo recoge.

-spec descartar(binary()) -> no_return().
descartar(Motivo) -> throw({descartar, Motivo}).

exigir(true, _) -> ok;
exigir(false, Motivo) -> descartar(Motivo).

buscar(Clave, Mapa, Motivo) ->
    case maps:find(Clave, Mapa) of
        {ok, Valor} -> Valor;
        error -> descartar(Motivo)
    end.

campos(Linea, Separador) ->
    [string:trim(C) || C <- binary:split(Linea, Separador, [global])].

modulo(Ctx, Escrito) ->
    Oficial = string:uppercase(
                binary:replace(Escrito, [<<"_">>, <<" ">>], <<"-">>, [global])),
    exigir(lists:member(Oficial, maps:get(modulos, Ctx)), <<"modulo desconocido">>),
    case Oficial =:= Escrito of
        true -> {Oficial, []};
        false -> {Oficial, [aviso(<<"alias_modulo">>,
                                  <<"modulo escrito '", Escrito/binary,
                                    "'; se interpreta como ", Oficial/binary>>)]}
    end.

entero(Texto, Min, Max, Motivo) ->
    case string:to_integer(Texto) of
        {N, <<>>} when is_integer(N), N >= Min, N =< Max -> N;
        _ -> descartar(Motivo)
    end.

%% Acepta "12", "-6.3". Rechaza valores absurdos para un sensor.
decimal(Texto) ->
    case re:run(Texto, <<"^-?[0-9]{1,3}(\\.[0-9]{1,6})?$">>,
                [{capture, none}, dollar_endonly]) of
        match ->
            case binary:match(Texto, <<".">>) of
                nomatch -> float(binary_to_integer(Texto));
                _ -> binary_to_float(Texto)
            end;
        nomatch ->
            descartar(<<"temperatura ilegible">>)
    end.

%% "2041-06-14T03:05:00" -> segundos Unix (la estación funciona en UTC)
fecha_iso(Texto) ->
    Patron = <<"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})$">>,
    case re:run(Texto, Patron, [{capture, all_but_first, binary}, dollar_endonly]) of
        {match, Partes} ->
            [A, M, D, H, Mi, S] = [binary_to_integer(P) || P <- Partes],
            exigir(calendar:valid_date(A, M, D) andalso H < 24
                   andalso Mi < 60 andalso S < 60,
                   <<"fecha_hora fuera de rango">>),
            a_unix({{A, M, D}, {H, Mi, S}});
        nomatch ->
            descartar(<<"fecha_hora ilegible">>)
    end.

%% "03.05" o "03:05" en la fecha del caso
hora_del_caso(#{fecha := Fecha}, Texto) ->
    Patron = <<"^([0-9]{1,2})[.:]([0-9]{1,2})$">>,
    case re:run(Texto, Patron, [{capture, all_but_first, binary}, dollar_endonly]) of
        {match, Partes} ->
            [H, M] = [binary_to_integer(P) || P <- Partes],
            exigir(H < 24 andalso M < 60, <<"hora fuera de rango">>),
            a_unix({Fecha, {H, M, 0}});
        nomatch ->
            descartar(<<"hora ilegible">>)
    end.

a_unix(FechaHora) ->
    calendar:datetime_to_gregorian_seconds(FechaHora) - ?EPOCA_UNIX.

iso(T) ->
    {{A, M, D}, {H, Mi, S}} =
        calendar:gregorian_seconds_to_datetime(T + ?EPOCA_UNIX),
    iolist_to_binary(
      io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0bZ", [A, M, D, H, Mi, S])).
