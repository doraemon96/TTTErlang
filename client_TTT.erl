-module(client_TTT).
-compile(export_all).

client(Host) ->
    {ok, Sock} = gen_tcp:connect(Host, 8000, [list, {packet, 4}]),
    case username_loop(Sock) of
        {username, UserName} -> 
            client_loop(Sock, 1, UserName, self());
        _ -> 
            {error, not_supported}
    end.

username_loop(Sock) ->
    io:format("Ingrese nombre de usuario: "),
    UserName = string:strip(io:get_line(""), right, $\n),
    ok       = gen_tcp:send(Sock, "CON " ++ UserName),
    receive
        {tcp, Sock, "valid_username"} -> 
            {username, UserName};
        {tcp, Sock, "invalid_username"} ->
            io:format("Usuario en uso.~n"),
            username_loop(Sock);
        _ -> 
            {error, not_supported}
    end.

updater(Sock, PCLoop) ->
    receive 
        {tcp, Sock, Msg} ->
            case Msg of
                "updateacc" ++ GId -> 
                    io:format("Su partida con ID ~p ha sido aceptada: ES SU TURNO~n", [GId]);
                "updatepla" ++ GId ->
                    io:format("Partida ~p: ES SU TURNO~n", [GId]),
                    receive
                        {tcp, Sock, Table} ->
                            print_table(Table)
                    end,
                    receive
                        {tcp, Sock, Status} ->
                            if Status == "won" -> io:format("~nHas perdido la partida!~n~n",[]);
                               Status == "tie" -> io:format("~nLa partida fue un empate!~n~n",[]);
                               true -> ok
                            end
                    end;
                "updateobs" ++ GId ->
                    io:format("Actualización en partida ~p~n", [GId]),
                    receive
                        {tcp, Sock, Table} ->
                            print_table(Table)
                    end,
                    receive
                        {tcp, Sock, Status} ->
                            if Status == "won" -> 
                                    receive
                                        {tcp, Sock, P} -> io:format("~n~pHa ganado la partida!~n~n",[P])
                                    end;
                               Status == "tie" -> io:format("~nLa partida fue un empate!~n~n",[]),
                                    receive
                                        {tcp, Sock, _} -> ok
                                    end;
                               true -> receive
                                        {tcp, Sock, _} -> ok
                                    end
                            end
                    end;
                _ -> PCLoop ! {updater, Msg}
            end; 
        S -> ok = gen_tcp:send(Sock, S)
    end,
    updater(Sock, PCLoop).

client_loop(Sock, CmdN, UserName, UPPid) -> 
    case CmdN of
        0 -> 
            ok;
        1 -> 
            Pid = spawn(?MODULE, updater, [Sock, self()]),
            gen_tcp:controlling_process(Sock, Pid),
            client_loop(Sock, CmdN + 1, UserName, Pid);
        _ -> 
            Data = string:strip(io:get_line("Comando: "), right, $\n), 
            io:format("Command ID: {~p, ~p} ~n", [UserName, CmdN]),
            UPPid ! Data ++ " " ++ UserName ++ erlang:integer_to_list(CmdN),
            receive
                {updater, "lsg"}    -> 
                    receive 
                        {updater, "cmdid"} -> 
                            receive
                                {updater, CmdId} ->
                                    io:format("~nLSG response ~p ~n~n", [CmdId])
                            end
                    end,
                    io:format(string:centre("Game ID", 22), []),
                    io:format(string:centre("Player 1", 22), []),
                    io:format(string:centre("Player 2", 22), []),
                    io:format("~n", []),
                    lsg_loop(),
                    client_loop(Sock, CmdN + 1, UserName, UPPid);
                {updater, "new_ok"} -> 
                    receive
                        {updater, ID} -> 
                            io:format("~n***Nueva partida creada con ID: ~p***~n~n", [erlang:list_to_integer(ID)])
                                           %spawn(?MODULE, new_game, [ID])
                    end, 
                    client_loop(Sock, CmdN + 1, UserName, UPPid);
                {updater, "acc"}    -> 
                    receive
                        {updater, "not_exists"} -> 
                            io:format("~n*** Error: la partida no existe~n~n", []);
                        {updater, "accepted"}   -> 
                            receive
                                {updater, GameId} ->
                                    io:format("~n||| Bienvenido al juego ~p |||~n~n", [erlang:list_to_integer(GameId)]);
                                    _ -> io:format("~n*** Error en la recepción del GameId", [])
                            end;
                        {updater, "not_accepted"} -> 
                            io:format("~n*** Error: la partida ya está en juego~n~n", [])
                    end,
                    client_loop(Sock, CmdN + 1, UserName, UPPid);
                {updater, "pla"}    -> 
                    receive
                        {updater, "success"} ->
                            io:format("SUCCES",[]),
                            receive 
                                {updater, Table} ->
                                    io:format("TABLA",[]),
                                    print_table(Table),
                                    receive
                                        {updater, Status} ->
                                            if Status == "won" -> io:format("~nHas ganado la partida!~n~n",[]);
                                               Status == "tie" -> io:format("~nLa partida fue un empate!~n~n",[]);
                                               true -> ok
                                            end
                                    end,
                                    client_loop(Sock, CmdN + 1, UserName, UPPid);
                                _ -> 
                                    io:format("~n*** Error en la recepción de la tabla luego del comando PLA~n~n", [])
                            end;
                        {updater, "not_allowed"} ->
                            io:format("~nxxx Jugada ilegal xxx~n~n", [])
                    end,
                    client_loop(Sock, CmdN + 1, UserName, UPPid);    
                {updater, "obs"}    ->
                    receive 
                        {updater, "success"} ->
                            receive 
                                {updater, Table} ->
                                    print_table(Table),
                                    client_loop(Sock, CmdN + 1, UserName, UPPid)
                            end;
                        {updater, "not_exists"} ->
                            io:format("La partida seleccionada no existe~n", [])
                    end,
                    client_loop(Sock, CmdN + 1, UserName, UPPid);
                {updater, "lea"}    ->
                    receive
                        {updater, "success"} -> io:format("Has dejado de observar la partida~n", []);
                        {updater, "not_exists"} -> io:format("La partida seleccionada no existe~n", [])
                    end,
                    client_loop(Sock, CmdN + 1, UserName, UPPid);
                {updater, "bye"}    -> 
                    io:format("~n||| ¡Hasta luego! |||~n~n", []),
                    ok = gen_tcp:close(Sock),
                    client_loop(Sock, 0, UserName, UPPid);
                _                   -> 
                    io:format("Comando no implementado ~n"),
                    client_loop(Sock, CmdN + 1, UserName, UPPid)
            end
    end.


%% lsg_loop
%% Loop utilizado para recibir el listado de juegos disponible
lsg_loop() ->
    receive
        {updater, "end"} -> 
            ok;
        {updater, List}  -> 
            Data = string:tokens(List, " "),
            lists:foreach(fun(X) -> io:format(string:centre(X, 22), []) end, Data),
            io:format("~n", []),
            lsg_loop();
        _                  -> 
            io:format("WTH? ~n"),
            lsg_loop()
    end,
    ok.

%% print_table
%% Funcion preety-print para un estado de la tabla de juego.
print_table(Table) ->
    F  = fun(X) -> 
            case X of
                0 -> " ";
                1 -> "X";
                2 -> "O";
                _ -> ""
            end
         end,
    [A1, B1, C1, A2, B2, C2, A3, B3, C3] = lists:map(F, lists:flatten(Table)),
    S1 = "    1   2   3 ~n",
    S2 = "              ~n",
    S3 = "   ---+---+---~n",
    S4 = "a   " ++ A1 ++ " ¦ " ++ B1 ++ " ¦ " ++ C1 ++ " ~n",
    S5 = "b   " ++ A2 ++ " ¦ " ++ B2 ++ " ¦ " ++ C2 ++ " ~n",
    S6 = "c   " ++ A3 ++ " ¦ " ++ B3 ++ " ¦ " ++ C3 ++ " ~n",
    io:format("~n" ++ S1 ++ S2 ++ S4 ++ S3 ++ S5 ++ S3 ++ S6 ++ "~n~n", []).
