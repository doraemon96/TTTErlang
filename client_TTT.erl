-module(client_TTT).
-compile(export_all).

client(Host) ->
    {ok , Sock} = gen_tcp:connect(Host, 8000, [list, {packet, 4}]),
    case username_loop(Sock) of
        {username, UserName} -> client_loop(Sock, 1, UserName);
        _ -> {error, not_supported}
    end.

username_loop(Sock) ->
    io:format("Ingrese nombre de usuario: "),
    UserName = string:strip(io:get_line(""), right, $\n),
    ok = gen_tcp:send(Sock, "CON "++UserName),
    receive
        {tcp, Sock, "valid_username"} -> 
            {username, UserName};
        {tcp, Sock, "invalid_username"} ->
            io:format("Usuario en uso.~n"),
            username_loop(Sock);
        _ -> {error, not_supported}
    end.

client_loop(Sock, CmdN, UserName) -> 
    case CmdN of
        0 -> ok;
        _ -> Data = string:strip(io:get_line("Comando: "), right, $\n), 
             io:format("Command ID: {~p, ~p} ~n", [UserName, CmdN]),
             ok   = gen_tcp:send(Sock, Data ++ " " ++ UserName ++ erlang:integer_to_list(CmdN)),
             receive
                {tcp, Sock, "lsg"}    -> receive 
                                            {tcp, Sock, "cmdid"} -> 
                                                receive
                                                    {tcp, Sock, CmdId} ->
                                                        io:format("~nLSG response ~p ~n~n", [CmdId])
                                                end
                                         end,
                                         io:format(string:centre("Game ID", 22), []),
                                         io:format(string:centre("Player 1", 22), []),
                                         io:format(string:centre("Player 2", 22), []),
                                         io:format("~n", []),
                                         lsg_loop(Sock),
                                         client_loop(Sock, CmdN + 1, UserName);
                {tcp, Sock, "new_ok"} -> receive
                                            {tcp, Sock, ID} -> io:format("~n***Nueva partida creada con ID: ~p***~n~n", [erlang:list_to_integer(ID)])
                                                               %spawn(?MODULE, new_game, [ID])
                                         end, 
                                         client_loop(Sock, CmdN + 1, UserName);
                {tcp, Sock, "acc"}    -> receive
                                            {tcp, Sock, "not_exists"}   -> io:format("~n*** Error: la partida no existe~n~n", []);
                                            {tcp, Sock, "accepted"}     -> 
                                                receive
                                                    {tcp, Sock, GameId} ->
                                                        io:format("~n||| Bienvenido al juego ~p |||~n~n", [erlang:list_to_integer(GameId)]);
                                                    _ -> io:format("~n*** Error en la recepción del GameId", [])
                                                end;
                                            {tcp, Sock, "not_accepted"} -> io:format("~n*** Error: la partida ya está en juego~n~n", [])
                                         end,
                                         client_loop(Sock, CmdN + 1, UserName);
                {tcp, Sock, "pla"}    -> receive
                                            {tcp, Sock, "success"} ->
                                                receive 
                                                    {tcp, Sock, Table} ->
                                                        print_table(Table),
                                                        client_loop(Sock, CmdN + 1, UserName);
                                                    _ -> io:format("~n*** Error en la recepción de la tabla luego del comando PLA~n~n", [])
                                                end;
                                            {tcp, Sock, "not_allowed"} ->
                                                io:format("~nxxx Jugada ilegal xxx~n~n", [])
                                         end,
                                         client_loop(Sock, CmdN + 1, UserName);    
                {tcp, Sock, "bye"}    -> io:format("~n||| ¡Hasta luego! |||~n~n", []),
                                         ok = gen_tcp:close(Sock),
                                         client_loop(Sock, 0, UserName);
                _                     -> io:format("Comando no implementado ~n"),
                                         client_loop(Sock, CmdN + 1, UserName)
             end
    end.
    %ok = gen_tcp:close(Sock).
    %client_loop(Sock, CmdN + 1, UserName).


%%
%%
%%
lsg_loop(Sock) ->
    receive
        {tcp, Sock, "end"} -> ok;
        {tcp, Sock, List}  -> Data = string:tokens(List, " "),
                              lists:foreach(fun(X) -> io:format(string:centre(X, 22), []) end, Data),
                              io:format("~n", []),
                              lsg_loop(Sock);
        _                  -> io:format("WTH? ~n"),
                              lsg_loop(Sock)
    end,
    ok.

%¦
print_table(Table) ->
    F  = fun(X) -> case X of
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

%new_game(ID) ->
    
