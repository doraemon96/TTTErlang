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
             io:format("Comand ID: {~p, ~p} ~n", [UserName, CmdN]),
             ok   = gen_tcp:send(Sock, Data ++ " " ++ UserName ++ CmdN),
             receive
                {tcp, Sock, "lsg"}    -> io:format(string:centre("Game ID", 22), []),
                                         io:format(string:centre("Player 1", 22), []),
                                         io:format(string:centre("Player 2", 22), []),
                                         io:format("~n", []),
                                         lsg_loop(Sock),
                                         client_loop(Sock, CmdN + 1, UserName);
                {tcp, Sock, "new_ok"} -> receive
                                            {tcp, Sock, ID} -> io:format("~n ***Nueva partida creada con ID: ~p***~n~n", [erlang:list_to_integer(ID)])
                                         end, 
                                         client_loop(Sock, CmdN + 1, UserName);
                {tcp, Sock, "acc"}    -> receive
                                            {tcp, Sock, "not_exists"}   -> io:format("*** Error: la partida no existe~n", []);
                                            {tcp, Sock, "accepted"}     -> io:format("~n||| Bienvenido al juego ??? |||~n~n", []);
                                            {tcp, Sock, "not_accepted"} -> io:format("*** Error: la partida ya está en juego~n", [])
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
