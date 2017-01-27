-module(client_TTT).
-compile(export_all).

client(Host) ->
    {ok , Sock} = gen_tcp:connect(Host, 8000, [list, {packet, 4}]),
    case username_loop(Sock) of
        {username, UserName} -> ok;
        _ -> {error, not_supported}
    end,
    client_loop(Sock).

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

client_loop(Sock) -> 
    Data = string:strip(io:get_line("Comando:"), right, $\n),
    ok = gen_tcp:send(Sock, Data),
    receive
        {tcp, Sock, "lsg"} -> lsg_loop(Sock);
        _ -> io:format("Comando no implementado")
    end,
    %ok = gen_tcp:close(Sock).
    client_loop(Sock).


%%
%%
%%
lsg_loop(Sock) ->
    receive
        {tcp, Sock, "end"} -> ok;
        {tcp, Sock, Tuple} -> io:format("~p~n", [binary_to_term(Tuple)]),
                              lsg_loop(Sock)
    end
    ok.
