-module(client_TTT).
-compile(export_all).

client() ->
    Host = "localhost",
    Data = "Funcionaaa",
    {ok , Sock} = gen_tcp:connect(Host, 8000, [binary, {packet, 4}]),
    ok = gen_tcp:send(Sock, Data),
    ok = gen_tcp:close(Sock).
