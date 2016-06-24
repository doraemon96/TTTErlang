-module(server_TTT).
-compile(export_all).

start_server() ->
    {ok, LSock} = gen_tcp:listen(8000, [list, {packet, 4}, {active, false}]),

    %%Registramos el nombre de los procesos pBalance y pStat 
    Queue = statistics(run_queue),
    {_, Reductions} = statistics(reductions),
    PidStat = spawn(?MODULE, pStat, []),
    PidBalance = spawn (?MODULE, pBalance, [Queue, Reductions, node()]),
    NameBalance = "balance" ++ integer_to_list(erlang:length(nodes())),
    global:register_name(NameBalance, PidBalance),

    dispatcher(LSock),
    ok.

start_server(Server) ->
    net_kernel:connect_node(Server),
    start_server(),
    ok.

dispatcher(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn(?MODULE, pSocket, [Sock]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    Pid ! ok,
    dispatcher(LSock).

pSocket(Sock) ->
    receive ok -> ok end,
    ok = inet:setopts(Sock, [{active, true}]),
    pSocket_loop(Sock).
pSocket_loop(Sock) ->
    receive 
        {tcp, Sock, Data} ->
            io:format(pCommand(Data, 0, 0) ++ "~n"); %%Encontrar la forma de dar playerId y commandId
        {_} -> 
            io:format("Error en el mensaje")
    end,
    pSocket_loop(Sock).    

pBalance(Queue, Reductions, Node) ->
    receive
        {pSocket, Pid} ->
            Pid ! {pBalance, Node};

        {pStat, {New_Queue, New_Reductions}, New_Node} ->
            case Queue > New_Queue of
                true -> pBalance(New_Queue, New_Reductions, New_Node);
                false ->
                    case Queue < New_Queue of 
                        true -> pBalance(Queue, Reductions, Node);
                        false ->
                            case Reductions > New_Reductions of
                                true -> pBalance(New_Queue, New_Reductions, New_Node);
                                false -> pBalance(Queue, Reductions, Node)
                            end
                    end
            end;

        _ -> {error, not_supported}
    end.

pStat() ->
    Queue = statistics(run_queue),
    {_, Reductions} = statistics(reductions),

    ListBalances = lists:filter(fun(X) -> lists:prefix("balance", X) end, global:registered_names()), 
    lists:map(fun(X) -> global:send(global:whereis_name(X), {pStat, {Queue, Reductions}, node()}) end, ListBalances),

    receive after 5000 -> ok end,
    pStat().


pCommand(Command, PlayerId, GameId) ->
    case string:tokens(Command," ") of
%        ["CON", Nombre] ->
%        ["LSG", CmdId] ->
%        ["NEW", CmdId] ->
%        ["ACC", CmdId, GameId] ->
%        ["PLA", CmdId, GameId, Play] ->
%        ["OBS", CmdId, GameId] ->
%        ["LEA", CmdId, GameId] ->
%        ["BYE"] ->
        _ -> "ERROR not_implemented"
    end.
