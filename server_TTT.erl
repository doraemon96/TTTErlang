-module(server_TTT).
-compile(export_all).

start_server() ->
    {ok, LSock} = gen_tcp:listen(8000, [list, {packet, 4}, {active, false}]),

    %%Registramos el nombre de los procesos pBalance y pStat 
    PidStat = spawn(?MODULE, pStat, []),
    PidBalance = spawn (?MODULE, pBalance, []),
    NameStat = "stat" ++ integer_to_list(erlang:length(nodes())),
    NameBalance = "balance" ++ integer_to_list(erlang:length(nodes())),
    global:register_name(NameStat,PidStat),
    global:register_name(NameBalance,PidBalance),

    dispatcher(LSock),
    ok.

start_server(Server) ->
    net_kernel:connect_node(Server),
    start_server(),
    ok.

dispatcher(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn(?MODULE, psocket, [Sock]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    Pid ! ok,
    dispatcher(LSock).

pSocket(Sock) ->
    receive ok -> ok end,
    ok = inet:setopts(Sock, [{active, true}]),
    receive 
        {tcp, Sock, Data} -> 
            io:format("El mensaje que decía ~p~n", [Data]);
        {_} -> 
            io:format("Error en el mensaje")
    end. 
        

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

    ListNumbers = lists:seq(0,erlang:length(nodes()) - 1),
    ListBalances = lists:map((fun(X) -> "balance" ++ integer_to_list(X) end), ListNumbers), 
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
