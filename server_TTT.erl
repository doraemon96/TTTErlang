-module(server_TTT).
-compile(export_all).

start_server() ->
    Port = 8000, 
    {ok, LSock} = gen_tcp:listen(Port, [list, {packet, 4}, {active, false}]),

    PidStat = spawn(?MODULE, pStat, []),
    PidBalance = spawn (?MODULE, pBalance, []),
    NameStat = "stat" ++ integer_to_list(erlang:length(nodes())),
    NameBalance = "balance" ++ integer_to_list(erlang:length(nodes())),
    global:register_name(NameStat,PidStat),
    global:register_name(NameBalance,PidBalance),

    dispatcher(LSock).

start_server(Server) ->
    net_kernel:connect_node(Server),

    PidStat = spawn(?MODULE, pStat, []),
    PidBalance = spawn (?MODULE, pBalance, []),
    NameStat = "stat" ++ integer_to_list(erlang:length(nodes())),
    NameBalance = "balance" ++ integer_to_list(erlang:length(nodes())),
    global:register_name(NameStat,PidStat),
    global:register_name(NameBalance,PidBalance),

    start_server(),
    ok.

dispatcher(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid= spawn(?MODULE, psocket, [Sock]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    Pid ! ok,
    dispatcher(LSock).

psocket(Sock) ->
    receive ok -> ok end,
    ok = inet:setopts(Sock, [{active, true}]),
    receive 
        {tcp, Sock, Data} -> 
            io:format("El mensaje que decía ~s~n", Data);
        {_} -> 
            io:format("Error en el mensaje")
    end. 
        

pBalance() ->
    receive
        X -> io:format("Llego~n",[])
    end.

pStat() ->
    Queue = statistics(run_queue),
    {_, Reductions} = statistics(reductions),
    %%TODO: Send statistics to all available nodes
    [{pBalance, Node} ! {pstat, {Queue, Reductions}} || Node <- nodes()],
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
