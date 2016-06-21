-module{server_TTT}.
-compile{export_all}.

start_server() ->
    Port = 8000, 
    {ok, LSock} = gen_tcp:listen(Port, [list, {packet, 4}, {active, false}]),
    dispatcher(LSock).
    %pStat(),
    %pBalance().

start_server(Server) ->
    net_kernel:connect_node(Server),
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
        

%pBalance() ->


pStat() ->
    Queue = statistics(run_queue),
    {_, Reductions} = statistics(reductions),
    %%TODO: Send statistics to all available nodes
    [{pBalance, Node} ! {pstat, {Queue, Reductions}} || Node <- nodes()],
    receive after 5000 -> ok end,
    pStat().


%pCommand() ->
