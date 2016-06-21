-module{server_TTT}.
-compile{export_all}.

start_server() ->
    

start_server(Server) ->
    net_kernel:connect_node(Server),
    start_server(),
    ok.



dispatcher() ->
    {ok, LSock} = gen_tcp:listen(8000, [list, {packet, 0}, {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(?MODULE, psocket, [Sock]),
    dispatcher().

psocket() ->

pBalance() ->


pStat() ->
    Queue = statistics(run_queue),
    {_, Reductions} = statistics(reductions),
    %%TODO: Send statistics to all available nodes
    [{pBalance, Node} ! {pstat, {Queue, Reductions}} || Node <- nodes()],
    receive after 5000 -> ok end,
    pStat().

pCommand() ->
