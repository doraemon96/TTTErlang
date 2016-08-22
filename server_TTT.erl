-module(server_TTT).
-compile(export_all).

start_server() ->
    {ok, LSock} = gen_tcp:listen(8000, [list, {packet, 4}, {active, false}]),

    %% Spawneamos y registramos el nombre de los procesos pBalance 
    Queue = statistics(run_queue),
    {_, Reductions} = statistics(reductions),
    PidStat = spawn(?MODULE, pStat, []),
    PidBalance = spawn (?MODULE, pBalance, [Queue, Reductions, node()]),
    NameBalance = "balance" ++ integer_to_list(erlang:length(nodes())),
    global:register_name(NameBalance, PidBalance),

    spawn(?MODULE, dispatcher,[LSock, PidBalance]),

    ok.

start_server(Server) ->
    net_kernel:connect_node(Server),
    start_server(),
    ok.

dispatcher(LSock, PidBalance) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn(?MODULE, pSocket, [Sock, PidBalance]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    Pid ! ok,
    dispatcher(LSock).

pSocket(Sock, PidBalance) ->
    receive ok -> ok end,
    ok = inet:setopts(Sock, [{active, true}]),
    pSocket_loop(Sock, PidBalance).

pSocket_loop(Sock, PidBalance) ->
    receive 
        {tcp, Sock, Data} ->
            PidBalance ! {pSocket, self()},
            receive
                {pBalance, Node} -> spawn(Node, ?MODULE, pCommand, [Data, nil, nil, self()]),
                _ -> {error, not_supported}
        {_} -> 
            io:format("Error en el mensaje")
    end,
    pSocket_loop(Sock).    

%% pBalance funciona de la siguiente manera: tiene como argumento un nodo que es el de menor carga.
%% Cuando recibe información de algún pStat compara si este nuevo nodo está menos cargado que el
%% que ya tiene. En caso afirmativo, este pasa a ser el nuevo argumento de la función pBalance; caso
%% contrario se vuelve a llamar con los mismos argumentos.
pBalance(Queue, Reductions, Node) ->
    receive
        %% Si recibimos un pedido de un psocket, le indicamos al nodo quien debe ejecutar el comando
        {pSocket, Pid} ->
            Pid ! {pBalance, Node};

        %% Si recibimos información de un pstat, comparamos con el mejor postor que tenemos
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
    %% Obtenemos datos de carga
    Queue = statistics(run_queue),
    {_, Reductions} = statistics(reductions),

    %% Enviamos datos de carga a todos los nodos
    ListBalances = lists:filter(fun(X) -> lists:prefix("balance", X) end, global:registered_names()), 
    lists:map(fun(X) -> global:send(global:whereis_name(X), {pStat, {Queue, Reductions}, node()}) end, ListBalances),

    %% Esperamos 5 segundos para volver a enviar
    receive after 5000 -> ok end,
    pStat().


pCommand(Command, PlayerId, GameId, PSocket) ->
    case string:tokens(Command," ") of
        ["CON", UserName] -> cmd_connect(UserName);
%        ["LSG", CmdId] ->
%        ["NEW", CmdId] ->
%        ["ACC", CmdId, GameId] ->
%        ["PLA", CmdId, GameId, Play] ->
%        ["OBS", CmdId, GameId] ->
%        ["LEA", CmdId, GameId] ->
%        ["BYE"] ->
        _ -> "ERROR not_implemented"
    end.
