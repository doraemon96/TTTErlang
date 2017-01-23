-module(server_TTT).
-compile(export_all).

%% Se inician todos los procesos inherentes al server 
start_server() ->
    %%FIXME: Si hay dos servidores y se cae el primero, un nuevo servidor se conectaria en el mismo numero que el ultimo.
    %%       Ejemplo: sv1 escucha en 8000, sv2 escucha en 8001, sv1 se cae, sv3 escucha en 8001 (!!!) 
    Number = erlang:length(nodes()),

    {ok, LSock} = gen_tcp:listen(8000 + Number, [list, {packet, 4}, {active, false}, {reuseaddr, true}]),

    %% Spawneamos y registramos el nombre de los procesos pBalance 
    Queue           = statistics(run_queue),
    {_, Reductions} = statistics(reductions),
    PidStat         = spawn(?MODULE, pStat, []),
    PidBalance      = spawn (?MODULE, pBalance, [Queue, Reductions, node()]),
    NameBalance     = "balance" ++ integer_to_list(Number),
    global:register_name(NameBalance, PidBalance),

    spawn(?MODULE, dispatcher,[LSock, PidBalance]),

    ok.

%% Se pasa como argumento el nombre de algún nodo que ya esté trabajando
start_server(Server) ->
    net_kernel:connect_node(Server),
    receive after 1000 -> ok end,
    start_server(),
    ok.

%% Aceptamos la conexión, y partir de allí creamos un nuevo proceso psocket.
dispatcher(LSock, PidBalance) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid        = spawn(?MODULE, pSocket, [Sock, PidBalance]),
    ok         = gen_tcp:controlling_process(Sock, Pid),
    Pid ! ok, 
    dispatcher(LSock, PidBalance).

pSocket(Sock, PidBalance) ->
    receive ok -> ok end,
    ok = inet:setopts(Sock, [{active, true}]),
    pSocket_loop(Sock, PidBalance).

pSocket_loop(Sock, PidBalance) ->
    receive 
        {tcp, Sock, Data} ->
            PidBalance ! {pSocket, self()},
            receive
                {pBalance, Node} ->
                    spawn(Node, ?MODULE, pCommand, [Data, nil, nil, self()]);
                _ -> {error, not_supported}
            end;
        {pCommand, Msg} ->
            case Msg of
                valid_username   -> ok = gen_tcp:send(Sock, "valid_username");
                invalid_username -> ok = gen_tcp:send(Sock, "invalid_username");
                _ -> error
            end;
        {tcp_closed, Socket} ->
                io:format("El usuario se ha desconectado~n");
        _ -> io:format("Error en el mensaje~n")
    end,
    pSocket_loop(Sock, PidBalance).    

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
                true  -> pBalance(New_Queue, New_Reductions, New_Node);
                false ->
                    case Queue < New_Queue of 
                        true  -> pBalance(Queue, Reductions, Node);
                        false ->
                            case Reductions > New_Reductions of
                                true  -> pBalance(New_Queue, New_Reductions, New_Node);
                                false -> pBalance(Queue, Reductions, Node)
                            end
                    end
            end;

        _ -> {error, not_supported}
    end,
    pBalance(Queue, Reductions, Node).

pStat() ->
    %% Obtenemos datos de carga
    Queue           = statistics(run_queue),
    {_, Reductions} = statistics(reductions),

    %% Enviamos datos de carga a todos los nodos
    ListBalances = lists:filter(fun(X) -> lists:prefix("balance", X) end, global:registered_names()), 
    lists:map(fun(X) -> global:send(X, {pStat, {Queue, Reductions}, node()}) end, ListBalances),

    %% Esperamos 5 segundos para volver a enviar
    receive after 5000 -> ok end,
    pStat().


pCommand(Command, PlayerId, GameId, PSocket) ->
    io:format("Me crearon en el nodo ~p ~n", [node()]),
    io:format("~p~n",[string:tokens(Command," ")]),
    case string:tokens(Command," ") of
        ["CON", UserName] -> cmd_connect(UserName, PSocket);
%        ["LSG", CmdId] ->
%        ["NEW", CmdId] ->
%        ["ACC", CmdId, GameId] ->
%        ["PLA", CmdId, GameId, Play] ->
%        ["OBS", CmdId, GameId] ->
%        ["LEA", CmdId, GameId] ->
%        ["BYE"] ->
        _ -> io:format("ERROR not_implemented~n"),
             PSocket ! ok
    end.

%% CAMBIAR ESTO 
cmd_connect(Username, PSocket) ->
    Check = [], 
    case Check of
        [] -> PSocket ! {pCommand, valid_username};  
        _  -> PSocket ! {pCommand, invalid_username}
    end,
    ok.
