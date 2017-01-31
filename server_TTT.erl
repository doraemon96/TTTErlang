-module(server_TTT).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("commands_TTT.erl").


%% *************************************************************** %%
%% ******************** Funciones del server ********************* %%
%% *************************************************************** %%

%% start_server
%% Se inician todos los procesos inherentes al server 
start_server(Port) ->
    Number = erlang:length(nodes()),

    {ok, LSock} = gen_tcp:listen(Port, [list, {packet, 4}, {active, true}, {reuseaddr, true}]),

    %% Spawneamos y registramos el nombre de los procesos pBalance 
    Queue           = statistics(run_queue),
    {_, Reductions} = statistics(reductions),
    PidStat         = spawn_link(?MODULE, pStat, []),
    PidBalance      = spawn_link(?MODULE, pBalance, [Queue, Reductions, node()]),
    NameBalance     = "balance" ++ integer_to_list(Number),
    global:register_name(NameBalance, PidBalance),

    PidDispatcher = spawn_link(?MODULE, dispatcher, [LSock, PidBalance]),

    process_flag(trap_exit, true),

    receive
        {'EXIT', _, Reason} -> 
            exit(PidStat, Reason),
            exit(PidBalance, Reason),
            exit(PidDispatcher, Reason),
            gen_tcp:close(LSock)
    end, 

    receive after 30000 -> ok end,

    spawn(?MODULE, start_server, [Port]),

    ok.

%% dispatcher
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
    pSocket_loop(Sock, PidBalance, nil).

pSocket_loop(Sock, PidBalance, UserName) ->
    io:format("User: ~p~n", [UserName]),
    receive 
        {tcp, Sock, Data} ->
            PidBalance ! {pSocket, self()},
            receive
                {pBalance, Node} ->
                    spawn(Node, ?MODULE, pCommand, [Data, UserName, nil, self()]);
                _ -> {error, not_supported}
            end;
        {pCommand, Msg} ->
            case Msg of
                {valid_username, UName} -> ok = gen_tcp:send(Sock, "valid_username"),
                                           pSocket_loop(Sock, PidBalance, UName);
                invalid_username -> ok = gen_tcp:send(Sock, "invalid_username");
                {lsg, Gl}        -> ok = gen_tcp:send(Sock, "lsg"),
                                    ok = lists:foreach(fun(X) -> ok = gen_tcp:send(Sock, X) end, Gl),
                                    ok = gen_tcp:send(Sock, "end");
                Default -> io:format("Error en mensaje de pCommand ~n", []),
                           ok = gen_tcp:send(Sock, Default);
            end;
        {tcp_closed, Sock} ->
                io:format("El usuario se ha desconectado~n");
        _ -> io:format("Error en el mensaje~n")
    end,
    pSocket_loop(Sock, PidBalance, UserName). 

   
%% pBalance
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

%% pStat
%% Obtiene datos de carga de los nodos y los envia.
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

%% pCommand
%% Llama a las funciones pertinentes a los comandos
pCommand(Command, PlayerId, GameId, PSocket) ->
    %io:format("Me crearon en el nodo ~p ~n", [node()]),
    io:format("~p~n",[string:tokens(Command," ")]),
    case string:tokens(Command," ") of
        ["CON", UserName] -> cmd_con(UserName, PSocket);
        ["LSG"]           -> cmd_lsg(PSocket, 0);
        ["NEW"]           -> cmd_new(PSocket);
%        ["ACC", CmdId, GameId] ->
%        ["PLA", CmdId, GameId, Play] ->
%        ["OBS", CmdId, GameId] ->
%        ["LEA", CmdId, GameId] ->
%        ["BYE"] ->
        _ -> PSocket ! "command_not_implemented"
    end.
