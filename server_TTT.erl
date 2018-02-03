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

    PidDispatcher = spawn_link(?MODULE, dispatcher, [LSock, PidBalance, 0]),

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

%% Se pasa como argumento el nombre de algún nodo que ya esté trabajando
%% y el puerto a donde va a escuchar
start_server(Port, Server) ->
    net_kernel:connect_node(Server),
    receive after 1000 -> ok end,
    start_server(Port),
    ok.

%% dispatcher
%% Aceptamos la conexión, y partir de allí creamos un nuevo proceso psocket.
dispatcher(LSock, PidBalance, Number) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid        = spawn(?MODULE, pSocket, [Sock, PidBalance, Number]),
    ok         = gen_tcp:controlling_process(Sock, Pid),
    Pid ! ok, 
    dispatcher(LSock, PidBalance, Number + 1).

pSocket(Sock, PidBalance, Number) ->
    receive ok -> ok end,
    ok  = inet:setopts(Sock, [{active, true}]),
    LoopName = "loop" ++ integer_to_list(Number) ++ atom_to_list(node()),
    Pid = spawn(?MODULE, pSocket_loop, [Sock, PidBalance, nil, LoopName]),
    %Borrar esto -> io:format(LoopName),
    global:register_name(LoopName, Pid),
    ok.

pSocket_loop(Sock, PidBalance, UserName, LoopName) ->
    receive 
        {tcp, Sock, Data} ->
            PidBalance ! {pSocket, self()},
            receive
                {pBalance, Node} ->
                    spawn(Node, ?MODULE, pCommand, [Data, UserName, nil, {LoopName, node()}]);
                _ -> {error, not_supported}
            end;
        {pCommand, Msg}  ->
            case Msg of
                {valid_username, UName} -> ok = gen_tcp:send(Sock, "valid_username"),
                                           pSocket_loop(Sock, PidBalance, UName, LoopName);
                invalid_username        -> ok = gen_tcp:send(Sock, "invalid_username");
                {lsg, Gl, CmdId}        -> ok = gen_tcp:send(Sock, "lsg"),
                                           ok = gen_tcp:send(Sock, "cmdid"),
                                           ok = gen_tcp:send(Sock, CmdId),
                                           ok = lists:foreach(fun(X) -> ok = gen_tcp:send(Sock, X) end, Gl),
                                           ok = gen_tcp:send(Sock, "end");
                {new_ok, ID, CmdId}     -> io:format("NEW~n",[]),
                                           ok = gen_tcp:send(Sock, "new_ok"),
                                           ok = gen_tcp:send(Sock, erlang:integer_to_list(ID));
                {acc, AccMsg, CmdId}    -> ok = gen_tcp:send(Sock, "acc"),
                                           case AccMsg of
                                              not_exists -> ok = gen_tcp:send(Sock, "not_exists");
                                              acc        -> 
                                                ok = gen_tcp:send(Sock, "accepted"),
                                                receive 
                                                    {gameid, GameId} -> ok = gen_tcp:send(Sock, erlang:integer_to_list(GameId))
                                                end; 
                                              not_acc    -> ok = gen_tcp:send(Sock, "not_accepted")
                                           end;
                {pla, Result}           -> ok = gen_tcp:send(Sock, "pla"),
                                           io:format("PLA~n",[]),
                                           case Result of 
                                            success     -> 
                                                ok = gen_tcp:send(Sock, "success"),
                                                receive 
                                                    {table, Table} ->
                                                        ok = gen_tcp:send(Sock, Table)
                                                end,
                                                receive
                                                    {status, Status} ->
                                                        ok = gen_tcp:send(Sock, atom_to_list(Status))
                                                end;
                                            not_allowed -> ok = gen_tcp:send(Sock, "not_allowed");
                                            _           -> io:format("Error en el mensaje PLA ~n", [])
                                           end;
                {obs, Result}           -> ok = gen_tcp:send(Sock, "obs"),
                                           case Result of
                                            success -> 
                                                ok = gen_tcp:send(Sock, "success"),
                                                receive 
                                                    {table, Table} -> 
                                                        ok = gen_tcp:send(Sock, Table)
                                                end;
                                            not_exists -> 
                                                ok = gen_tcp:send(Sock, "not_exists")
                                           end;         
                {lea, Result}           -> ok = gen_tcp:send(Sock, "lea"),
                                           case Result of
                                            success -> ok = gen_tcp:send(Sock, "success");
                                            not_exists -> ok = gen_tcp:send(Sock, "not_exists")
                                           end;
                bye                     -> ok = gen_tcp:send(Sock, "bye");
                {update, UpMsg, GId}    -> case UpMsg of
                                               acc -> ok = gen_tcp:send(Sock, "updateacc" ++ erlang:integer_to_list(GId));
                                               pla -> ok = gen_tcp:send(Sock, "updatepla" ++ erlang:integer_to_list(GId)),
                                                      receive 
                                                         {table, Table} ->   
                                                            ok = gen_tcp:send(Sock, Table);
                                                         _ -> error
                                                      end,
                                                      receive
                                                         {status, Status} ->
                                                            ok = gen_tcp:send(Sock, atom_to_list(Status));
                                                         _ -> error
                                                      end;
                                               obs -> ok = gen_tcp:send(Sock, "updateobs" ++ erlang:integer_to_list(GId)),
                                                      receive 
                                                         {table, Table} ->   
                                                            ok = gen_tcp:send(Sock, Table);
                                                         _ -> error
                                                      end;
                                                _   -> error_not_implemented
                                           end;
                Default                 -> io:format("Error en mensaje de pCommand ~p ~n", [Default]),
                                           ok = gen_tcp:send(Sock, Default)
            end;
        {tcp_closed, Sock} ->
            %%TODO: Chequear que Sock sea lo correcto a pasar aca.
            delete_by_username(Sock, UserName),
            delete_username(UserName),
            io:format("El usuario se ha desconectado~n");
        Default           -> 
            io:format("Error en el mensaje ~p~n", [Default]),
            ok = gen_tcp:send(Sock, "wrong_command")
    end,
    pSocket_loop(Sock, PidBalance, UserName, LoopName). 

   
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
                true  -> 
                    pBalance(New_Queue, New_Reductions, New_Node);
                false ->
                    case Queue < New_Queue of 
                        true  -> 
                            pBalance(Queue, Reductions, Node);
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
    io:format("~p~n",[string:tokens(Command," ")]),
    case string:tokens(Command," ") of
        ["CON", UserName]         -> io:format("COOON!~n"), cmd_con(PSocket, UserName);
        ["LSG", CmdId]            -> cmd_lsg(PSocket, CmdId);
        ["NEW", CmdId]            -> cmd_new(PSocket, PlayerId, CmdId);
        ["ACC", GId, CmdId]       -> cmd_acc(PSocket, erlang:list_to_integer(GId), PlayerId, CmdId);
        ["PLA", GId, Play, CmdId] -> cmd_pla(PSocket, erlang:list_to_integer(GId), Play, PlayerId, CmdId);
        ["OBS", GId, CmdId]       -> cmd_obs(PSocket, erlang:list_to_integer(GId));
        ["LEA", GId, CmdId]       -> cmd_lea(PSocket, erlang:list_to_integer(GId));
        ["BYE", CmdId]            -> cmd_bye(PSocket, PlayerId); 
        _ -> PSocket ! "command_not_implemented"
    end.
