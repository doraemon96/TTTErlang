-module(server_TTT).
-compile(export_all).
%-include("commands_TTT.erl").

-record(user, {name,
               empty}).

%% Si vamos a hacer un spawn por cada partida ¿Por qué no usar el pid ya que este es único?
-record(game, {user1,
               user2,
               gameid,
               status}). 

%% *************************************************************** %%
%% ***** Funciones para agregar o quitar de la base de datos ***** %%
%% *************************************************************** %%
%% Registro de username
add_username(UName) ->
    F = fun() ->
            mnesia:write(#user{name=UName})
        end,
    mnesia:activity(transaction, F).

exists_username(UName) ->
    F = fun() ->
            case mnesia:read({user, UName}) of 
                [] -> false;
                _  -> true
            end
        end,
    mnesia:activity(transaction, F). 

delete_username(UName) ->
    F = fun() ->
            mnesia:delete({user, UName})
        end,
    mnesia:activity(transaction, F).

%% Listar todos los juegos
list_games() ->
    F = fun() ->
            CatchAll = [{'_',[],['$_']}],
            mnesia:select(user, CatchAll)
        end,
    mnesia:activity(transaction, F).

%% Manipular juegos
%create_game()

init(Port) -> 
    mnesia:create_schema([node()]),
    mnesia:start(),   
    mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_copies, [node()]}]),
    mnesia:create_table(game, [{attributes, record_info(fields, game)}, {disc_copies, [node()]}]),
    spawn(?MODULE, start_server, [Port]),
    ok.

init(Port, Node) ->
    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:add_table_copy(user, node(), disc_copies),
    mnesia:add_table_copy(game, node(), disc_copies),
    spawn(?MODULE, start_server, [Port]),
    ok.

%% *************************************************************** %%
%% ******************** Funciones del server ********************* %%
%% *************************************************************** %%

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
                {lsg, Gl}        -> ok = gen_tcp:send(Sock, "lsg"),
                                    ok = lists:foreach(fun(X) -> ok = gen_tcp:send(Sock, term_to_binary(X)) end, Gl),
                                    ok = gen_tcp:send(Sock, "end");
                _ -> error
            end;
        {tcp_closed, Socket} ->
                io:format("El usuario se ha desconectado~n");
        _ -> io:format("Error en el mensaje~n")
    end,
    pSocket_loop(Sock, PidBalance). 

%%send_list_tcp(Sock, [H | T]) ->
%%    case T of
%%    [] -> ok= 
%%    ok = gen_tcp:send(Sock, term_to_binary(H)),
%%    send_list_tcp(T).
   

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
    %io:format("Me crearon en el nodo ~p ~n", [node()]),
    io:format("~p~n",[string:tokens(Command," ")]),
    case string:tokens(Command," ") of
        ["CON", UserName] -> cmd_connect(UserName, PSocket);
        ["LSG"]           -> cmd_lsg(PSocket, 0);
%        ["NEW", CmdId] ->
%        ["ACC", CmdId, GameId] ->
%        ["PLA", CmdId, GameId, Play] ->
%        ["OBS", CmdId, GameId] ->
%        ["LEA", CmdId, GameId] ->
%        ["BYE"] ->
        _ -> io:format("ERROR not_implemented~n"),
             PSocket ! ok
    end.

%% *************************************************************** %%
%% ******************** Comandos del cliente ********************* %%
%% *************************************************************** %%
cmd_connect(UserName, PSocket) ->
    case exists_username(UserName) of
        true  -> PSocket ! {pCommand, invalid_username}; 
        false ->
            add_username(UserName), 
            PSocket ! {pCommand, valid_username}
    end,
    ok. 
cmd_lsg(PSocket, CmdId) ->
    GamesList  = list_games(),
    GamesList2 = lists:foreach(term_to_binary, GamesList),
    PSocket ! {lsg, GamesList2},
    ok.
    
