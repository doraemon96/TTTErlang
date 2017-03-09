-compile(export_all).

-record(game, {gameid, 
               user1, 
               sock1,
               user2 = "*waiting*", 
               sock2,
               table = [[0,0,0],[0,0,0],[0,0,0]], 
               observers}).
-record(user, {name, empty}).

%% *************************************************************** %%
%% *****   Funciones para iniciar la base de datos mnesia    ***** %%
%% *************************************************************** %%

%% mnesia init
%%
%% Inicia el esquema de la base de datos y las tablas a usar
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
%% ***** Funciones para agregar o quitar de la base de datos ***** %%
%% *************************************************************** %%

%% add_username
%% Agrega un usuario a la base de datos si es posible
add_username(UName) ->
    F = fun() ->
            mnesia:write(#user{name=UName})
        end,
    mnesia:activity(transaction, F).

%% exists_username
%% True si el usuario existe, caso contrario, False
exists_username(UName) ->
    F = fun() ->
            case mnesia:read({user, UName}) of 
                [] -> false;
                _  -> true
            end
        end,
    mnesia:activity(transaction, F). 

%% delete_username
%% Elimina un usuario de la base de datos
delete_username(UName) ->
    F = fun() ->
            mnesia:delete({user, UName})
        end,
    mnesia:activity(transaction, F).
