-compile(export_all).
-include("database_TTT.erl").
-include("gametable_TTT.erl").

%% CON
%% Hace un llamado a la base de datos para saber si ya existe
%% un usuario con su mismo nombre, en caso contrario lo registra.
cmd_con(PSocket, UserName) ->
    case exists_username(UserName) of
        true  -> 
            PSocket ! {pCommand, invalid_username}; 
        false ->
            add_username(UserName), 
            PSocket ! {pCommand, {valid_username, UserName}}
    end,
    ok.


%% LSG
%% Devuelve todas las partidas listadas en la base de datos
%% de la forma "GameId Usuario1 Usuario2"
cmd_lsg(PSocket, CmdId) ->
    GamesList  = list_games(),
    GamesList2 = lists:map(fun({_ , X, Y, _, Z, _, _, _}) -> erlang:integer_to_list(X) ++ " " 
                                                    ++ Y ++ " " 
                                                    ++ Z end, GamesList),
    PSocket ! {pCommand, {lsg, GamesList2, CmdId}},
    ok.

%% NEW
%% Dado un usuario, crea una partida a su nombre con un id
%% unico. Deja al jugador2 vacio, en espera a que se una.
cmd_new(PSocket, PlayerId, CmdId) ->
    F = fun() -> 
          Handle = qlc:q([P#game.gameid || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    Ids = mnesia:activity(transaction, F),
    case Ids of
        [] -> 
            create_game(1, PlayerId, PSocket),
            PSocket ! {pCommand, {new_ok, 1, CmdId}};
        _  ->
            create_game(ID = lists:max(Ids) + 1, PlayerId, PSocket),
            PSocket ! {pCommand, {new_ok, ID, CmdId}}
    end,
    ok.
    

%% ACC
%% Dados un juego y un usuario, intenta unirse en la posicion
%% del jugador2 si esta esta vacia.
cmd_acc(PSocket, GameId, UserName, CmdId) ->
    F = fun() -> R = mnesia:read({game, GameId}),
                 case R of
                     []   -> 
                        PSocket ! {pCommand, {acc, not_exists, CmdId}};
                     [G]  -> 
                        case erlang:element(5, G) of
                            "*waiting*" -> PSocket ! {pCommand, {acc, acc, CmdId}},
                                           PSocket ! {gameid, GameId},
                                           mnesia:write(G#game{user2=UserName,
                                                               sock2=PSocket}),
                                           io:format("~p~n",[erlang:element(4,G)]),
                                           erlang:element(4, G) ! {pCommand, {update, acc, erlang:element(2, G)}};
                            _           -> PSocket ! {pCommand, {acc, not_acc, CmdId}}
                        end
                 end
        end,
    mnesia:activity(transaction, F), 
    ok.

%% PLA
%% Dados un juego y una jugada, actualiza el tablero de juego
%% si dicha jugada es posible.
cmd_pla(PSocket, GameId, Play, UserName, CmdId) ->
    MakePlay = make_play(UserName, GameId, Play),
    F = fun() -> R = mnesia:read({game, GameId}),
                 case R of
                    [] -> 
                        PSocket ! {pCommand, {pla, not_allowed}};
                    [G] ->
                        SetGameTable = set_game_table(self(), GameId, MakePlay, UserName),
                        {U1, U2} = get_game_players(GameId), 
                        P = if 
                                UserName == U1 -> 6;
                                true -> 4
                            end,
                        if 
                            SetGameTable -> PSocket ! {pCommand, {pla, success}},
                                            receive
                                                {table, Table} -> 
                                                    PSocket ! {table, Table},
                                                    erlang:element(P, G) ! {pCommand, {update, pla, erlang:element(2, G)}},
                                                    erlang:element(P, G) ! {table, Table},
                                                    Observers = erlang:element(8, G),
                                                    lists:foreach(fun(X) -> X ! {pCommand, {update, obs, erlang:element(2, G)}},
                                                                            X ! {table, Table} end, Observers)
                                            end;
                            true         -> PSocket ! {pCommand, {pla, not_allowed}}
                        end
                 end
        end,
    case MakePlay of
        error     -> PSocket ! {pCommand, {pla, not_allowed}};
        game_over -> 
            PSocket ! {pCommand, {pla, game_over}},
            %% TODO: aca no hay que mandar una variable como es GameResult,
            %% sino un atom. Además hay que agregar en el server la opción
            %% del mensajito que mandemos
            PSocket ! {GameResult};
        _         -> mnesia:activity(transaction, F)
    end,
    ok.  

%% OBS
%% Dado un juego, agrega dicho socket a la lista de observadores
%% de un juego, los cuales reciben actualizaciones de estado del
%% mismo pero no pueden realizar jugadas.
cmd_obs(PSocket, GameId) ->
    F = fun() -> R = mnesia:read({game, GameId}),
                 case R of
                     []   -> 
                        PSocket ! {pCommand, {obs, not_exists}};
                     [G]  -> 
                        Observers = [PSocket | erlang:element(8, G)],
                        mnesia:write(G#game{observers=Observers}),
                        PSocket ! {pCommand, {obs, success}},
                        PSocket ! {table, erlang:element(7, G)}
                 end
        end,
    mnesia:activity(transaction, F),
    ok.

%% LEA
%%
%% TODO
cmd_lea() ->
    ok.

%% BYE
%% Elimina al usuario de la base de datos y
%% corta la ejecucion del programa.
cmd_bye(PSocket, UserName) ->
    delete_by_username(PSocket, UserName),
    PSocket ! {pCommand, bye},
    ok.

%% HELP
%% Muestra la ayuda
cmd_help(PSocket) ->
    PSocket ! {pCommand, help},
    ok.
