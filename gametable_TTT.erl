-compile(export_all).

%% create_game
%%
%% TODO
create_game(GameId, UName) ->
    F = fun() ->
            mnesia:write(#game{gameid=GameId,
                               user1=UName})
        end,
    mnesia:activity(transaction, F).

%% list_games
%%
%% TODO
list_games() ->
    F = fun() -> 
          Handle = qlc:q([P || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    mnesia:activity(transaction, F).


%%
%%
%%
delete_game(GameId) ->
    F = fun() ->
            mnesia:delete({game, GameId})
        end,
    mnesia:activity(transaction, F).

%%
%%
%%
delete_by_username(UName) ->
    F = fun() -> 
          Handle = qlc:q([P#game.gameid || P <- mnesia:table(game), (P#game.user1 == UName) or (P#game.user2 == UName)]),
          qlc:e(Handle)
        end,
    L = mnesia:activity(transaction, F),
    lists:foreach(fun(X) -> delete_game(X) end, L).

%% get_game_table devuelve el tablero de un juego.
get_game_table(Game) ->
    Game#game.table.


%% get_game_players devuelve una tupla de jugadores.
get_game_players(Game) ->
    {Game#game.user1, Game#game.user2}.


%% set_game_table cambia el tablero segun una jugada,
%%  devuelve true si la jugada fue posible, y false si no.
set_game_table(Game, Table) ->
    case is_table_possible(get_game_table(Game), Table) of
        false -> false;
        true  -> Game#game{table = Table},
                 true
    end.

is_table_possible(TableIn,TableOut) ->
    case table_checkequal(TableIn, TableOut) of
        true -> false;
        _ -> case table_checksuperpos(TableIn, TableOut) of
                true -> false;
                _ -> true
            end
    end.

table_checkequal(TableIn, TableOut) ->
    if TableIn == TableOut -> true;
       true -> false
    end.
table_checksuperpos(TableIn, TableOut) ->
    Zip = lists:zip(lists:flatten(TableIn), lists:flatten(TableOut)),
    Fea = lists:foreach(fun(X) -> if 
                                    (element(1,X) == 0) or (element(2,X) == 0) -> true;
                                    true -> element (1,X) == element(2,X) end
                        end,
                        Zip),
    lists:all(fun(X) -> X end, Fea).


%% get_game_status muestra si el juego fue ganado o sigue en progreso
get_game_status(Game) ->
    Table = get_game_table(Game),
    table_checkrow(Table) or table_checkcol(Table) or table_checkdiagonal(Table).

table_checkrow(Table) ->
    Fea = lists:foreach(fun(X) -> (lists:nth(1,X) == lists:nth(2,X)) and (lists:nth(2,X) == lists:nth(3,X)) end, Table),
    lists:any(fun(X) -> X end, Fea).
table_checkcol(Table) ->
    Zip = lists:zip(lists:nth(1,Table), lists:nth(2,Table), lists:nth(3,Table)),
    Fea = lists:foreach(fun(X) -> (element(1,X) == element(2,X)) and (element(2,X) == element(3,X)) end),
    lists:any(fun(X) -> X end, Fea).
table_checkdiagonal(Table) ->
    Diag1 = (element(1, lists:nth(1,Table)) == element(2, lists:nth(2,Table))) and (element(2, lists:nth(2,Table)) == element(3, lists:nth(3,Table))),
    Diag2 = (element(3, lists:nth(1,Table)) == element(2, lists:nth(2,Table))) and (element(2, lists:nth(2,Table)) == element(1, lists:nth(3,Table))),
    Diag1 or Diag2.
