-module(gametable_TTT).
-compile(export_all).

-record(game, {table = [[0,0,0],[0,0,0],[0,0,0]], player1, player2}).


%% create_game toma (Jugador, nil) si espera un jugador mas para empezar la partida,
%%  cuando este se conecte, se llamara a create_game con ambos jugadores como parametros.
create_game(nil, nil) -> 
    {error, not_supported};
create_game(Player1, Player2) ->
    case Player2 of
        nil ->
            receive
                {new_player, Player2} -> create_game(Player1, Player2);
                _ -> {error, not_supported}
            end;
        _ -> Game = #game{player1 = Player1, player2 = Player2}
    end,
    ok.


%% get_game_table devuelve el tablero de un juego.
get_game_table(Game) ->
    Game#game.table.


%% get_game_players devuelve una tupla de jugadores.
get_game_players(Game) ->
    {Game#game.player1, Game#game.player2}.


%% set_game_table cambia el tablero segun una jugada,
%%  devuelve true si la jugada fue posible, y false si no.
set_game_table(Game, Table) ->
    case is_table_possible(get_game_table(Game), Table) of
        true  -> Game#game{table = Table},
                 true;
        false -> false
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
    Diag1 = (element(1, lists:nth(1,Table)) == element(2, lists:nth(2,Table))) and (element(2, lists:nth(2,Table)) == element(3, lists(nth(3,Table)))),
    Diag2 = (element(3, lists:nth(1,Table)) == element(2, lists:nth(2,Table))) and (element(2, lists:nth(2,Table)) == element(1, lists(nth(3,Table)))),
    Diag1 or Diag2.
