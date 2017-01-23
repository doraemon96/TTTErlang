-module(structures_TTT).
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


%% TODO: get_game_status muestra si el juego fue ganado o sigue en progreso
get_game_status(Game) ->
    ok.

