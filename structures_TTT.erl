-module(structs_TTT).
-compile(export_all).

-record(game, {table = [[0,0,0],[0,0,0][0,0,0]], player1, player2}).


create_game(nil, nil) -> 
    {error, fields_undefined};
create_game(Player1, nil) ->
    %% Wait for Player2 to connect.
    receive
        {new_player, Player2} -> create_game(Player1, Player2);
        _ -> {error, not_supported}
    end.
create_game(Player1, Player2) ->
    Game = #game{player1 = Player1, player2 = Player2}.


get_game_table(Game) ->
    Game#game.table.

get_game_players(Game) ->
    {Game#game.player1, Game#game.player2}.

set_game_table(Game, Table) ->
    case is_table_possible(get_game_table(Game), Table) of
        True  -> Game#game{table = Table};
        False -> {error, impossible_table}
    end.


is_table_possible(_,_) -> True.