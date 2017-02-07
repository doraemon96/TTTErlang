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


%% delete_game
%%
%% TODO
delete_game(GameId) ->
    F = fun() ->
            mnesia:delete({game, GameId})
        end,
    mnesia:activity(transaction, F).

%% delete_by_username
%%
%% TODO
delete_by_username(UName) ->
    F = fun() -> 
          Handle = qlc:q([P#game.gameid || P <- mnesia:table(game), (P#game.user1 == UName) or (P#game.user2 == UName)]),
          qlc:e(Handle)
        end,
    L = mnesia:activity(transaction, F),
    lists:foreach(fun(X) -> delete_game(X) end, L).

%% get_game_table
%% Devuelve el tablero de un juego.
get_game_table(GameId) ->
    F = fun() ->
            case mnesia:read({game, GameId}) of
                [T] -> erlang:element(5, T);
                _   -> []
            end
        end,
    mnesia:activity(transaction, F).


%% get_game_players
%% Devuelve una tupla de jugadores.
get_game_players(GameId) ->
    F = fun() ->
            case mnesia:read({game, GameId}) of
                [T] -> U1 = erlang:element(3, T),
                       U2 = erlang:element(4, T),
                       {U1, U2};
                _   -> []
            end
        end,
    mnesia:activity(transaction, F).


%% set_game_table 
%% Cambia el tablero segun una jugada y devuelve true si la jugada fue
%%  posible, y false si no.
set_game_table(GameId, Table, UserName) ->
    OldTable   = get_game_table(GameId),
    IsTurn     = is_turn(GameId, OldTable, UserName),
    if IsTurn -> 
        case is_table_impossible(OldTable, Table) of
            true  -> false;
            false -> F = fun() ->
                            [R] = mnesia:wread({game, GameId}), 
                            mnesia:write(R#game{table = Table})
                         end,
                     mnesia:activity(transaction, F),
                     true
        end;
        true -> false
    end.

is_turn(GameId, OldTable, UserName) ->
    {U1, U2} = get_game_players(GameId),
    case (lists:foldl(fun(X, Sum) -> if X == 0 -> 1 + Sum; true -> Sum end end, 0, lists:flatten(OldTable))) rem 2 of
        0 -> UserName == U2;
        _ -> UserName == U1
    end.

is_table_impossible(TableIn, TableOut) ->
    table_checkmultimove(TableIn, TableOut) or table_checksuperpos(TableIn, TableOut).

table_checkmultimove(TableIn, TableOut) ->
    Zip = lists:zip(lists:flatten(TableIn), lists:flatten(TableOut)),
    F   = fun(X) -> if
                      (element(1,X) /= element(2,X)) -> 1;
                      true                           -> 0
                    end
          end,
    (lists:sum(lists:map(F, Zip))) /= 1.

table_checksuperpos(TableIn, TableOut) ->
    Zip = lists:zip(lists:flatten(TableIn), lists:flatten(TableOut)),
    Fea = lists:map(fun(X) -> if 
                                (element(1,X) /= 0) and (element(2,X) /= element(1,X)) -> false;
                                true                                                   -> true
                              end
                    end,
                    Zip),
    not lists:all(fun(X) -> X end, Fea).


%% get_game_status
%% Muestra si el juego fue ganado o sigue en progreso
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

%% NICO NO PUEDE ESCRIBIR NINGUN COMENTARIO ACA
%% YOU HAVE NO POWER HERE!
%%
make_play(UserName, GameId, Play) ->
    OldTable = get_game_table(GameId),
    [H | T] = Play,
    case H of
        97  -> X2 = erlang:list_to_integer(T),
               B  = (X2 < 4) and (X2 > 0),
                   if B -> L2 = lists:nth(2, OldTable),
                           L3 = lists:nth(3, OldTable),
                           L1 = set_nth_list(lists:nth(1, OldTable), X2, 1),
                           [L1, L2, L3];
                      true -> error1
                   end;
        98  -> X2 = erlang:list_to_integer(T),
               B  = (X2 < 4) and (X2 > 0),
                   if B -> L1 = lists:nth(1, OldTable),
                           L3 = lists:nth(3, OldTable),
                           L2 = set_nth_list(lists:nth(2, OldTable), X2, 1),
                           [L1, L2, L3];
                      true -> error2
                   end;
        99  -> X2 = erlang:list_to_integer(T),
               B  = (X2 < 4) and (X2 > 0),
                   if B -> L1 = lists:nth(1, OldTable),
                           L2 = lists:nth(2, OldTable),
                           L3 = set_nth_list(lists:nth(3, OldTable), X2, 1),
                           [L1, L2, L3];
                      true -> error3
                   end;
        _ -> error
    end.
        %"b" ++ X -> list_to_integer(X)
        %"c" ++ X -> list_to_integer(X)

set_nth_list([H | T], Number, Element) ->
    case Number of
        1 -> [Element] ++ T;
        _ -> [H] ++ set_nth_list(T, Number - 1, Element)
    end.
        
    
