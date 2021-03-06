-compile(export_all).

%% create_game
%% Crea un nuevo juego en la tabla de mnesia con un gameId unico
%% y deja vacia la posicion del segundo jugador.
create_game(GameId, UName, PSocket) ->
    F = fun() ->
            mnesia:write(#game{gameid=GameId,
                               user1=UName,
                               sock1=PSocket})
        end,
    mnesia:activity(transaction, F).

%% list_games
%% Hace una query en mnesia por todas las tablas de juego.
%% TODO: Usar mnesia: y no qlc:
list_games() ->
    F = fun() -> 
          Handle = qlc:q([P || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    mnesia:activity(transaction, F).


%% delete_game
%% Elimina todos los registros en mnesia con el gameId dado.
delete_game(GameId) ->
    F = fun() ->
            mnesia:delete({game, GameId})
        end,
    mnesia:activity(transaction, F).


%% delete_by_username
%% Elimina los juegos en los que participa el usuario y lo quita
%% de la lista de observadores de todos los juegos.
%% TODO?: enviar un aviso de que el usuario abandono una partida
delete_by_username(PSocket, UName) ->
    %% Lo elimina de todas las partidas en las que participa
    %% TODO: Usar mnesia: y no qlc:
    F = fun() -> 
            Handle = qlc:q([P#game.gameid || P <- mnesia:table(game), (P#game.user1 == UName) or (P#game.user2 == UName)]),
            qlc:e(Handle)
        end,
    L = mnesia:activity(transaction, F),
    lists:foreach(fun(X) -> delete_game(X) end, L),
    %% Lo elimina de todas las listas de observadores
    %% TODO?: No usar match_object
    G = fun() -> 
            R = mnesia:match_object({game, '_', '_', '_', '_', '_', '_', '_'}),
            lists:foreach(fun(X) -> mnesia:write(X#game{observers = lists:delete(PSocket, element(8, X))}) end, R)
        end,
    M = mnesia:activity(transaction, G),
    ok.

%% get_game_table
%% Devuelve el tablero de un juego.
get_game_table(GameId) ->
    F = fun() ->
            case mnesia:read({game, GameId}) of
                [T] -> 
                    erlang:element(7, T);
                _   -> 
                    []
            end
        end,
    mnesia:activity(transaction, F).


%% get_game_players
%% Devuelve una tupla de jugadores.
get_game_players(GameId) ->
    F = fun() ->
            case mnesia:read({game, GameId}) of
                [T] -> 
                    U1 = erlang:element(3, T),
                    U2 = erlang:element(5, T),
                    {U1, U2};
                _   -> 
                    []
            end
        end,
    mnesia:activity(transaction, F).


%% set_game_table 
%% Cambia el tablero segun una jugada y devuelve true si la jugada fue
%% posible, y false si no.
set_game_table(PCmd, GameId, Table, UserName) ->
    OldTable   = get_game_table(GameId),
    IsTurn     = is_turn(GameId, OldTable, UserName),
    if IsTurn -> 
        case is_table_impossible(OldTable, Table) of
            true  -> false;
            false -> F = fun() ->
                            [R] = mnesia:wread({game, GameId}), 
                            mnesia:write(R#game{table = Table}),
                            PCmd ! {table, Table}
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


%% is_game_won
%% Muestra si el juego fue ganado
is_game_won(Table) ->
    table_checkrow(Table) or table_checkcol(Table) or table_checkdiagonal(Table).

table_checkrow(Table) ->
    Fea = lists:map(fun(X) -> ((lists:nth(1,X)) == lists:nth(2,X)) 
                          and ((lists:nth(2,X)) == lists:nth(3,X)) 
                          and (lists:nth(1,X) /= 0) %evita falsos positivos cuando el juego recien comienza
                    end, Table), 
    lists:any(fun(X) -> X end, Fea).

table_checkcol(Table) ->
    Zip = lists:zip3(lists:nth(1,Table), lists:nth(2,Table), lists:nth(3,Table)),
    Fea = lists:map(fun(X) -> (element(1,X) == element(2,X)) 
                          and (element(2,X) == element(3,X))  
                          and (element(1,X) /= 0)
                    end, Zip),
    lists:any(fun(X) -> X end, Fea).

table_checkdiagonal(Table) ->
    Diag1 = (lists:nth(1, lists:nth(1,Table)) == lists:nth(2, lists:nth(2,Table))) 
        and (lists:nth(2, lists:nth(2,Table)) == lists:nth(3, lists:nth(3,Table)))
        and (lists:nth(1, lists:nth(1,Table)) /= 0),
    Diag2 = (lists:nth(3, lists:nth(1,Table)) == lists:nth(2, lists:nth(2,Table))) 
        and (lists:nth(2, lists:nth(2,Table)) == lists:nth(1, lists:nth(3,Table)))
        and (lists:nth(3, lists:nth(1,Table)) /= 0),
    Diag1 or Diag2.

is_game_tie(Table) ->
    F = fun(X) ->
        case X of
            0 -> true;
            _ -> false
        end
    end,
    not lists:any(fun(X) -> X end, (lists:map(F, lists:flatten(Table)))).

is_game_over(Table) ->
    is_game_tie(Table) or is_game_won(Table).


%% NICO NO PUEDE ESCRIBIR NINGUN COMENTARIO ACA
%% YOU HAVE NO POWER HERE!
%%
make_play(UserName, GameId, Play) ->
    OldTable = get_game_table(GameId),
    {U1, U2} = get_game_players(GameId),
    Num      = if UserName == U1 -> 1; true -> 2 end,
    [H | T]  = Play,
    IsGameOver = is_game_over(OldTable),
    case IsGameOver of
        true -> game_over;
        _    ->
            case H of
                97  -> 
                    X2 = get_number(T),
                    B  = (X2 < 4) and (X2 > 0),
                    if B -> L2 = lists:nth(2, OldTable),
                            L3 = lists:nth(3, OldTable),
                            L1 = set_nth_list(lists:nth(1, OldTable), X2, Num),
                            [L1, L2, L3];
                       true -> error
                    end;
                98  -> 
                    X2 = get_number(T),
                    B  = (X2 < 4) and (X2 > 0),
                    if B -> L1 = lists:nth(1, OldTable),
                            L3 = lists:nth(3, OldTable),
                            L2 = set_nth_list(lists:nth(2, OldTable), X2, Num),
                            [L1, L2, L3];
                       true -> error
                    end;
                99  -> 
                    X2 = get_number(T),
                    B  = (X2 < 4) and (X2 > 0),
                    if B -> L1 = lists:nth(1, OldTable),
                            L2 = lists:nth(2, OldTable),
                            L3 = set_nth_list(lists:nth(3, OldTable), X2, Num),
                            [L1, L2, L3];
                       true -> error
                    end;
                _ -> 
                    error
            end
    end.
        %"b" ++ X -> list_to_integer(X)
        %"c" ++ X -> list_to_integer(X)

%% Funcion auxiliar de make_play
set_nth_list([H | T], Number, Element) ->
    case Number of
        1 -> 
            [Element] ++ T;
        _ -> 
            [H] ++ set_nth_list(T, Number - 1, Element)
    end.
       
%% Funcion auxiliar de make_play, para ver que la jugada
%% tenga el formato indicado
get_number(Tail) ->
    case Tail of
        []      -> error;
        [H | T] ->
            try erlang:list_to_integer([H]) of  
                X -> 
                    if T == [] -> X;
                       true -> error
                    end
            catch
                _:_ -> error
            end
    end.    
