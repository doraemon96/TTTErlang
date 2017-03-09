-compile(export_all).
-include("database_TTT.erl").
-include("gametable_TTT.erl").

%% CON
%%
%% TODO
cmd_con(PSocket, UserName) ->
    case exists_username(UserName) of
        true  -> PSocket ! {pCommand, invalid_username}; 
        false ->
            add_username(UserName), 
            PSocket ! {pCommand, {valid_username, UserName}}
    end,
    ok.


%% LSG
%%
%% TODO
cmd_lsg(PSocket, CmdId) ->
    GamesList  = list_games(),
    GamesList2 = lists:map(fun({_ , X, Y, _, Z, _, _, _}) -> erlang:integer_to_list(X) ++ " " 
                                                    ++ Y ++ " " 
                                                    ++ Z end, GamesList),
    PSocket ! {pCommand, {lsg, GamesList2, CmdId}},
    ok.

%% NEW
%%
%% TODO
cmd_new(PSocket, PlayerId, CmdId) ->
    F = fun() -> 
          Handle = qlc:q([P#game.gameid || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    Ids = mnesia:activity(transaction, F),
    case Ids of
        [] -> create_game(1, PlayerId, PSocket),
              PSocket ! {pCommand, {new_ok, 1, CmdId}};
        _  -> create_game(ID = lists:max(Ids) + 1, PlayerId, PSocket),
              PSocket ! {pCommand, {new_ok, ID, CmdId}}
    end,
    ok.
    

%% ACC
%%
%% TODO
cmd_acc(PSocket, GameId, UserName, CmdId) ->
    F = fun() -> R = mnesia:read({game, GameId}),
                 case R of
                     []   -> PSocket ! {pCommand, {acc, not_exists, CmdId}};
                     [G]  -> case erlang:element(5, G) of
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
%%
%% TODO
cmd_pla(PSocket, GameId, Play, UserName, CmdId) ->
    SetGameTable = set_game_table(self(), GameId, make_play(UserName, GameId, Play), UserName),
    if 
        SetGameTable -> PSocket ! {pCommand, {pla, success}},
                        receive
                            {table, Table} -> PSocket ! {table, Table}
                        end;
        true         -> PSocket ! {pCommand, {pla, not_allowed}}
    end,
    ok. 

%% OBS
%%
%% TODO
cmd_obs() ->
    ok.

%% LEA
%%
%% TODO
cmd_lea() ->
    ok.

%% BYE
%%
%% TODO
cmd_bye(PSocket, UserName) ->
    delete_by_username(UserName),
    PSocket ! {pCommand, bye},
    ok.
