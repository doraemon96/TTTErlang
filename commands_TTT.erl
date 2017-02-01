-compile(export_all).
-include("database_TTT.erl").
-include("gametable_TTT.erl").

%% CON
%%
%% TODO
cmd_con(UserName, PSocket) ->
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
cmd_lsg(PSocket) ->
    GamesList  = list_games(),
    GamesList2 = lists:map(fun({_ , X, Y, Z, _}) -> erlang:integer_to_list(X) ++ " " 
                                                    ++ Y ++ " " 
                                                    ++ Z end, GamesList),
    PSocket ! {pCommand, {lsg, GamesList2}},
    ok.

%% NEW
%%
%% TODO
cmd_new(PSocket, PlayerId) ->
    F = fun() -> 
          Handle = qlc:q([P#game.gameid || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    Ids = mnesia:activity(transaction, F),
    case Ids of
        [] -> create_game(1, PlayerId),
              PSocket ! {pCommand, {new_ok, 1}};
        _  -> create_game(ID = lists:max(Ids) + 1, PlayerId),
              PSocket ! {pCommand, {new_ok, ID}}
    end,
    ok.
    

%% ACC
%%
%% TODO
cmd_acc(PSocket, GameId, UserName) ->
    F = fun() -> R = mnesia:read({game, GameId}),
                 case R of
                     []   -> PSocket ! {pCommand, {acc, not_exists}};
                     [G]  -> case erlang:element(4, G) of
                                "*waiting*" -> PSocket ! {pCommand, {acc, acc}},
                                               mnesia:write(G#game{user2=UserName});
                                _           -> PSocket ! {pCommand, {acc, not_acc}}
                             end
                 end
        end,
    mnesia:activity(transaction, F), 
    ok.

%% PLA
%%
%% TODO
cmd_pla() ->
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
