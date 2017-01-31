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
cmd_lsg(PSocket, CmdId) ->
    GamesList  = list_games(),
    io:format("Start ~n", []),
    GamesList2 = lists:map(fun({_ , X, Y, Z, _}) -> erlang:integer_to_list(X) ++ " " 
                                                    ++ Y ++ " " 
                                                    ++ Z end, GamesList),
    PSocket ! {pCommand, {lsg, GamesList2}},
    io:format("End~n", []),
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
cmd_acc() ->
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
cmd_bye() ->
    ok.
