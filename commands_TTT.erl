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
    GamesList2 = lists:map(fun({_ ,X, Y, Z}) -> erlang:integer_to_list(X) ++ " " 
                                                ++ Y ++ " " 
                                                ++ Z end, GamesList),
    PSocket ! {pCommand, {lsg, GamesList2}},
    ok.

%% NEW
%%
%% TODO
cmd_new(PSocket) ->
    F = fun() -> 
          Handle = qlc:q([P#game.gameid || P <- mnesia:table(game)]),
          qlc:e(Handle)
        end,
    Max = lists:max(mnesia:activity(transaction, F)),
    ok.
    %create_game(Max + 1, UName). 
    

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
