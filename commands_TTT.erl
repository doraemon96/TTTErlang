-module(commands_TTT).
-compile(export_all).

%%cmd_connect(UserName) ->
%%    case query_PList(UserName) of
%%        true -> {}
try_connect(Username) ->
    %%Probamos si el usuario no existe
    Global = global:registered_names(),
    case lists:any(fun(X) -> X =:= Username, Global) of

%% LSG
cmd_list() ->
    ok.

%% NEW
cmd_newgame() ->
    ok.

%% ACC
cmd_accept() ->
    ok.

%% PLA
cmd_makeplay() ->
    ok.

%% OBS
cmd_spectate() ->
    ok.

%% LEA
cmd_leavespectate() ->
    ok.

%% BYE
cmd_disconnect() ->
    ok.
