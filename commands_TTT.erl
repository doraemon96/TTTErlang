-module(commands_TTT).
-compile(export_all).

cmd_connect(UserName) ->
    case query_PList(UserName) of
        true -> {}
