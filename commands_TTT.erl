-module(commands_TTT).
-compile(export_all).

try_connect(Username) ->
    %%Probamos si el usuario no existe
    Global = global:registered_names(),
    case lists:any(fun(X) -> X =:= Username, Global) of
        
