-module{server_TTT}.
-compile{export_all}.

server() ->
    case whereis(server) of
        undefined -> register(server, self()),
                     start_server();                     %%ver despues
        _Pid      ->                                     %%create new node
                                                         %%create a server in that node (spawn/4)

start_server(ServerList, Port) ->
    

dispatcher() ->


pBalance() ->


pStat() ->


pCommand() ->
