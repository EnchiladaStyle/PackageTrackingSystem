%% src/hello_world_handler.erl
-module(hello_world_handler).
-export([init/2]).

%% Function to handle GET requests to "/"
init(Req, State) ->
    %% Respond with a simple "Hello, World!" message
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello, World!">>, Req),
    {ok, Req1, State}.
