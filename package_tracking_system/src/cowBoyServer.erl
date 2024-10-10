%% src/cowBoyServer.erl
-module(cowBoyServer).
-export([start/0]).

start() ->
    %% Compile the routes to dispatch requests to different handlers
    Dispatch = cowboy_router:compile([
        %% Route root "/" to the hello_world_handler (for GET requests)
        {'_', [
            {"/", hello_world_handler, []},  % Route for GET requests to "/"
            {"/updateTruckLocation", update_truck_location_handler, []}  % Route for POST requests to "/updateTruckLocation"
        ]}
    ]),

    %% Start the Cowboy HTTP server on port 8080
    {ok, _} = cowboy:start_clear(http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),

    %% Log to indicate that the server is running
    io:format("Server running at http://localhost:8080~n").

