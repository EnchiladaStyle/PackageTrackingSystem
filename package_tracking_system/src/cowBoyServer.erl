%% src/cowBoyServer.erl
-module(cowBoyServer).
-export([start/0, get_truck_pid/1, ensure_truck_process/1]).

%% Create an ETS table to store {TruckID, TruckPID}
start() ->
    ets:new(truck_registry, [named_table, public, set]),  % Create a named ETS table
    %% Start Cowboy HTTP server on port 8080
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/updateTruckLocation", update_truck_location_handler, []},
            {"/hello", hello_world_handler, []},
            {"/pkg_update", pkg_update_request_handler, []},
            {"/pkg_id", pkg_id_request_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    io:format("Server running at http://localhost:8080~n").

%% Fetch the PID of the truck tracker process for a given TruckID
get_truck_pid(TruckID) ->
    case ets:lookup(truck_registry, TruckID) of
        [{_, Pid}] -> {ok, Pid};  % If found, return the Pid
        [] -> not_found  % If not found, return not_found
    end.

%% Ensure a truck process exists, if not, start a new one
ensure_truck_process(TruckID) ->
    case get_truck_pid(TruckID) of
        {ok, Pid} ->
            {ok, Pid};  % Truck process exists, return its PID
        not_found ->
            %% Start a new truck tracker process if it doesn't exist
            {ok, Pid} = truck_tracker:start_link(TruckID),
            ets:insert(truck_registry, {TruckID, Pid}),
            {ok, Pid}
    end.
