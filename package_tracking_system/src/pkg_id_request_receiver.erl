-module(pkg_id_request_receiver).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_info({process_request, PackageId}, State) ->
    %% Create entry in Riak with default values
    WarehouseId = "W123",
    Latitude = 0.0,
    Longitude = 0.0,
    State = "In Transit",
    %% Assuming a function write_to_riak/2 exists to write the package to Riak
    write_to_riak(PackageId, {WarehouseId, Latitude, Longitude, State}),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Helper function to write package data to Riak
write_to_riak(PackageId, {WarehouseId, Latitude, Longitude, State}) ->
    %% Implementation to write to Riak goes here
    ok.
