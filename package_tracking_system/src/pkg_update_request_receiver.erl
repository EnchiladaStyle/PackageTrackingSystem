-module(pkg_update_request_receiver).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_info({process_update, {PackageId, WarehouseId, Latitude, Longitude, State}}, State) ->
    %% Analyze state and apply logic
    NewState = case WarehouseId of
        <<"TRU", _/binary>> -> "Out for Delivery";
        _ -> case State of
            "Out for Delivery" -> "Delivered";
            _ -> State
        end
    end,
    %% Update the Riak entry
    update_riak(PackageId, {WarehouseId, Latitude, Longitude, NewState}),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Helper function to write package data to Riak
update_riak(PackageId, {WarehouseId, Latitude, Longitude, NewState}) ->
    %% Logic to update Riak database with the new package information
    ok.
