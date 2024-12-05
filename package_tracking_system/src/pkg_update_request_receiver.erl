-module(pkg_update_request_receiver).
-behaviour(gen_server).

%% API
-export([start_link/1, update_package/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Riak Connection
-define(RIAK_HOST, "137.184.176.152").
-define(RIAK_PORT, 8087).

%% Client API
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% This function is called to send an update request to the server
update_package(Name, PackageId, WarehouseId, Latitude, Longitude, State) ->
    gen_server:call(Name, {update, PackageId, WarehouseId, Latitude, Longitude, State}).

%% Server callback - initialization
init([]) ->
    %% Start the Riak client application
    ok = application:start(riakc),

    %% Connect to the database
    {ok, RiakPid} = riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT),

    %% Store the Pid in ther server's state
    {ok, #{riak_pid => RiakPid}}.

%% Handle the update request
handle_call({update, PackageId, WarehouseId, Latitude, Longitude, _State}, _From, State) ->
    %% Analyze and change the state
    NewState = case lists:prefix("TRU", WarehouseId) of
        true -> "Out for Delivery";
        _ -> case lists:prefix("DEL", WarehouseId) of
                true ->  "Delivered";
                _ -> State
             end
    end,
    %% Update the package data in the Riak database
    UpdatedData = #{warehouse_id => WarehouseId, latitude => Latitude, longitude => Longitude, state => NewState},

    %% Get Riak connection from the state
    RiakPid = maps:get(riak_pid, State),

    %% Fetch the old object
    {ok, Obj} = riakc_pb_socket:get(RiakPid, <<"default">>, PackageId),

    %% Create a new object with the updated value
    UpdatedObj = riak_object:update_value(Obj, UpdatedData),

    %% Store the updated object back to Riak
    {ok, _} = riakc_pb_socket:put(RiakPid, UpdatedObj),

    %% Update the internal state (if needed)
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
