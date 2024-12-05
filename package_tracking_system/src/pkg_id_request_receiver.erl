-module(pkg_id_request_receiver).
-behaviour(gen_server).

%% API
-export([start_link/0, store_package/2]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%% Riak Connection
-define(RIAK_HOST, "137.184.176.152").
-define(RIAK_PORT, 8087).

%% API to start the worker
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Public function to store package data
store_package(Pid, PackageId) ->
    gen_server:cast(Pid, {store_package, PackageId}).

%% Callbacks
init([]) ->
    %% Start the Riak client application
    ok = application:start(riakc),

    %% Connect to the database
    {ok, RiakPid} = riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT),

    %% Store the Pid in ther server's state
    {ok, #{riak_pid => RiakPid}}.

handle_cast({store_package, PackageId}, State) ->
    %% Default data
    WarehouseId = <<"DefaultWarehouse">>,
    Latitude = 51.5074,  %% Example location (London)
    Longitude = -0.1278,
    StateData = <<"In Transit">>,
    %% Data structure
    PackageData = {WarehouseId, Latitude, Longitude, StateData},

    %% Convert data to a binary term for Riak
    Bucket = <<"default">>,
    Key = binary:encode_unsigned(PackageId), %% Package Id is the Key
    Value = term_to_binary(PackageData),
    
    %% Get Riak connection from the state
    RiakPid = maps:get(riak_pid, State),

    %% Store in database
    case riakc_pb_socket:put(RiakPid, riakc_obj:new(Bucket, Key, Value)) of
        ok ->
            io:format("Package ~p stored successfully in database~n", [PackageId]);
        {error, Reason} ->
            io:format("Failed to store package~p in database: ~p~n", [PackageId, Reason])
        end,
    
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Stop the Riak connection
    RiakPid = maps:get(riak_pid, State),
    riakc_pb_socket:stop(RiakPid),

    %% Stop the Riak application
    application:stop(riakc),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
