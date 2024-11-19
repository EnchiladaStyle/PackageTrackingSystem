-module(pkg_id_request_receiver).
-behaviour(gen_server).

%% API
-export([start_link/0, store_package/2]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%% API to start the worker
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Public function to store package data
store_package(Pid, PackageId) ->
    gen_server:cast(Pid, {store_package, PackageId}).

%% Callbacks
init([]) ->
    {ok, []}.

handle_cast({store_package, PackageId}, State) ->
    %% Default data
    WarehouseId = <<"DefaultWarehouse">>,
    Latitude = 51.5074,  %% Example location (London)
    Longitude = -0.1278,
    StateData = <<"In Transit">>,
    %% Data structure
    PackageData = {WarehouseId, Latitude, Longitude, StateData},
    
    %% Simulate storing in Riak (replace with actual Riak API call)
    riak:put(PackageId, PackageData),
    
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
