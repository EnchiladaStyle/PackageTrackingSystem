-module(pkg_update_request_handler).
-behaviour(gen_server).

%% API
-export([start_link/1, send_update_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Record to store state
-record(state, {workers, count=0}).

%% API to start the request handler
start_link(Workers) ->
    gen_server:start_link(?MODULE, Workers, []).

%% Public API to send an update request
send_update_request(Pid, {PackageId, WarehouseId, Latitude, Longitude, State}) ->
    gen_server:call(Pid, {send_update, PackageId, WarehouseId, Latitude, Longitude, State}).

%% Callbacks
init(Workers) ->
    {ok, #state{workers = Workers}}.

handle_call({send_update, PackageId, WarehouseId, Latitude, Longitude, State}, _From, State = #state{workers=Workers, count=Count}) ->
    %% Round-robin dispatch
    Worker = lists:nth((Count rem length(Workers)) + 1, Workers),
    %% Send the update request to the selected worker
    Worker ! {update_package, PackageId, WarehouseId, Latitude, Longitude, State},
    %% Increment the counter to ensure round-robin distribution
    {reply, ok, State#state{count = Count + 1}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
