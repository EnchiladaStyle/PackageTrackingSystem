-module(pkg_update_request_handler).

-behaviour(gen_server).


%% API

-export([start_link/1, send_update_request/2]).


%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% API to start the request handler

start_link(RecieverPids) ->

    gen_server:start_link(?MODULE, RecieverPids, []).


%% Public API to send an update request

send_update_request(Pid, {PackageId, WarehouseId, Latitude, Longitude, State}) ->

    gen_server:call(Pid, {send_update, PackageId, WarehouseId, Latitude, Longitude, State}).


%% Callbacks

init(RecieverPids) ->

    {ok, {[], RecieverPids}}.

handle_call(Data, From, {Used, []}) ->
    handle_call(Data, From, {[], lists:reverse(Used)});

handle_call({send_update, PackageId, WarehouseId, Latitude, Longitude, PackageState}, _From, {Used, [H|T]}) ->
    pkg_update_request_receiver:update_package(H, PackageId, WarehouseId, Latitude, Longitude, PackageState),
    {reply, ok, {[H]++ Used, T}}.


handle_cast(_Msg, State) ->

    {noreply, State}.


handle_info(_Info, State) ->

    {noreply, State}.


terminate(_Reason, _State) ->

    ok.


code_change(_OldVsn, State, _Extra) ->

    {ok, State}.