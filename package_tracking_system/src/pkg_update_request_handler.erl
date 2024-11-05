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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


handle_call_data_test_() ->
    {setup,
        fun()-> meck:new(pkg_update_request_receiver, [non_strict]),
    		meck:expect(pkg_update_request_receiver, update_package, fun(_, _, _, _, _, _) -> ok end)
    		 end,
 	fun(_)-> meck:unload(pkg_update_request_receiver) end,
	[
        %% Happy Path: Reverse the Used list when T is empty
        ?_assertEqual(
            {reply, ok, {[1], [2]}},
            pkg_update_request_handler:handle_call({send_update,b,c,d,e,f}, self(), {[2,1], []})),
	?_assertEqual({reply,none,{[],[]}},
	   pkg_update_request_handler:handle_call({send_update,b,c,d,e,f}, self(), {[], []}))
    ]}.



%% Tests for `pkg_update_request_handler:handle_call({send_update, ...}, _From, {Used, [H|T]})`
send_update_case_tests() ->
    [
        %% Happy Path: Successful call with non-empty Used and list [H|T]
        ?_assertEqual(
            {reply, ok, {[1], [2,3]}},
            pkg_update_request_handler:handle_call(
                {send_update, "pkg123", "wh456", 45.0, -93.0, "in_transit"},
                self(),
                {[], [1,2,3]}
            )
        ),

        %% Edge Case: Empty Used list but with non-empty T
        ?_assertEqual(
            {reply, ok, {[5], []}},
            pkg_update_request_handler:handle_call(
                {send_update, "pkg789", "wh789", 40.7, -74.0, "delivered"},
                self(),
                {[], [5]}
            )
        ),

        %% Edge Case: Invalid PackageState - simulate an error
        meck:expect(pkg_update_request_receiver, update_package, fun(_, _, _, _, _, _) -> {error, invalid_state} end),
        ?_assertEqual(
            {reply, {error, invalid_state}, {[5], []}},
            catch pkg_update_request_handler:handle_call(
                {send_update, "pkg101", "wh101", 40.7, -74.0, invalid_state},
                self(),
                {[], [5]}
            )
        )
    ].

-endif.