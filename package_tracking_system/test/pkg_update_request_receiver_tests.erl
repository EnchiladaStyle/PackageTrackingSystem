-module(pkg_update_request_receiver_tests).
-include_lib("eunit/include/eunit.hrl").

%% Setup and teardown functions
setup() ->
    %% Start the gen_server and return its pid for testing
    {ok, Pid} = pkg_update_request_receiver:start_link(),
    Pid.

teardown(Pid) ->
    %% Stop the gen_server after the test
    gen_server:stop(Pid).

%% Unit Tests
handle_info_out_for_delivery_test_() ->
    {setup,
     fun setup/0,
     fun(Pid) ->
         %% Mock update_riak/2
         meck:new(pkg_update_request_receiver),
         meck:expect(pkg_update_request_receiver, update_riak, fun(_, _) -> ok end),

         %% Send process_update message with "TRU" WarehouseId
         PackageId = "pkg_001",
         WarehouseId = <<"TRU123">>,
         Latitude = 35.0,
         Longitude = -120.0,
         State = "In Transit",
         gen_server:cast(Pid, {process_update, {PackageId, WarehouseId, Latitude, Longitude, State}}),

         %% Verify that update_riak/2 is called with "Out for Delivery" as the NewState
         ExpectedData = {PackageId, {WarehouseId, Latitude, Longitude, "Out for Delivery"}},
         ?_assert(meck:called(pkg_update_request_receiver, update_riak, [PackageId, {WarehouseId, Latitude, Longitude, "Out for Delivery"}])),

         meck:unload(pkg_update_request_receiver),
         teardown(Pid)
     end}.

handle_info_delivered_test_() ->
    {setup,
     fun setup/0,
     fun(Pid) ->
         %% Mock update_riak/2
         meck:new(pkg_update_request_receiver),
         meck:expect(pkg_update_request_receiver, update_riak, fun(_, _) -> ok end),

         %% Send process_update message with a state "Out for Delivery"
         PackageId = "pkg_002",
         WarehouseId = <<"OTHER">>,
         Latitude = 40.0,
         Longitude = -75.0,
         State = "Out for Delivery",
         gen_server:cast(Pid, {process_update, {PackageId, WarehouseId, Latitude, Longitude, State}}),

         %% Verify that update_riak/2 is called with "Delivered" as the NewState
         ExpectedData = {PackageId, {WarehouseId, Latitude, Longitude, "Delivered"}},
         ?_assert(meck:called(pkg_update_request_receiver, update_riak, [PackageId, {WarehouseId, Latitude, Longitude, "Delivered"}])),

         meck:unload(pkg_update_request_receiver),
         teardown(Pid)
     end}.

handle_info_no_change_test_() ->
    {setup,
     fun setup/0,
     fun(Pid) ->
         %% Mock update_riak/2
         meck:new(pkg_update_request_receiver),
         meck:expect(pkg_update_request_receiver, update_riak, fun(_, _) -> ok end),

         %% Send process_update message with no state change
         PackageId = "pkg_003",
         WarehouseId = <<"OTHER">>,
         Latitude = 40.0,
         Longitude = -75.0,
         State = "In Transit",
         gen_server:cast(Pid, {process_update, {PackageId, WarehouseId, Latitude, Longitude, State}}),

         %% Verify that update_riak/2 is called with the same State ("In Transit")
         ExpectedData = {PackageId, {WarehouseId, Latitude, Longitude, "In Transit"}},
         ?_assert(meck:called(pkg_update_request_receiver, update_riak, [PackageId, {WarehouseId, Latitude, Longitude, "In Transit"}])),

         meck:unload(pkg_update_request_receiver),
         teardown(Pid)
     end}.

start_link_test_() ->
    {setup,
     fun setup/0,
     fun(Pid) ->
         %% Verify the gen_server starts properly
         ?_assertEqual(is_process_alive(Pid), true),
         teardown(Pid)
     end}.
