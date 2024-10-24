-module(pkg_id_request_receiver_tests).
-include_lib("eunit/include/eunit.hrl").

%% Setup and teardown functions
setup() ->
    %% Start the gen_server and return its pid for testing
    {ok, Pid} = pkg_id_request_receiver:start_link(),
    Pid.

teardown(Pid) ->
    %% Stop the gen_server after the test
    gen_server:stop(Pid).

%% Unit Tests
handle_info_test_() ->
    {setup,
     fun setup/0,
     fun(Pid) ->
         %% Mock the write_to_riak/2 function
         meck:new(pkg_id_request_receiver),
         meck:expect(pkg_id_request_receiver, write_to_riak, fun(_, _) -> ok end),

         %% Send a process_request message
         PackageId = "pkg_456",
         gen_server:cast(Pid, {process_request, PackageId}),

         %% Verify that write_to_riak/2 was called with the correct parameters
         ExpectedData = {"W123", 0.0, 0.0, "In Transit"},
         ?_assert(meck:called(pkg_id_request_receiver, write_to_riak, [PackageId, ExpectedData])),

         %% Clean up
         meck:unload(pkg_id_request_receiver),
         teardown(Pid)
     end}.

start_link_test_() ->
    {setup,
     fun setup/0,
     fun(Pid) ->
         %% Test that the gen_server starts correctly
         ?_assertEqual(is_process_alive(Pid), true),
         teardown(Pid)
     end}.
