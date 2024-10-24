-module(pkg_id_request_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-record(state, {workers, index}).

%% Setup and teardown functions
setup() ->
    %% Start the gen_server and return the state for the test
    {ok, Pid} = pkg_id_request_handler:start_link(),
    State = gen_server:call(Pid, get_state),
    {Pid, State}.

teardown({Pid, _State}) ->
    %% Stop the gen_server after the test
    gen_server:stop(Pid).

%% Unit Tests
init_test_() ->
    %% Test that init/1 correctly sets the initial state with 1000 workers
    {setup,
     fun setup/0,
     fun({Pid, State}) ->
         Workers = State#state.workers,
         Index = State#state.index,
         ?_assertEqual(1000, length(Workers)),  %% Expect 1000 workers
         ?_assertEqual(1, Index),               %% Initial index should be 1
         teardown({Pid, State})
     end}.

handle_cast_test_() ->
    {setup,
     fun setup/0,
     fun({Pid, State}) ->
         %% Send a request and verify the worker receives it
         PkgId = "test_pkg_123",
         %% Mock worker processes to capture the message sent to them
         Worker1 = spawn(fun() -> receive {process_request, _} -> ok end end),
         Worker2 = spawn(fun() -> receive {process_request, _} -> ok end end),
         Workers = [Worker1, Worker2],
         %% Overriding the initial state
         NewState = State#state{workers = Workers, index = 1},
         gen_server:cast(Pid, {request, PkgId}),
         
         %% Check if the request was sent to the first worker
         receive
             {process_request, "test_pkg_123"} ->
                 ?_assertEqual(Worker1, self())  %% Verify that Worker1 gets the first request
         after 1000 ->
             ?_assertEqual(false, "First worker did not receive the request")
         end,

         %% Verify the round-robin behavior by sending another request
         gen_server:cast(Pid, {request, PkgId}),
         receive
             {process_request, "test_pkg_123"} ->
                 ?_assertEqual(Worker2, self())  %% Verify that Worker2 gets the second request
         after 1000 ->
             ?_assertEqual(false, "Second worker did not receive the request")
         end,

         teardown({Pid, State})
     end}.

send_request_test_() ->
    {setup,
     fun setup/0,
     fun({Pid, _State}) ->
         %% Test that send_request/1 properly casts the request
         PkgId = "pkg_test",
         %% Override the cast to validate that the cast was received
         meck:new(pkg_id_request_handler),
         meck:expect(pkg_id_request_handler, handle_cast, fun(_) -> ok end),
         pkg_id_request_handler:send_request(PkgId),
         ?_assert(meck:called(pkg_id_request_handler, handle_cast, [{request, PkgId}])),
         meck:unload(pkg_id_request_handler),

         teardown({Pid, _State})
     end}.
