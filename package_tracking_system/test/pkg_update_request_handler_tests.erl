-module(pkg_update_request_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-record(state, {workers, index}).

%% Setup and teardown functions
setup() ->
    %% Start the gen_server and return its pid and initial state for testing
    {ok, Pid} = pkg_update_request_handler:start_link(),
    State = gen_server:call(Pid, get_state),
    {Pid, State}.

teardown({Pid, _State}) ->
    %% Stop the gen_server after the test
    gen_server:stop(Pid).

%% Unit Tests
init_test_() ->
    %% Test that init/1 correctly sets up the initial state with 1000 workers
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
         %% Mock worker processes to capture the message sent to them
         Data = "update_data",
         Worker1 = spawn(fun() -> receive {process_update, _} -> ok end end),
         Worker2 = spawn(fun() -> receive {process_update, _} -> ok end end),
         Workers = [Worker1, Worker2],
         %% Overriding the initial state
         NewState = State#state{workers = Workers, index = 1},
         gen_server:cast(Pid, {update_request, Data}),
         
         %% Verify that the first worker receives the request
         receive
             {process_update, "update_data"} ->
                 ?_assertEqual(Worker1, self())  %% Verify that Worker1 gets the first request
         after 1000 ->
             ?_assertEqual(true, false) %% First worker did not receive the update request
         end,

         %% Verify round-robin behavior by sending another update request
         gen_server:cast(Pid, {update_request, Data}),
         receive
             {process_update, "update_data"} ->
                 ?_assertEqual(Worker2, self())  %% Verify that Worker2 gets the second request
         after 1000 ->
             ?_assertEqual(false, "Second worker did not receive the update request")
         end,

         teardown({Pid, State})
     end}.

send_update_test_() ->
    {setup,
     fun setup/0,
     fun({Pid, _State}) ->
         %% Test that send_update/1 properly casts the request
         Data = "update_test_data",
         %% Override the cast to validate that the cast was received
         meck:new(pkg_update_request_handler),
         meck:expect(pkg_update_request_handler, handle_cast, fun(_) -> ok end),
         pkg_update_request_handler:send_update(Data),
         ?_assert(meck:called(pkg_update_request_handler, handle_cast, [{update_request, Data}])),
         meck:unload(pkg_update_request_handler),

         teardown({Pid, _State})
     end}.
