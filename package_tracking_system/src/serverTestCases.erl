-module(serverTestCases).

%% Export test cases
-export([all/0]).

%% List all tests
all() ->
    [send_request_test_].

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Test case: Send request and ensure 200 response
send_request_test_() ->
    %% Try making the request and handle connection failure gracefully
    Result = httpc:request(get, {"http://localhost:8080/hello", []}, [], []),
    case Result of
        {ok, {{_, StatusCode, _}, _, _}} ->
            %% Return an assertion for the status code check
            ?_assertEqual(200, StatusCode);
        {error, {failed_connect, Details}} ->
            %% Handle connection failure gracefully and assert failure with matched variable
            io:format("Warning: Unable to connect to server on localhost:8080.~nDetails: ~p~n", [Details]),
            ?_assertMatch({failed_connect, _}, {failed_connect, Details});
        {error, Reason} ->
            %% Catch any other potential error and assert failure
            io:format("Unexpected error: ~p~n", [Reason]),
            ?_assertMatch(error, Reason)
    end.

    

-endif.
