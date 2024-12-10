%% src/package_tracking_system.erl
-module(package_tracking_system).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Ensure Ranch is started
    case application:ensure_all_started(ranch) of
        {ok, _} ->
            % If Ranch starts successfully, proceed to start Cowboy
            case application:ensure_all_started(cowboy) of
                {ok, _} ->
                    % Start the Cowboy server
                    cowBoyServer:start(),
                    {ok, self()};
                {error, Reason} ->
                    io:format("Failed to start Cowboy: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to start Ranch: ~p~n", [Reason]),
            {error, Reason}
    end.

stop(_State) ->
    ok.
