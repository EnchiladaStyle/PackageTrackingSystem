-module(pkg_id_request_handler).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([send_request/1]).

%% State: Keep track of next process to send the request
-record(state, {workers, index}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% List of worker pids for round robin distribution
init([]) ->
    Workers = start_workers(1000),  % Assuming we have 1000 worker processes in Code File Two
    {ok, #state{workers = Workers, index = 1}}.

send_request(PackageId) ->
    gen_server:cast(?MODULE, {request, PackageId}).

%% Pick next worker and send the request in a round robin way
handle_cast({request, PackageId}, #state{workers=Workers, index=Index} = State) ->
    NextWorker = lists:nth(Index, Workers),
    NextIndex = if Index == length(Workers) -> 1; true -> Index + 1 end,
    NextWorker ! {process_request, PackageId},
    {noreply, State#state{index = NextIndex}}.

%% Handling synchronous calls
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Handling other callbacks
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Function to start worker processes
start_workers(Num) ->
    [spawn(fun worker_loop/0) || _ <- lists:seq(1, Num)].

worker_loop() ->
    receive
        {process_request, PackageId} ->
            %% Process the package request here
            worker_loop()
    end.