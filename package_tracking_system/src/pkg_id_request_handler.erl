-module(pkg_id_request_handler).
-behaviour(gen_server).

%% API
-export([start_link/1, send_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Record for maintaining state
-record(state, {workers, count=0}).

%% Start the request handler
start_link(Workers) ->
    gen_server:start_link(?MODULE, Workers, []).

%% Client API to send request
send_request(Pid, PackageId) ->
    gen_server:call(Pid, {send_request, PackageId}).

%% Callbacks
init(Workers) ->
    {ok, #state{workers = Workers}}.

handle_call({send_request, PackageId}, _From, State = #state{workers=Workers, count=Count}) ->
    case Workers of
        [] ->
            {reply, {error, no_workers_available}, State};
        _ ->
            %% Round-robin: Pick a worker based on the count
            Worker = lists:nth((Count rem length(Workers)) + 1, Workers),
            Worker ! {new_package, PackageId},
            %% Update state to keep track of next worker
            {reply, ok, State#state{count = Count + 1}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
