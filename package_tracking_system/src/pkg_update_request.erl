-module(pkg_update_request).
-behaviour(gen_server).

%% API
-export([start_link/1, update_package/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Client API

%% Starts the gen_server with a given PackageId
start_link(PackageId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{package_id => PackageId, updates => []}, []).

%% Sends an update request to the gen_server
update_package(PackageId, Lid, Latitude, Longitude, PkgState) ->
    gen_server:call(?MODULE, {update, PackageId, Lid, Latitude, Longitude, PkgState}).

%% gen_server Callbacks

%% Initialize the server state
init(InitialState) ->
    %% Initial state includes a map to track updates
    {ok, InitialState}.

%% Handle `update` requests
handle_call({update, PackageId, Lid, Latitude, Longitude, _OldState}, _From, State) ->
    %% Determine the new state of the package
    NewState = case lists:prefix("TRU", Lid) of
        true -> <<"Out for Delivery">>;
        _ -> case lists:prefix("DEL", Lid) of
                true -> <<"Delivered">>;
                _ -> <<"In Transit">>
             end
    end,

    %% Simulate updating the Riak database (replace with actual interaction)
    UpdatedData = #{id => Lid, latitude => Latitude, longitude => Longitude, state => NewState},
    io:format("Updating Riak for Package ID ~p with data: ~p~n", [PackageId, UpdatedData]),

    %% Update the internal state map
    UpdatedState = State#{updates => [{Lid, UpdatedData} | maps:get(updates, State, [])]},

    %% Respond with success
    {reply, ok, UpdatedState};

%% Catch-all for unexpected calls
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_request}, State}.

%% Handle asynchronous messages (not used here but included for extensibility)
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle unexpected messages
handle_info(_Info, State) ->
    {noreply, State}.

%% Cleanup resources when the server terminates
terminate(_Reason, _State) ->
    ok.

%% Handle changes to the server's code
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
