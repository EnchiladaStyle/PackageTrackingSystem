-module(truck_tracker).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, set_location/3, get_location/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {latitude = 0.0, longitude = 0.0}).

%% Public API

start_link(Truck_ID) ->
    %% Convert Truck_ID to an atom for registration
    TruckAtom = list_to_atom(integer_to_list(Truck_ID)),
    gen_server:start_link({local, TruckAtom}, ?MODULE, [], []).



stop(Truck_ID) ->
    TruckAtom = list_to_atom(integer_to_list(Truck_ID)),
    gen_server:call(TruckAtom, stop).
    
set_location(Truck_ID, Latitude, Longitude) ->
    TruckAtom = list_to_atom(integer_to_list(Truck_ID)),
    gen_server:call(TruckAtom, {set_location, Latitude, Longitude}).
    
get_location(Truck_ID) ->
    TruckAtom = list_to_atom(integer_to_list(Truck_ID)),
    gen_server:call(TruckAtom, get_location).




%% gen_server callbacks

init([]) ->
    %% Initial coordinates (latitude, longitude)
    {ok, #state{latitude = 0.0, longitude = 0.0}}.

handle_call({set_location, Latitude, Longitude}, _From, State) ->
    %% Update the coordinates in the state
    NewState = State#state{latitude = Latitude, longitude = Longitude},
    {reply, ok, NewState};

handle_call(get_location, _From, State) ->
    %% Return the current coordinates
    {reply, {State#state.latitude, State#state.longitude}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
