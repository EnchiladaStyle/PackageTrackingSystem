-module(truck_tracker).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, set_location/3, get_location/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {latitude = 0.0, longitude = 0.0}).

%% Public API

start_link(Truck_ID) ->
    %% Ensure Truck_ID is converted to an atom for registration
    _TruckAtom = case Truck_ID of
        <<_Bin/binary>> -> binary_to_atom(Truck_ID, utf8);  % Handle binary input
        _ -> list_to_atom(integer_to_list(Truck_ID))        % Handle integer input
    end.

stop(Truck_ID) ->
    TruckAtom = convert_to_atom(Truck_ID),
    gen_server:call(TruckAtom, stop).

set_location(Truck_ID, Latitude, Longitude) ->
    TruckAtom = convert_to_atom(Truck_ID),
    gen_server:call(TruckAtom, {set_location, Latitude, Longitude}).

get_location(Truck_ID) ->
    TruckAtom = convert_to_atom(Truck_ID),
    io:format("get_location called with TruckAtom: ~p~n", [TruckAtom]),
    gen_server:call(TruckAtom, get_location).

%% Utility function to convert Truck_ID to an atom
convert_to_atom(Truck_ID) ->
    io:format("convert_to_atom received Truck_ID: ~p~n", [Truck_ID]),
    case Truck_ID of
        <<_Bin/binary>> -> list_to_atom(binary_to_list(Truck_ID)); % Handle valid binary Truck_ID
        [_|_] -> list_to_atom(Truck_ID);                          % Handle valid string Truck_ID
        _ -> error({invalid_truck_id, <<"Truck ID must be a string or binary">>})
    end.





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
