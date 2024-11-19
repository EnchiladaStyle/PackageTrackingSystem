-module(package_location_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, _Opts) ->
    %% Extract Package_ID from the query string or body
    {ok, PackageID} = extract_package_id(Req),

    %% Mock fetching package location
    {ok, Location} = mock_fetch_package_location(PackageID),

    %% Check if the location is a Truck_ID
    Response =
        case is_truck_id(Location) of
            true ->
                %% Convert Truck_ID (e.g., "TRU001") to an integer or atom
                TruckID = convert_truck_id(Location),
                io:format("Truck ID being queried: ~p~n", [TruckID]),

                case truck_tracker:get_location(TruckID) of
                    {Lat, Lon} when is_number(Lat), is_number(Lon) ->
                        io:format("Successfully retrieved location: Latitude=~p, Longitude=~p~n", [Lat, Lon]),
                        #{status => <<"in_transit">>, latitude => Lat, longitude => Lon};
                    {error, Reason} ->
                        io:format("Error retrieving Truck location: ~p~n", [Reason]),
                        #{status => <<"unknown">>, message => Reason};
                    _ ->
                        io:format("Unexpected response from get_location for TruckID: ~p~n", [TruckID]),
                        #{status => <<"unknown">>, message => <<"Truck location unavailable">>}
                end;
            false ->
                io:format("Non-truck location detected: ~p~n", [Location]),
                #{status => <<"delivered">>, location => ensure_binary(Location)}
        end,

    %% Transform Response into a front-end-friendly format
    FinalResponse = maps:map(
        fun(_, Value) ->
            case Value of
                List when is_list(List) -> list_to_binary(List);  %% Convert strings to binaries
                Map when is_map(Map) -> maps:map(fun(_, V) -> ensure_binary(V) end, Map); %% Recursively handle maps
                Other -> Other %% Leave integers, binaries, etc., as-is
            end
        end, Response),

    %% Send the response
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(FinalResponse), Req),
    {ok, Req, state}.


%% Ensure values are converted to binaries if necessary
ensure_binary(Value) when is_list(Value) ->
    list_to_binary(Value); %% Convert strings to binaries
ensure_binary(Value) when is_map(Value) ->
    maps:map(fun(_, V) -> ensure_binary(V) end, Value); %% Recursively process maps
ensure_binary(Value) ->
    Value. %% Leave integers, binaries, etc., unchanged

%% Extract Package_ID from query string
extract_package_id(Req) ->
    %% Read the body of the request
    case cowboy_req:read_body(Req) of
        {ok, Body, _Req2} ->
            %% Attempt to parse the JSON body
            case catch jsx:decode(Body, [return_maps]) of
                {'EXIT', _} ->
                    %% Handle invalid JSON
                    {error, <<"Invalid JSON in request body">>};
                ParsedBody ->
                    %% Attempt to extract the `package_id` from the parsed map
                    case maps:get(<<"package_id">>, ParsedBody, undefined) of
                        undefined ->
                            %% Handle missing `package_id`
                            {error, <<"Missing or invalid package_id in body">>};
                        PackageID ->
                            %% Successfully retrieved `package_id`
                            {ok, PackageID}
                    end
            end;
        {error, Reason} ->
            %% Handle errors reading the request body
            {error, <<"Failed to read request body: ">> ++ Reason}
    end.



%% Mock function to simulate fetching the package location
mock_fetch_package_location(PackageID) ->
    case PackageID of
        12345 -> {ok, "TRU001"};  % Truck ID
        67890 -> {ok, "123 Main St, City, State"};  % Static location
        _ -> {ok, "Unknown Location"}
    end.

%% Check if the location is a Truck_ID
is_truck_id(Location) ->
    %% Normalize Location to binary using the existing ensure_binary/1
    NormalizedLocation = ensure_binary(Location),
    binary:match(NormalizedLocation, <<"TRU">>) =:= {0, 3}.


%% Convert Truck_ID string to an appropriate format (integer or atom)
convert_truck_id(<<"TRU", Rest/binary>>) ->
    io:format("convert_truck_id: Binary input, returning ~p~n", [<<"TRU", Rest/binary>>]),
    <<"TRU", Rest/binary>>;
convert_truck_id(List) when is_list(List) ->
    io:format("convert_truck_id: String input, converting to binary ~p~n", [List]),
    convert_truck_id(list_to_binary(List));
convert_truck_id(Other) ->
    io:format("convert_truck_id: Invalid format ~p~n", [Other]),
    {error, <<"Invalid Truck ID format">>}.
