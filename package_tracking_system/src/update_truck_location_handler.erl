%% src/update_truck_location_handler.erl
-module(update_truck_location_handler).
-export([init/2]).

%% Function to handle POST requests to "/updateTruckLocation"
init(Req, State) ->
    %% Read the body of the POST request
    {ok, Body, Req1} = cowboy_req:read_body(Req),

    %% Log the body to confirm we received it correctly
    io:format("Received Body: ~s~n", [Body]),

    %% Try to decode the JSON body (assuming it's a JSON-encoded string)
    case jsx:decode(Body, [return_maps]) of
        %% If successful, log the truck_id, lattitude, and longitude
        #{<<"truck_id">> := TruckId, <<"lattitude">> := Lattitude, <<"longitude">> := Longitude} ->
            io:format("Parsed Data - Truck ID: ~p, Lattitude: ~p, Longitude: ~p~n", [TruckId, Lattitude, Longitude]),

            %% Respond with success after logging
            ResponseBody = <<"{\"status\":\"success\"}">>,
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseBody, Req1),
            {ok, Req2, State};

        %% If there's an error decoding the JSON, log the error
        Error ->
            io:format("Failed to decode JSON. Body: ~s. Error: ~p~n", [Body, Error]),
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"Invalid JSON\"}">>, Req1),
            {ok, Req2, State}
    end.
