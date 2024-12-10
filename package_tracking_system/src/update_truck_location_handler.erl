%% src/update_truck_location_handler.erl
-module(update_truck_location_handler).
-export([init/2]).

%% Function to handle POST requests to "/updateTruckLocation"
init(Req, State) ->
    %% Read the body of the POST request
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    %% Try to decode the JSON body
    case jsx:decode(Body, [return_maps]) of
        #{<<"truck_id">> := TruckId, <<"lattitude">> := Latitude, <<"longitude">> := Longitude} ->
            %% Ensure the truck process exists (create if necessary)
            {ok, _TruckPid} = cowBoyServer:ensure_truck_process(TruckId),
            %% Update the truck's location
            truck_tracker:set_location(TruckId, Latitude, Longitude),
            io:format("decoded JSON. Body: ~p~f~f. Success", [TruckId, Latitude, Longitude]),
            %% Respond with success
            ResponseBody = <<"{\"status\":\"success\"}">>,
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseBody, Req1),
            {ok, Req2, State};

        %% If there's an error decoding the JSON
        Error ->
            io:format("Failed to decode JSON. Body: ~s. Error: ~p~n", [Body, Error]),
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"Invalid JSON\"}">>, Req1),
            {ok, Req2, State}
    end.
