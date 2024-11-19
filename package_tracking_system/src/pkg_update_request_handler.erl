% -module(pkg_update_request_handler).
% -export([init/2]).

% %% Function to handle POST requests to "/updateTruckLocation"
% init(Req, State) ->
%     %% Read the body of the POST request
%     {ok, Body, Req1} = cowboy_req:read_body(Req),
%     %% Try to decode the JSON body
%     case jsx:decode(Body, [return_maps]) of
        
%         #{<<"pkg_id">> := PackageId,<<"location_id">>:= Lid, <<"lattitude">> := Latitude, <<"longitude">> := Longitude, <<"state">>:=State} ->
            
%             %% Update the pkg_ID 
%             %% update_package(PackageId, Lid, Latitude, Longitude, State)
%             pkg_update_request:update_package(PackageId,Lid, Latitude, Longitude, State ),
%             io:format("decoded JSON. Body: ~p~f~f. Success", [PackageId,Lid, Latitude, Longitude, State]),
%             %% Respond with success
%             ResponseBody = <<"{\"status\":\"success\"}">>,
%             Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseBody, Req1),
%             {ok, Req2, State};

%         %% If there's an error decoding the JSON
%         Error ->
%             io:format("Failed to decode JSON. Body: ~s. Error: ~p~n", [Body, Error]),
%             Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"Invalid JSON\"}">>, Req1),
%             {ok, Req2, State}
%     end.

-module(pkg_update_request_handler).
-export([init/2]).
%% Handler for updating package information
init(Req, State) ->
    %% Read and decode the JSON body
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case jsx:decode(Body, [return_maps]) of
        #{<<"pkg_id">> := PackageId, <<"location_id">> := Lid, <<"latitude">> := Latitude, <<"longitude">> := Longitude, <<"state">> := PkgState} ->
            %% Ensure the process exists for the package
            case cowBoyServer:ensure_truck_process(PackageId) of
                {ok, Pid} ->
                    %% Send the update request to the process
                    gen_server:call(Pid, {update, PackageId, Lid, Latitude, Longitude, PkgState}),
                    %% Respond with success
                    ResponseBody = <<"{\"status\":\"success\"}">>,
                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseBody, Req1),
                    {ok, Req2, State};
                Error ->
                    %% If there's an issue ensuring the process, return an error
                    io:format("Failed to start or retrieve process for ~p. Error: ~p~n", [PackageId, Error]),
                    Req2 = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"Server error\"}">>, Req1),
                    {ok, Req2, State}
            end;
        %% Handle invalid JSON
        _ ->
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"Invalid JSON\"}">>, Req1),
            {ok, Req2, State}
    end.





