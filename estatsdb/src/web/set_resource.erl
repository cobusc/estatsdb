%% @author cobusc <cobusc@miranetworks.net>

-module(set_resource).
-export([init/1, 
         to_json/2, 
         content_types_provided/2, 
         allowed_methods/2, 
         malformed_request/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("estatsdb.hrl").

init(Config) ->
    {ok, Config}.
    %%enable tracing the decision core for debugging
%    {{trace, "/tmp"}, Config}.

content_types_provided(RD, Ctx) ->
    {[ {"application/json", to_json} ], RD, Ctx}.

allowed_methods(RD, Ctx) ->
    {['GET'], RD, Ctx}.

%%
%% @doc Check if the request parameters are correct and complete
%%

malformed_request(ReqData, Ctx) ->
    QueryArgs = wrq:req_qs(ReqData),
    case proplists:lookup("tablename", QueryArgs) of
        none ->
            R = wrq:set_resp_body(<<"tablename argument not specified">>, ReqData),
            {true, R, Ctx};
        {"tablename", TableName} ->
            case schema_server:lookup(TableName) of
                none ->
                    R = wrq:set_resp_body("Table "++TableName++" is not known to estatsdb. Maybe refresh the schema server?", ReqData),
                    {true, R, Ctx};
                {value, #table_info{columns=Cols}=TableInfo} ->
                    % @todo Check PKs etc, here...
                    SuppliedCols = ordsets:from_list([list_to_binary(N) || {N, _} <- QueryArgs, N =/= "tablename"]),
                    Pks = ordsets:from_list([ C || #schema_column{name=C, is_pk=true} <- Cols]), 
                    case ordsets:is_subset(Pks, SuppliedCols) of
                        true ->
                            % Check for unknown columns
                            AvailableCols = ordsets:from_list([N || #schema_column{name=N} <- Cols]),
                            case ordsets:is_subset(SuppliedCols, AvailableCols) of
                                true ->
                                    Map = fun({K,V}) ->
                                        Name = list_to_binary(K),
                                        Value = list_to_binary(V), % @todo Translate
                                        IsPk = lists:member(Name, Pks),
                                        #request_column{name=Name, value=Value, is_pk=IsPk}
                                    end,
                                    Columns = lists:map(Map, [ E || {N, _}=E <- QueryArgs, N =/= "tablename"]),
                                    Q = #request_info{
                                        table_name = TableName,
                                        table_info = TableInfo,
                                        columns = Columns
                                    },
                                    {false, ReqData, Q};
                                false ->
                                    R = wrq:set_resp_body(<<"Unknown columns specified.">>, ReqData),
                                    {true, R, Ctx}
                            end;
                        false ->
                            % Not all PKs specified
                            R = wrq:set_resp_body(<<"Some primary keys are missing.">>, ReqData),
                            {true, R, Ctx}
                    end

            end
    end.

%%
%% @doc Response formatting in JSON format.
%%
-spec to_json(ReqData::#wm_reqdata{}, RequestInfo::#request_info{}) -> 
    {JsonResponse::string(), ReqData::#wm_reqdata{}, {list({string(), string()}), tuple()}}.

to_json(ReqData, RequestInfo)
when is_record(RequestInfo, request_info) ->
    io:format("~s~n", [sqlbuilder:set_sql(RequestInfo)]),
    Result = {error, not_implemented}, % @todo
    case Result of
        {ok, Data} -> 
            JsonResponse = mochijson2:encode({struct, [{success, true}, {data, Data}]}),
            {JsonResponse, ReqData, undefined};
        {error, Reason} ->
            JsonResponse = mochijson2:encode({struct, [{success, false},{reason, Reason}]}),
            {JsonResponse, ReqData, undefined}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

