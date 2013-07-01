%% @author cobusc <cobusc@miranetworks.net>

-module(set_resource).
-export([init/1, 
         to_json/2, 
         content_types_provided/2, 
         allowed_methods/2, 
         malformed_request/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

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
                {value, TableInfo} ->
                    % @todo Check PKs etc, here...
                    % If all OK, pass TableInfo and QueryArgs on...
                    {false, ReqData, {QueryArgs, TableInfo}}
            end
    end.

%%
%% @doc Response formatting in JSON format.
%%
-spec to_json(ReqData::#wm_reqdata{}, {QueryArgs::list({string(), string()}), tuple()}) -> 
    {JsonResponse::string(), ReqData::#wm_reqdata{}, {list({string(), string()}), tuple()}}.

to_json(ReqData, {_QueryArgs, _TableInfo}=Ctx) ->
    Result = {error, not_implemented}, % @todo
    case Result of
        {ok, Data} -> 
            JsonResponse = mochijson2:encode({struct, [{success, true}, {data, Data}]}),
            {JsonResponse, ReqData, Ctx};
        {error, Reason} ->
            JsonResponse = mochijson2:encode({struct, [{success, false},{reason, Reason}]}),
            {JsonResponse, ReqData, Ctx}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

