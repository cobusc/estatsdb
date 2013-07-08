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
    case request:build(QueryArgs) of
        {error, Reason} ->
            R = wrq:set_resp_body(Reason, ReqData),
            {true, R, Ctx};
        {ok, #request_info{}=ReqInfo} ->
            {false, ReqData, ReqInfo}
    end.

%%
%% @doc Response formatting in JSON format.
%%
-spec to_json(ReqData::#wm_reqdata{}, RequestInfo::#request_info{}) -> 
    {JsonResponse::string(), ReqData::#wm_reqdata{}, {list({string(), string()}), tuple()}}.

to_json(ReqData, RequestInfo)
when is_record(RequestInfo, request_info) ->
    Sql = sqlbuilder:set_sql(RequestInfo),
    {ok, Cols, Rows} = pgdb_tools:equery(Sql, []),
    [PropList] = pgdb_tools:transduce(Cols, Rows),
    Sanitized = response:sanitize_for_json(PropList),
    Result = {struct, Sanitized},
    JsonResponse = mochijson2:encode(Result),
    {JsonResponse, ReqData, undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

