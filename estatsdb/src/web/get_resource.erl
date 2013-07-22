-module(get_resource).
-export([init/1, 
         to_json/2, 
         content_types_provided/2, 
         allowed_methods/2, 
         malformed_request/2,
         resource_exists/2
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
%% @doc Check of the requested information is available
%%
resource_exists(ReqData, RequestInfo)
when is_record(RequestInfo, request_info) ->
    Sql = sqlbuilder:select_sql(RequestInfo),
    {ok, Cols, Rows} = pgdb_tools:equery(Sql, []),
    case Rows of
        [] -> 
            {false, ReqData, undefined};
        _ -> 
            [RowData] = pgdb_tools:transduce(Cols, Rows),
            {true, ReqData, RowData}
    end.

%%
%% @doc Response formatting in JSON format.
%%
-spec to_json(ReqData::#wm_reqdata{}, RowData::list({binary(), any()})) -> 
    {JsonResponse::string(), ReqData::#wm_reqdata{}, {list({string(), string()}), tuple()}}.

to_json(ReqData, RowData) ->
    Sanitized = response:sanitize_for_json(RowData),
    Result = {struct, Sanitized},
    JsonResponse = mochijson2:encode(Result),
    {JsonResponse, ReqData, undefined}.

