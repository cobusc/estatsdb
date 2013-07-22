%% @author cobusc <cobusc@miranetworks.net>

-module(refresh_resource).
-export([init/1, 
         to_json/2, 
         content_types_provided/2, 
         allowed_methods/2, 
         malformed_request/2,
         forbidden/2
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

forbidden(RD, Ctx) ->
    {"127.0.0.1" =/= wrq:peer(RD), RD, Ctx}.

%%
%% @doc Check if the request parameters are correct and complete
%%

malformed_request(ReqData, Ctx) ->
    {false, ReqData, Ctx}.

%%
%% @doc Response formatting in JSON format.
%%
-spec to_json(ReqData::#wm_reqdata{}, Ctx::any()) -> 
    {JsonResponse::string(), Ctx::any(), {list({string(), string()}), tuple()}}.

to_json(ReqData, Ctx) ->
    ok = schema_server:refresh(),
    JsonResponse = mochijson2:encode({struct,[{"success", true}]}),
    {JsonResponse, ReqData, Ctx}.

