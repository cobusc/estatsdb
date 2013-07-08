-module(response).
-export([sanitize_for_json/1]).

%%
%% @doc Sanitize a proplist for JSON encoding
%%
%% The representation of certain DB types (most notably date and timestamp types)
%% are not suitable for JSON encoding. We map these types to appropriate representations
%% which are compatible with the JSON format.
%%
-spec sanitize_for_json(PropList::list({Key::binary(), Value::any()})) ->
    list({Key::binary(), Value::any()}).

sanitize_for_json(PropList) ->
    Sanitize = fun ({K,V}) ->
        {K, sanitize_value_for_json(V)}
    end,
    lists:map(Sanitize, PropList).

%%
%% @doc Date tuple to string conversion
%%

sanitize_value_for_json({Y,M,D}=Date)
when is_integer(Y), is_integer(M), is_integer(D) ->
    list_to_binary(pgdb_tools:date_to_string(Date));

%%
%% @doc Timestamp tuple to string conversion
%%

sanitize_value_for_json({{Y,Mo,D},{H,Mi,S}}=Timestamp)
when is_integer(Y), is_integer(Mo), is_integer(D),
     is_integer(H), is_integer(Mi), is_float(S) ->
    list_to_binary(pgdb_tools:timestamp_to_string(Timestamp));

%%
%% @doc Default: No conversion
%%

sanitize_value_for_json(Other) ->
    Other.
