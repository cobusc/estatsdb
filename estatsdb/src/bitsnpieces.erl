-module(bitsnpieces).
-export([]).



test() ->
    TableName = "spm.testTable",
    Input = [{"a", "1"}, {"b", "2"}, {"c", "3"}, {"d", "4"}, {"e", "5"}],
    PKs = ["a", "b", "c"],

    Partition = fun({K, _} = E, {AccTrue, AccFalse}) ->
        case lists:member(K, PKs) of
            true -> {[E | AccTrue], AccFalse};
            false -> {AccTrue, [E | AccFalse]}
        end
    end,

    {PkValues, OtherValues} = lists:foldl(Partition, {[],[]}, Input),

    Transform = fun({K, V}) ->
        io_lib:format("~s = ~p", [K, V])
    end,

    SqlSetDetail = string:join(lists:map(Transform, OtherValues), ", "),

    SqlWhereDetail = string:join(lists:map(Transform, PkValues), " AND "),

    UpdateSql = "UPDATE "++TableName++" SET "++SqlSetDetail++" WHERE "++SqlWhereDetail, 

    ExtractColumn = fun({K, _}) -> K end,

    InsertColumns = string:join(lists:map(ExtractColumn, Input), ", "),

    ExtractValue = fun({_, V}) -> V end,
        
    InsertValues = string:join(lists:map(ExtractValue, Input), ", "),

    InsertSql = "INSERT INTO "++TableName++" ("++InsertColumns++") VALUES ("++InsertValues++")",

    io:format("Update: ~s~nInsert: ~s~n", [UpdateSql,InsertSql]).


%% A "set" operation will do the following:
%% 
%% INSERT INTO spm.hourly_stats (ts,host,a)
%% VALUES ('2013-01-01', 'aew1', '1');
%% ...if DUPLICATE_KEY then...
%% UPDATE spm.hourly_stats
%%    SET a = 1
%%  WHERE ts = '2013-01-01'
%%    AND host = 'aew1';
%% 
%% An "update" operation will do the following:
%% 
%% UPDATE spm.hourly_stats
%%    SET a = COALESCE(a, 0) + 1
%%  WHERE ts = '2013-01-01'
%%    AND host = 'aew1';
%% ...if 0 rows updated then...
%% INSERT INTO spm.hourly_stats (ts,host,a)
%% VALUES ('2013-01-01', 'aew1', 1);
%% 
%% Example of helper stored procedures:
%% 
%% CREATE OR REPLACE
%% FUNCTION set_helper(_InsertStatement TEXT, _UpdateStatement)
%%   RETURNS void
%% AS $$
%% BEGIN
%%     BEGIN
%%         EXECUTE _InsertStatement;
%%     EXCEPTION
%%         WHEN unique_violation THEN
%%             EXECUTE _UpdateStatement;
%%     END;
%%     RETURN _Result;
%% END
%% $$
%% LANGUAGE plpgsql;
%% 
%% CREATE OR REPLACE
%% FUNCTION update_helper(_UpdateStatement TEXT, _InsertStatement)
%%   RETURNS void
%% AS $$
%% BEGIN
%%     BEGIN
%%         EXECUTE _UpdateStatement
%%           INTO _Result;
%%     EXCEPTION
%%         WHEN NO_DATA_FOUND THEN
%%             EXECUTE _UpdateStatement;
%%     END;
%%     RETURN _Result;
%% END
%% $$
%% LANGUAGE plpgsql;


