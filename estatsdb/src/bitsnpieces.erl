-module(bitsnpieces).
-export([]).

-spec get_tables(Schemas::list(string())) -> list(string()).

get_tables(Schemas) ->
    % SQL used to select tables in a particular schema
    SelectSql = "SELECT $1::text||'.'||table_name AS table_name
                   FROM information_schema.tables 
                  WHERE table_schema=$1",

    % Function to get tables for a particular schema
    GetTables = fun(Schema, Acc) ->
        case dbutils:equery(SelectSql, [Schema]) of
            {ok, _Cols, Rows} ->
                [lists:map(fun ({X}) -> binary_to_list(X) end, Rows) | Acc]
        end
    end,

    lists:foldl(GetTables, [], Schemas).


describe_table(TableName) ->
    % Tables must have a primary key for this to work...which is a good thing.
    % Returns column name, data type, is_pk
    Sql = "SELECT pg_attribute.attname AS column_name,
                  FORMAT_TYPE(pg_attribute.atttypid, pg_attribute.atttypmod) AS format_type,
                  pg_attribute.attnum = ANY(pg_index.indkey) AS is_pk
             FROM pg_attribute,
                  pg_index
            WHERE pg_attribute.attrelid = $1::regclass
              AND pg_index.indrelid = pg_attribute.attrelid
              AND pg_index.indisprimary
              AND NOT pg_attribute.attislocal",

    case dbutils:equery(Sql, [TableName]) of
        {ok, Cols, Rows} ->
    
    end,



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


A "set" operation will do the following:

INSERT INTO spm.hourly_stats (ts,host,a)
VALUES ('2013-01-01', 'aew1', '1');
...if DUPLICATE_KEY then...
UPDATE spm.hourly_stats
   SET a = 1
 WHERE ts = '2013-01-01'
   AND host = 'aew1';

An "update" operation will do the following:

UPDATE spm.hourly_stats
   SET a = a + 1
 WHERE ts = '2013-01-01'
   AND host = 'aew1';
...if 0 rows updated then...
INSERT INTO spm.hourly_stats (ts,host,a)
VALUES ('2013-01-01', 'aew1', 1);








