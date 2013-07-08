
-record(column, 
{ 
    name :: binary(), 
    type :: binary(),
    value :: binary(),
    is_pk = false :: boolean()
}). 
 
-record(table_info, 
{
    table_name :: binary(),
    as_sql :: iodata(),
    returning_sql :: iodata(), 
    columns :: list(#column{}) 
}). 

-record(request_info,
{
    table_info :: #table_info{},
    columns :: list(#column{})
}).

-define(ERROR_NO_TABLE, "The tablename argument was not specified.").
-define(ERROR_UNKNOWN_TABLE, "Table specified table is not known to estatsdb. Maybe refresh the schema server?").
-define(ERROR_MISSING_PK, "Some primary key column is missing").
-define(ERROR_UNKNOWN_COLUMN, "Unknown column(s) specified.").
-define(ERROR_NO_METRIC, "At least one metric column needs to be specified.").


