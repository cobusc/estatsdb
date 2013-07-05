
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
