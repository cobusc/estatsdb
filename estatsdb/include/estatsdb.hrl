
-record(schema_column, 
{ 
    name :: binary(), 
    type :: binary(), 
    is_pk :: boolean()
}). 
 
-record(table_info, 
{ 
    columns :: list(#schema_column{}) 
}). 

-record(request_column,
{
    name :: binary(),
    value :: any(),
    is_pk :: boolean()
}).

-record(request_info,
{
    table_name :: string(),
    table_info :: #table_info{},
    columns :: list(#request_column{})
}).
