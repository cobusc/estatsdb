estatsdb
========

Erlang server for collecting stats in a PostgreSQL DB

WORK IN PROGRESS
================

Usage
-----

Create a table for you application statistics:
```sql
CREATE TABLE myapp.daily_stats
(
    day DATE,
    host TEXT,
    metric1 INTEGER,
    metric2 FLOAT
    metric3 BIGINT,
    PRIMARY KEY (day, host)
);
```

Refresh the _schema\_server_:
```
scema_server:refresh().
```

To *set* values, call:
```
http://localhost:8000/set?tablename=myapp.daily_stats&day=2013-01-01&host=testhost&metric1=100
```

This will return something like:
```json
{
    "tablename": "myapp.daily_stats",
    "day": "2013-01-01",
    "host": "testhost",
    "metric1": 100,
    "metric2": nil,
    "metric3": nil
}
```

Note that the table name and primary key values *must* always be specified. At least *one* metric value is required.
If a value already existed, it is replaced.

To *update* values, call:

```
http://localhost:8000/update?tablename=myapp.daily_stats&day=2013-01-01&host=testhost&metric1=50
```

This will return something like:
```json
{
    "tablename": "myapp.daily_stats",
    "day": "2013-01-01",
    "host": "testhost",
    "metric1": 150,
    "metric2": nil,
    "metric3": nil
}
```

As for the *set* function, the table name and primary key values *must* always be specified. At least *one* metric value is required.
If the value to update was not set before, the new value is set. Otherwise, the existing value is incremented with the specified value.


