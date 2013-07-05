[![Build Status](https://www.travis-ci.org/cobusc/estatsdb.png?branch=master)](https://www.travis-ci.org/cobusc/estatsdb)

estatsdb
========

Erlang server for collecting stats in a PostgreSQL DB

Usage
-----

Create a table for you application statistics. It can exist in its own schema.
```sql
CREATE SCHEMA myapp;
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
schema_server:refresh().
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
    "metric2": null,
    "metric3": null
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
    "metric2": null,
    "metric3": null
}
```

As for the *set* function, the table name and primary key values *must* always be specified. At least *one* metric value is required.
If the value to update was not set before, the new value is set. Otherwise, the existing value is incremented with the specified value.


