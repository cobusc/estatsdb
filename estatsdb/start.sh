#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -config rel/files/sys.config -boot start_sasl -s reloader -s estatsdb -name estatsdb@127.0.0.1
