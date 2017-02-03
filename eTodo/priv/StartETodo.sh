#!/bin/sh
cd "$(dirname "$0")"
cd ..
cd ..
cd ePort/src
erl -make
cd ..
cd ..
cd eLog/src
erl -make
cd ..
cd ..
cd eTodo/src
export "ERL_LIBS=../.."; erl -make
cd ..
cd ebin
erl -boot start_sasl -smp -pa ../../jsx/ebin -pa ../../eLog/ebin -pa ../../ePort/ebin -pa ../../eTodo/ebin -mnesia dir "\"$HOME/.eTodo\"" -eLog logDir "\"$HOME/.eTodo/logs/eLog\"" -run startETodo gui

