#!/bin/sh
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
erl -make
cd ..
cd ebin
erl -boot start_sasl -smp -pa ../../eLog/ebin -pa ../../ePort/ebin -pa ../../eTodo/ebin -mnesia dir "\"$HOME/.eTodo\"" -eLog logDir "\"$HOME/.eTodo/logs/eLog\"" -noshell -detached -run startETodo noGui $@



