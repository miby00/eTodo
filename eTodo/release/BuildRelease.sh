#!/usr/bin/env bash
rm -rf eTodo
rm eTodo.zip
cd ..
cd ..
cd eLog\src
erl -make
cd ..
cd ..
cd ePort\src
erl -make
cd ..
cd ..
cd eTodo\src
erl -make
cd ..
cd release
escript.exe makeRelease.esc
