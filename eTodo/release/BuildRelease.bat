rmdir eTodo /s /q
del eTodo.zip
del eTodo.tar.gz
cd ..
cd ..
set ERL_LIBS=%cd%
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
pause

