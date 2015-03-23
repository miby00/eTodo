rmdir eTodo /s /q
del eTodo.exe
del eTodo.zip
cd ..
cd ..
svn update eLog
cd eLog\src
erl -make
cd ..
cd ..
svn update ePort
cd ePort\src
erl -make
cd ..
cd ..
svn update eTodo
cd eTodo\src
erl -make
cd ..
cd release
escript.exe makeRelease.esc
start /d "C:\Program Files\NSIS\" NSIS.exe
pause
copy eTodo.exe O:\Arbetskatalog\Utvecklingsverktyg\eTodo\