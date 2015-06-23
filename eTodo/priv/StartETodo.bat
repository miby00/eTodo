cd ..
cd ..
cd ePort\src
erl -make
cd ..
cd ..
cd eLog\src
erl -make
cd ..
cd ..
cd eTodo\src
erl -make
cd ..
cd ebin
erl -boot start_sasl -smp -pa ../../eLog/ebin -pa ../../ePort/ebin -pa ../../eTodo/ebin -mnesia dir "\"%SystemDrive%/Users/%USERNAME%/AppData/Roaming/eTodo\"" -eLog logDir "\"%SystemDrive%/Users/%USERNAME%/AppData/Roaming/eTodo/logs/eLog\"" -run startETodo gui

