NAME=etodo
lockfile=/var/lock/subsys/$NAME
binpath=/usr/lib/eTodo/bin
startcommand="$binpath/eTodo start"

#!/bin/sh
check_pid_status() {
    pid=$(ps ax | grep beam | grep "$NAME" | grep "\-progname" | grep -v grep | awk '{print $1}')
    if [ "$pid" = "" ]; then
        # prog not running?
        return 1
    else
        # running
        return 0
    fi
}

for n in $(seq 1 120); do
    sleep 1
    check_pid_status
    RETVAL=$?
    if [ $RETVAL -eq 1 ]; then
        break
    fi
done

# Wait for lock file to be removed when doing a controlled shuting down
sleep 1

[ -f $lockfile ] && sh -c "$startcommand"