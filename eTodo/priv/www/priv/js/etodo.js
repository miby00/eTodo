var eTodoData = {
    users: [],
    setUsers: function (_users) {
        this.users = _users;
    },
    getUsers: function () {
        return this.users;
    },
    fetchFromSrv: function () {
        "use strict";
        var url  = '/eTodo/eWeb:getUsersJSON',
            AJAX = eTodo.newAJAX();

        eTodo.sendCall(AJAX, url, function () {
            if (AJAX.readyState === 4 || AJAX.readyState === "complete") {
                var obj = JSON.parse(AJAX.responseText);

                eTodoData.setUsers(obj.Users);
            }
        });
    }
};

var eTodo;

(function (eTodo) {
    function isInt(value) {
        "use strict";
        var x;

        if (isNaN(value)) {
            return false;
        }
        x = parseFloat(value);
        return (x | 0) === x;
    }

    function newAJAX() {
        "use strict";
        var AJAX = null;

        if (window.XMLHttpRequest) {
            AJAX = new XMLHttpRequest();
        } else {
            AJAX = new ActiveXObject("Microsoft.XMLHTTP");
        }
        return AJAX;
    }
    eTodo.newAJAX = newAJAX;

    function sendCall(AJAX, url, callback) {
        "use strict";
        if (AJAX === null) {
            alert("Initialisation of AJAX failed.");
            return false;
        }

        if (typeof callback === "function") {
            AJAX.onreadystatechange = callback;
        }

        AJAX.open("GET", url, true);
        AJAX.send(null);
    }
    eTodo.sendCall = sendCall;

    function stopPropagation(evt) {
        "use strict";
        evt = evt || window.event; // For IE

        if (typeof evt.stopPropagation === "function") {
            evt.stopPropagation();
        } else {
            evt.cancelBubble = true;
        }
    }
    eTodo.stopPropagation = stopPropagation;

    function openLink(url) {
        "use strict";
        window.open(url, '_self');
        /* window.open(url, '_blank') */
        /* window.focus(); */
    }
    eTodo.openLink = openLink;

    function addClass(element, elementClass) {
        "use strict";
        if (element.classList.contains(elementClass) === false) {
            element.classList.add(elementClass);
        }
    }

    function saveChanges(type, uid) {
        "use strict";
        var element = document.getElementById(type + uid),
            compactDesc,
            progress,
            fieldValue,
            AJAX,
            data,
            url;

        if ((type === 'Description') &&
            document.getElementById('compactDesc' + uid)) {
            compactDesc = document.getElementById('compactDesc' + uid);
            if (compactDesc.innerHTML !== element.innerHTML) {
                compactDesc.innerHTML = element.innerHTML;
            } else {
                return;
            }
        }

        if (type === 'Progress(%)') {
            progress = parseInt(element.innerHTML);
            if (!(progress >= 0 && progress <= 100) || !(isInt(progress))) {
                return;
            }
        }

        if (type === 'Estimate(%)' || type === 'Remaining(h)') {
            fieldValue = parseInt(element.innerHTML);
            if (!isInt(fieldValue)) {
                return;
            }
        }

        AJAX = newAJAX();
        if (type === 'Due date') {
            data = document.getElementById('date_' + uid).value;
        } else {
            data = element.innerHTML;
        }
        url  = '/eTodo/eWeb:sendFieldChange?field=' + type +
            '&value=' + encodeURIComponent(data) + '&uid=' + uid;

        sendCall(AJAX, url);
    }

    function cancelBtn(event) {
        "use strict";
        stopPropagation(event);

        location.reload(true);
    }
    eTodo.cancelBtn = cancelBtn;

    function saveTaskChanges(event, uid) {
        "use strict";
        stopPropagation(event);

        saveChanges('Comment',      uid);
        saveChanges('Description',  uid);
        saveChanges('Estimate(h)',  uid);
        saveChanges('Progress(%)',  uid);
        saveChanges('Remaining(h)', uid);
        saveChanges('Due date',     uid);
    }
    eTodo.saveTaskChanges = saveTaskChanges;

    function deleteList(event) {
        "use strict";
        stopPropagation(event);
        var element1 = document.getElementById('dListRow'),
            element2 = document.getElementById('yesNoDList');

        addClass(element1, 'hide');
        element2.classList.remove('hide');
    }
    eTodo.deleteList = deleteList;

    function deleteListYes(event, id) {
        "use strict";
        stopPropagation(event);
        var list = document.getElementById(id).value,
            AJAX = newAJAX();

        sendCall(AJAX, '/eTodo/eWeb:deleteTaskList?dlist=' + encodeURIComponent(list),
            function () {
                if (AJAX.readyState === 4 || AJAX.readyState === "complete") {
                    openLink('/eTodo/eWeb:index');
                }
            });
    }
    eTodo.deleteListYes = deleteListYes;

    function deleteListNo(event, uid) {
        "use strict";
        stopPropagation(event);
        var element1 = document.getElementById('dListRow'),
            element2 = document.getElementById('yesNoDList');

        element1.classList.remove('hide');
        addClass(element2, 'hide');
    }
    eTodo.deleteListNo = deleteListNo;


    function deleteTask(event, uid) {
        "use strict";
        stopPropagation(event);
        var element = document.getElementById('table' + uid);

        addClass(element.childNodes[0].childNodes[8], 'hide');
        if (element.childNodes[0].childNodes[9].classList !== undefined) {
            element.childNodes[0].childNodes[9].classList.remove('hide');
        }
    }
    eTodo.deleteTask = deleteTask;

    function deleteYes(event, uid) {
        "use strict";
        stopPropagation(event);
        var element  = document.getElementById('table' + uid),
            AJAX = newAJAX();

        addClass(element.childNodes[0].childNodes[9], 'hide');
        element.childNodes[0].childNodes[8].classList.remove('hide');
        sendCall(AJAX, '/eTodo/eWeb:deleteTask?uid=' + uid,
            function () {
                if (AJAX.readyState === 4 || AJAX.readyState === "complete") {
                    location.reload(true);
                }
            });
    }
    eTodo.deleteYes = deleteYes;

    function deleteNo(event, uid) {
        "use strict";
        stopPropagation(event);
        var element = document.getElementById('table' + uid);

        addClass(element.childNodes[0].childNodes[9], 'hide');
        element.childNodes[0].childNodes[8].classList.remove('hide');
    }
    eTodo.deleteNo = deleteNo;

    function showDetails(uid) {
        "use strict";
        var element = document.getElementById('table' + uid);
        if (element.classList.contains('tCompact')) {
            element.classList.remove('tCompact');
            element.childNodes[0].childNodes[1].classList.remove('hide');
            element.childNodes[0].childNodes[2].classList.remove('hide');
            element.childNodes[0].childNodes[3].classList.remove('hide');
            element.childNodes[0].childNodes[4].classList.remove('hide');
            element.childNodes[0].childNodes[5].classList.remove('hide');
            element.childNodes[0].childNodes[6].classList.remove('hide');
            element.childNodes[0].childNodes[7].classList.remove('hide');
            element.childNodes[0].childNodes[8].classList.remove('hide');
            fillOwnerSelectBox();
            if (element.classList.contains('ttCompact')) {
                addClass(element.childNodes[0].childNodes[0], 'hide');
            }
        } else {
            element.classList.add('tCompact');
            addClass(element.childNodes[0].childNodes[2], 'hide');
            addClass(element.childNodes[0].childNodes[3], 'hide');
            addClass(element.childNodes[0].childNodes[4], 'hide');
            addClass(element.childNodes[0].childNodes[5], 'hide');
            addClass(element.childNodes[0].childNodes[7], 'hide');
            addClass(element.childNodes[0].childNodes[8], 'hide');
            if (element.classList.contains('ttCompact')) {
                element.childNodes[0].childNodes[0].classList.remove('hide');
                addClass(element.childNodes[0].childNodes[1], 'hide');
                addClass(element.childNodes[0].childNodes[6], 'hide');
            }
        }
    }
    eTodo.showDetails = showDetails;

    function fillOwnerSelectBox() {
        "use strict";

        console.log(eTodoData.getUsers());
    }

    function submitForm(value) {
        "use strict";
        var formElement = document.getElementById(value);

        console.log(value);
        formElement.submit();
    }
    eTodo.submitForm = submitForm;

    function sendSetting(Id) {
        "use strict";
        var AJAX = newAJAX(),
            data = document.getElementById(Id).value,
            url = '/eTodo/eWeb:sendSetting?key=' + Id + '&value=' + data;

        sendCall(AJAX, url);
    }
    eTodo.sendSetting = sendSetting;

    function sendMsg(ToId, MsgId) {
        "use strict";
        var msg  = encodeURIComponent(document.getElementById(MsgId).value),
            AJAX = newAJAX(),
            to,
            url;

        if (msg !== '') {
            document.getElementById(MsgId).value = '';
            to  = encodeURIComponent(document.getElementById(ToId).value);
            url = '/eTodo/eWeb:sendMsg?to=' + to + '&msg=' + msg;
            sendCall(AJAX, url);
        }
    }
    eTodo.sendMsg = sendMsg;

    function checkForEnter(event) {
        "use strict";

        var keyCode = (event.hasOwnProperty('which')) ? event.which : event.keyCode;

        if (keyCode == '13') {
            sendMsg('userSelect', 'sendMessage');
        }
        return true;
    }
    eTodo.checkForEnter = checkForEnter;

    function setTextDecoration(Data, Uid) {
        "use strict";

        if (Data === 'Done') {
            addClass(document.getElementById('compactDesc' + Uid), 'done');
            addClass(document.getElementById('Comment' + Uid),     'done');
            addClass(document.getElementById('Description' + Uid), 'done');
        } else {
            document.getElementById('compactDesc' + Uid).classList.remove('done');
            document.getElementById('Comment' + Uid).classList.remove('done');
            document.getElementById('Description' + Uid).classList.remove('done');
        }
    }

    function sendStatus(Id, Uid) {
        "use strict";
        var data = document.getElementById(Id).value,
            AJAX,
            url;

        if (Id === 'idStatus' + Uid) {
            document.getElementById('idStatusc' + Uid).value = data;
        } else {
            document.getElementById('idStatus' + Uid).value = data;
        }

        setTextDecoration(data, Uid);
        AJAX = newAJAX();
        url = '/eTodo/eWeb:sendStatus?status=' + data + '&uid=' + Uid;
        sendCall(AJAX, url);
    }
    eTodo.sendStatus = sendStatus;

    function sendPriority(Id, Uid) {
        "use strict";
        var AJAX = newAJAX(),
            data = document.getElementById(Id).value,
            url  = '/eTodo/eWeb:sendPriority?priority=' + data + '&uid=' + Uid;
        sendCall(AJAX, url);
    }
    eTodo.sendPriority = sendPriority;

    function notifyUser() {
        "use strict";
        var element = document.getElementById('messageField'),
            msgType = element.childNodes[0].getAttribute('class'),
            msgRec,
            msgText,
            title,
            icon,
            options;

        if ((msgType === 'msgReceived') || (msgType === 'msgAlarm')) {
            if (window.Notification !== undefined) {
                msgRec =
                    element.
                        childNodes[0].
                        childNodes[0].
                        childNodes[0].
                        childNodes[2].textContent;

                msgText =
                    element.
                        childNodes[0].
                        childNodes[0].
                        childNodes[1].
                        childNodes[2].textContent;

                title = (msgType === 'msgReceived') ?
                'eTodo: ' + msgRec.split(' to ')[0] : 'eTodo: alarm';

                icon = '/priv/Icons/etodoBig.png';

                options = {
                    body: msgText,
                    tag: 'preset',
                    icon: icon
                };
            }
            if (Notification.permission === "default") {
                Notification.requestPermission(function () {
                    new Notification(title, options);
                });
            } else {
                if (Notification.permission === "granted") {
                    new Notification(title, options);
                }
            }

            if (window.navigator && window.navigator.vibrate) {
                // Vibrate mobile device
                navigator.vibrate(1000);
            }
        }
    }

    function handleResult(responseText, status) {
        "use strict";
        if (status === 200 && responseText !== "noMessages") {
            document.getElementById('messageField').innerHTML = responseText;
            notifyUser();
        }
        setTimeout(checkForMessage, 1000);
    }

    function checkForMessage() {
        "use strict";
        var AJAX = newAJAX(),
            url  = '/eTodo/eWeb:checkForMessage';

        sendCall(AJAX, url, function () {
            if (AJAX.readyState === 4 || AJAX.readyState === "complete") {
                handleResult(AJAX.responseText, AJAX.status);
            }
        });

        if (Notification.permission === "default") {
            Notification.requestPermission(function () {});
        }
    }
    eTodo.checkForMessage = checkForMessage;

    function toHoursMinSec(seconds) {
        "use strict";
        var hour = parseInt(seconds / 3600, 10),
            min  = parseInt(seconds / 60 - hour * 60, 10),
            sec  = seconds % 60;

        sec = sec < 10 ? ("0" + sec) : sec;

        if (hour === 0) {
            return min + ":" + sec;
        }
        min = min < 10 ? ("0" + min) : min;
        return hour + ":" + min + ":" + sec;
    }

    function handleStatusResult(responseText, status) {
        "use strict";
        var obj     = JSON.parse(responseText),
            seconds = parseInt(obj.timer, 10),
            element;

        if (status === 200) {
            document.getElementById('timerField').innerHTML = toHoursMinSec(seconds);
            document.getElementById('timerStatus').innerHTML = obj.status;
            document.getElementById('timerStatusMsg').innerHTML = obj.statusMsg;

            element = document.getElementById('theBody');

            if (seconds === 0) {
                element.classList.remove('timerBusy');
                addClass(element, 'timerAvailable');
            } else {
                element.classList.remove('timerAvailable');
                addClass(element, 'timerBusy');
            }

            if (obj.status === 'Available') {
                element.classList.remove('statusAway');
                element.classList.remove('statusBusy');
                addClass(element, 'statusAvailable');
            } else if (obj.status === 'Away') {
                element.classList.remove('statusAvailable');
                element.classList.remove('statusBusy');
                addClass(element, 'statusAway');
            } else {
                element.classList.remove('statusAvailable');
                element.classList.remove('statusAway');
                addClass(element, 'statusBusy');
            }
        }
    }

    function checkStatus() {
        "use strict";
        var AJAX = newAJAX(),
            url  = '/eTodo/eWeb:checkStatus';

        sendCall(AJAX, url, function () {
            if (AJAX.readyState === 4 || AJAX.readyState === "complete") {
                handleStatusResult(AJAX.responseText, AJAX.status);
            }
        });
    }
    eTodo.checkStatus = checkStatus;

    function enableButton(id1, id2) {
        "use strict";
        var val = document.getElementById(id1).value;

        document.getElementById(id2).disabled = (val === '');
    }


    function osType() {
        "use strict";
        // "Windows"    for all versions of Windows
        // "MacOS"      for all versions of Macintosh OS
        // "Linux"      for all versions of Linux
        // "UNIX"       for all other UNIX flavors
        // "Unknown OS" indicates failure to detect the OS

        if (navigator.appVersion.indexOf("Win") !== -1) {
            return "Windows";
        }
        if (navigator.appVersion.indexOf("Mac") !== -1) {
            return "MacOS";
        }
        if (navigator.appVersion.indexOf("X11") !== -1) {
            return "UNIX";
        }
        if (navigator.appVersion.indexOf("Linux") !== -1) {
            return "Linux";
        }

        return "Unknown OS";
    }
})(eTodo || (eTodo = {}));


