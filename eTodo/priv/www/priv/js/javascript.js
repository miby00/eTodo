function cancelBtn() {
    event.cancelBubble = true;
    location.reload(true);
}

function saveTaskChanges(uid) {
    event.cancelBubble = true;
    saveChanges('Comment',      uid);
    saveChanges('Description',  uid);
    saveChanges('Estimate(h)',  uid);
    saveChanges('Progress(%)',  uid);
    saveChanges('Remaining(h)', uid);
}

function deleteTask(uid)
{
    event.cancelBubble = true;
    var Element = document.getElementById('table' + uid);
    addClass(Element.childNodes[0].childNodes[8], 'hideRow');
    if (Element.childNodes[0].childNodes[9].classList != undefined) {
        Element.childNodes[0].childNodes[9].classList.remove('hideRow');
    }
}

function deleteYes(uid)
{
    var taskList = document.getElementById('taskSelect').value;
    event.cancelBubble = true;
    var Element = document.getElementById('table' + uid);
    addClass(Element.childNodes[0].childNodes[9], 'hideRow');
    Element.childNodes[0].childNodes[8].classList.remove('hideRow');
    var AJAX = newAJAX();
    sendCall(AJAX, '/eTodo/eWeb:deleteTask?uid=' + uid,
             function () {
                 if (AJAX.readyState == 4 || AJAX.readyState == "complete") {
                     location.reload(true);
                 }
             });
}

function deleteNo(uid)
{
    event.cancelBubble = true;
    var Element = document.getElementById('table' + uid);
    addClass(Element.childNodes[0].childNodes[9], 'hideRow');
    Element.childNodes[0].childNodes[8].classList.remove('hideRow');
}

function openLink(url)
{
    window.open(url, '_self');
    /* window.open(url, '_blank') */
    /* window.focus(); */
}

function sendCall(AJAX, url, callback)
{
    if (AJAX == null) {
        alert("Initialisation of AJAX failed.");
        return false;
    }

    AJAX.onreadystatechange = callback;

    AJAX.open("GET", url, true);
    AJAX.send(null);
}

function saveChanges(type, uid) {
    var Element = document.getElementById(type + uid);
    if ((type == 'Description') &&
        document.getElementById('compactDesc' + uid)) {
        var CompactDesc = document.getElementById('compactDesc' + uid);
        if (CompactDesc.innerHTML != Element.innerHTML) {
            CompactDesc.innerHTML = Element.innerHTML;
        }
        else {
            return
        }
    }

    if (type == 'Progress(%)') {
        var Progress = Element.innerHTML;
        if (!(Progress >= 0 && Progress <= 100) || !(isInt(Progress))) {
            return
        }
    }

    if (type == 'Estimate(%)' || type == 'Remaining(h)') {
        var FieldValue = Element.innerHTML;
        if (!isInt(FieldValue)) {
            return
        }
    }

    var AJAX = newAJAX();
    if (AJAX == null)
    {
        alert("Initialisation of AJAX failed.");
        return false;
    }
    var Data = Element.innerHTML;
    var url  = '/eTodo/eWeb:sendFieldChange?field=' + type +
        '&value=' + encodeURIComponent(Data) + '&uid=' + uid;
    AJAX.open("GET", url, true);
    AJAX.send(null);
}

function isInt(value) {
    var x;
    if (isNaN(value)) {
        return false;
    }
    x = parseFloat(value);
    return (x | 0) === x;
}

function showDetails(uid) {
    var Element = document.getElementById('table' + uid);
    if (Element.classList.contains('tCompact')) {
        Element.classList.remove('tCompact');
        Element.childNodes[0].childNodes[1].classList.remove('hideRow');
        Element.childNodes[0].childNodes[2].classList.remove('hideRow');
        Element.childNodes[0].childNodes[3].classList.remove('hideRow');
        Element.childNodes[0].childNodes[4].classList.remove('hideRow');
        Element.childNodes[0].childNodes[5].classList.remove('hideRow');
        Element.childNodes[0].childNodes[6].classList.remove('hideRow');
        Element.childNodes[0].childNodes[7].classList.remove('hideRow');
        Element.childNodes[0].childNodes[8].classList.remove('hideRow');
        if (Element.classList.contains('ttCompact')) {
            addClass(Element.childNodes[0].childNodes[0], 'hideRow');
        }
    }
    else {
        Element.classList.add('tCompact');
        addClass(Element.childNodes[0].childNodes[2], 'hideRow');
        addClass(Element.childNodes[0].childNodes[3], 'hideRow');
        addClass(Element.childNodes[0].childNodes[4], 'hideRow');
        addClass(Element.childNodes[0].childNodes[5], 'hideRow');
        addClass(Element.childNodes[0].childNodes[7], 'hideRow');
        addClass(Element.childNodes[0].childNodes[8], 'hideRow');
        if (Element.classList.contains('ttCompact')) {
            Element.childNodes[0].childNodes[0].classList.remove('hideRow');
            addClass(Element.childNodes[0].childNodes[1], 'hideRow');
            addClass(Element.childNodes[0].childNodes[6], 'hideRow');
        }
    }
}

function submitForm(value) {
    console.log(value);
    var FormElement = document.getElementById(value);
    FormElement.submit();
}

function sendSetting(Id)
{
    var AJAX = newAJAX();
    if (AJAX == null)
    {
        alert("Initialisation of AJAX failed.");
        return false;
    }
    var Data = document.getElementById(Id).value;
    var url  = '/eTodo/eWeb:sendSetting?key=' + Id + '&value=' + Data;
    AJAX.open("GET", url, true);
    AJAX.send(null);
}

function checkForEnter (event) {
    var keyCode = ('which' in event) ? event.which : event.keyCode;

    if (keyCode == '13') {
        sendMsg('userSelect', 'sendMessage')
    }
    return true;
}

function newAJAX()
{
    var AJAX = null;
    if (window.XMLHttpRequest)
    {
        AJAX=new XMLHttpRequest();
    } else {
        AJAX=new ActiveXObject("Microsoft.XMLHTTP");
    }
    return AJAX;
}


function sendStatus(Id, Uid)
{
    var Data = document.getElementById(Id).value;

    if (Id == 'idStatus' + Uid) {
        document.getElementById('idStatusc' + Uid).value = Data;
    } else {
        document.getElementById('idStatus' + Uid).value = Data;
    }

    setTextDecoration(Data, Uid);

    var AJAX = newAJAX();
    if (AJAX == null)
    {
        alert("Initialisation of AJAX failed.");
        return false;
    }
    var url= '/eTodo/eWeb:sendStatus?status=' + Data + '&uid=' + Uid;
    AJAX.open("GET", url, true);
    AJAX.send(null);
}

function setTextDecoration(Data, Uid)
{
    if (Data == 'Done') {
        addClass(document.getElementById('compactDesc' + Uid), 'done');
        addClass(document.getElementById('Comment' + Uid),     'done');
        addClass(document.getElementById('Description' + Uid), 'done');
    } else  {
        document.getElementById('compactDesc' + Uid).classList.remove('done');
        document.getElementById('Comment' + Uid).classList.remove('done');
        document.getElementById('Description' + Uid).classList.remove('done');
    }
}
function sendPriority(Id, Uid)
{
    var AJAX = newAJAX();
    if (AJAX == null)
    {
        alert("Initialisation of AJAX failed.");
        return false;
    }
    var Data = document.getElementById(Id).value;
    var url= '/eTodo/eWeb:sendPriority?priority=' + Data + '&uid=' + Uid;
    AJAX.open("GET", url, true);
    AJAX.send(null);
}

function sendMsg(ToId, MsgId)
{
    var Msg = encodeURIComponent(document.getElementById(MsgId).value);
    if (Msg != '') {
        var AJAX = newAJAX();
        if (AJAX == null) {
            alert("Initialisation of AJAX failed.");
            return false;
        }

        document.getElementById(MsgId).value = '';
        var To  = encodeURIComponent(document.getElementById(ToId).value);
        var url = '/eTodo/eWeb:sendMsg?to=' + To + '&msg=' + Msg;
        AJAX.open("GET", url, true);
        AJAX.send(null);
    }
}

function checkForMessage()
{
    var AJAX = newAJAX();
    if (AJAX == null)
    {
        alert("Initialisation of AJAX failed.");
        return false;
    }

    AJAX.onreadystatechange = function() {
        if (AJAX.readyState == 4 || AJAX.readyState == "complete") {
            handleResult(AJAX.responseText, AJAX.status);
        }
    };

    if (Notification.permission == "default") {
        Notification.requestPermission(function() {});
    };

    var url= '/eTodo/eWeb:checkForMessage';
    AJAX.open("GET", url, true);
    AJAX.send(null);
}

function handleResult(responseText, status)
{
    if (status == 200 && responseText != "noMessages") {
        document.getElementById('messageField').innerHTML = responseText;
        notifyUser();
    }
    setTimeout('checkForMessage()', 0);
}

function checkStatus() {
    var AJAX = newAJAX();
    if (AJAX == null) {
        alert("Initialisation of AJAX failed.");
        return false;
    }

    AJAX.onreadystatechange = function () {
        if (AJAX.readyState == 4 || AJAX.readyState == "complete") {
            handleStatusResult(AJAX.responseText, AJAX.status);
        }
    };
    var url = '/eTodo/eWeb:checkStatus';
    AJAX.open("GET", url, true);
    AJAX.send(null);
    setTimeout('checkStatus()', 1000);
}

function enableButton(id1, id2) {
    var val = document.getElementById(id1).value;
    document.getElementById(id2).disabled = (val == '');
}

function handleStatusResult(responseText, status)
{
    if (status == 200) {
        var obj = JSON.parse(responseText);
        var seconds = parseInt(obj.timer);

        document.getElementById('timerField').innerHTML = toHoursMinSec(seconds);
        document.getElementById('timerStatus').innerHTML = obj.status;
        document.getElementById('timerStatusMsg').innerHTML = obj.statusMsg;

        var element = document.getElementById('theBody');

        if (seconds == 0) {
            element.classList.remove('timerBusy');
            addClass(element, 'timerAvailable');
        }
        else {
            element.classList.remove('timerAvailable');
            addClass(element, 'timerBusy');
        }

        if (obj.status == 'Available') {
            element.classList.remove('statusAway');
            element.classList.remove('statusBusy');
            addClass(element, 'statusAvailable');
        }
        else if (obj.status == 'Away') {
            element.classList.remove('statusAvailable');
            element.classList.remove('statusBusy');
            addClass(element, 'statusAway');
        }
        else
        {
            element.classList.remove('statusAvailable');
            element.classList.remove('statusAway');
            addClass(element, 'statusBusy');
        }
    }
}

function addClass(element, elementClass) {
    if (element.classList.contains(elementClass) == false) {
        element.classList.add(elementClass);
    }
}

function toHoursMinSec(seconds) {
    var hour = parseInt(seconds / 3600);
    var min  = parseInt(seconds / 60 - hour * 60);
    var sec  = seconds % 60;

    sec = sec < 10 ? ("0" + sec) : sec;

    if (hour == 0) {
        return min + ":" + sec;
    }
    else {
        min = min < 10 ? ("0" + min) : min;
        return hour + ":" + min + ":" + sec;
    }
}

function notifyUser() {
    var Element = document.getElementById('messageField');
    var msgType = Element.childNodes[0].getAttribute('class');
    if ((msgType == 'msgReceived') || (msgType == 'msgAlarm')) {
        if ('Notification' in window) {
            var msgRec =
                Element.
                    childNodes[0].
                    childNodes[0].
                    childNodes[0].
                    childNodes[2].textContent;

            var msgText =
                Element.
                    childNodes[0].
                    childNodes[0].
                    childNodes[1].
                    childNodes[2].textContent;

            var title = (msgType == 'msgReceived') ?
            'eTodo: ' + msgRec.split(' to ')[0]:'eTodo: alarm';

            var icon = (osType() == "Windows") ?
                '/priv/Icons/etodoBig.png':'/priv/Icons/etodoBig.png';

            var options = {
                body: msgText,
                tag: 'preset',
                icon: icon
            }
        }
        if (Notification.permission == "default") {
            Notification.requestPermission(function() {
		        var notification = new Notification(title, options);
	        });
        }
        else {
            if (Notification.permission == "granted") {
                var notification = new Notification(title, options);
            }
        }

        window.navigator = window.navigator || {};
        if (navigator.vibrate != undefined) {
            // Vibrate mobile device
            navigator.vibrate(1000);
        }
    }
}

function osType() {
    // This script sets OSName variable as follows:
    // "Windows"    for all versions of Windows
    // "MacOS"      for all versions of Macintosh OS
    // "Linux"      for all versions of Linux
    // "UNIX"       for all other UNIX flavors
    // "Unknown OS" indicates failure to detect the OS

    var OSName="Unknown OS";
    if (navigator.appVersion.indexOf("Win")!=-1) OSName="Windows";
    if (navigator.appVersion.indexOf("Mac")!=-1) OSName="MacOS";
    if (navigator.appVersion.indexOf("X11")!=-1) OSName="UNIX";
    if (navigator.appVersion.indexOf("Linux")!=-1) OSName="Linux";

    return OSName;
}
