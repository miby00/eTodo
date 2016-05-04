function openInNewTab(url)
{
    window.open(url, '_blank');
    window.focus();
}

function showDetails(uid) {
    var Element = document.getElementById('table' + uid);
    if (Element.classList.contains('tCompact')) {
        Element.classList.remove('tCompact');
        Element.childNodes[0].childNodes[1].classList.remove('hideRow');
        Element.childNodes[0].childNodes[2].classList.remove('hideRow');
        Element.childNodes[0].childNodes[3].classList.remove('hideRow');
        Element.childNodes[0].childNodes[4].classList.remove('hideRow');
        Element.childNodes[0].childNodes[6].classList.remove('hideRow');
    }
    else {
        Element.classList.add('tCompact');
        addClass(Element.childNodes[0].childNodes[1], 'hideRow');
        addClass(Element.childNodes[0].childNodes[2], 'hideRow');
        addClass(Element.childNodes[0].childNodes[3], 'hideRow');
        addClass(Element.childNodes[0].childNodes[4], 'hideRow');
        addClass(Element.childNodes[0].childNodes[6], 'hideRow');
    }
}

function submitForm() {
    var FormElement = document.getElementById('searchForm');
    FormElement.submit();
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
    var AJAX = newAJAX();
    if (AJAX == null)
    {
        alert("Initialisation of AJAX failed.");
        return false;
    }
    var Data = document.getElementById(Id).value;
    var url= '/eTodo/eWeb:sendStatus?status=' + Data + '&uid=' + Uid;
    AJAX.open("GET", url, true);
    AJAX.send(null);
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
