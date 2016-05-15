function navChange(list){
    eTodoDB.setCurrentList(list);
    eTodoDB.updateTodoList(list);
    current = undefined;

}
var current = 0;
var dialogInit = false;
function saveSelect(Uid){
    current = Uid;
    console.log("Here Uid = "+current)
    //alert(eTodoDB.getTodo(Uid).Description);
}

function collapse(id){
    if (eTodoDB.getExpState(id)) {
        var list = eTodoDB.getTodoList(id);
        console.log("collapse - len: "+list.length);
        var i;
        for (i = 0; i < list.length; i++) {
            console.log("Printing: " + list[i].Uid + " " + list[i].Description);
            view.removeTodo(list[i]);
            collapse(list[i].Uid);
        }
        eTodoDB.setExpState(id, false);

       // var i;
       // for(i=0;i<10;i++)
    }
}
function expList(id){
    current = id;
    expandList();
}
function expandList() {
    console.log("current="+current);
    if (eTodoDB.getExpState(current)) {
        var list = eTodoDB.getTodoList(current);
        var i;
        for (i = 0; i < list.length; i++) {
            console.log("Printing: " + list[i].Uid + " " + list[i].Description);
            view.removeTodo(list[i]);
            collapse(list[i].Uid);
        }
        eTodoDB.setExpState(current, false);

    }
    else {
        var list = eTodoDB.getTodoList(current);
        if(list == undefined){
            return;
        }
        var i;
        for (i = 0; i < list.length; i++) {
            console.log("Printing: " + list[i].Uid + " " + list[i].Description);
            view.insertTodo(current, list[i]);
        }
        eTodoDB.setExpState(current, true);
    }
    view.refreshExpIcon(current);
    view.refreshList();
}

function refreshList() {
    console.log("Refreshing current="+current);
    if(current == undefined){
        navChange(eTodoDB.getCurrentList());
    }
    if (eTodoDB.getExpState(current)) {
        var list = eTodoDB.getTodoList(current);
        var i;
        for (i = 0; i < list.length; i++) {
            console.log("removing: " + list[i].Uid + " " + list[i].Description);
            view.removeTodo(list[i]);
            collapse(list[i].Uid);
        }
        for (i = 0; i < list.length; i++) {
            console.log("adding: " + list[i].Uid + " " + list[i].Description);
            view.insertTodo(current, list[i]);
        }
        eTodoDB.setExpState(current, true);
        view.refreshList();
    }
    else{
        expList(current)
    }

}


function launchForm()
{
    //$( "#datepicker" ).datepicker();
    if(dialogInit){
        $("#prioLow").prop("checked",false).checkboxradio("refresh");
        $("#prioMedium").prop("checked",false).checkboxradio("refresh");
        $("#prioHigh").prop("checked",false).checkboxradio("refresh");
        $("#statusNa").prop("checked",false).checkboxradio("refresh");
        $("#statusPlan").prop("checked",false).checkboxradio("refresh");
        $("#statusInProgress").prop("checked",false).checkboxradio("refresh");
        $("#statusDone").prop("checked",false).checkboxradio("refresh");

    }

}
function editTodo()
{
    eTodoDB.setAction("edit");

    launchForm();
}

function createSiblingTodo(){
    eTodoDB.setAction("createSiebling");
    launchForm();
}
function createChildTodo(){
    eTodoDB.setAction("createChild");
    launchForm();
}
function createTodo(){
    eTodoDB.setAction("create");
    launchForm();
}
function closeTodo()
{
    $("#popupMenu").popup("close");
}
function fetchLists() {
	console.log("Fetching lists");
	$.getJSON("/eTodo/eWeb:listListsJSON?list=All+tasks&search=&submit=Search", function(data) {
		var lists = [];
		$.each(data, function(key, val) {
			lists.push(val.list)
			console.log(val.list);
		});
		eTodoDB.setLists(lists);
        $.each(data, function(key, val) {
            eTodoDB.fetchTodos(val.list, false);
        });
		view.updateNavBar();
	});
}
var settings;
$(function() {

	fetchLists();
    $( ".selector" ).bind({
        create: function(event, ui) {  concole.log("created"); },
        refresh: function(event, ui) { concole.log("refreshed"); }
    });

	//view.updateNavBar();
    $( document ).delegate("#edit2", "pageinit", function() {
        console.log('A page with an id of "aboutPage" was just created by jQuery Mobile!');
    });

    $(document).on('pageinit pageshow', 'div:jqmData(role="page"), div:jqmData(role="dialog")', function(event){

        if(!dialogInit){
            $('#dueTime').scroller({
                preset: 'date',
                theme: 'jqm',
                display: 'modal',
                mode: 'scroller',
                dateOrder: 'yyyymmdd',
                'dateFormat':'yyyy-mm-dd'
            });
            $('#createTime').scroller({
                preset: 'datetime',
                theme: 'jqm',
                display: 'modal',
                mode: 'scroller',
                dateOrder: 'yyyymmdd',
                'dateFormat':'yyyy-mm-dd'
            });
        }
        dialogInit = true;
        var todo = eTodoDB.getTodo(current);
        console.log("Current item = "+current);
        console.log("action="+ eTodoDB.getAction());

        var status, prio, list;

        if(todo == undefined){
            status = "N/A";
            prio = "N/A";
        }
        else{
            if(todo.Status.length===0){
                status = "N/A";
            }
            else{
                status = todo.Status;
            }
            if(todo.Priority.length===0){
                prio = "N/A";
            }
            else{
                prio = todo.Priority;
            }
        }
        console.log("Status = "+status);
        console.log("Prio = "+prio);
        console.log("Fixing date picker");
        //$( "#pick" ).kendoDatePicker();
        $("#edit").attr("href","edit.html?id="+current);
        if(eTodoDB.getAction() === "edit"){
            $("[name=status]").filter("[value='"+status+"']").attr("checked",true).checkboxradio("refresh");
            $("[name=prio]").filter("[value='"+prio+"']").attr("checked",true).checkboxradio("refresh");
            $("#description").val(eTodoDB.removeBr(todo.Description));
            $("#search").val("apa");
            $("#dueTime").val(todo.DueTime);
            $("#createTime").text(todo.CreateTime);
            $("#progress").val(todo.Progress).slider("refresh");
            $("#owner").text(todo.Owner);
            $("#sharedWith").text(todo.SharedWith);
            $("#uid").val(todo.Uid);
            $("#comment").val(eTodoDB.removeBr(todo.Comment));
        }
        else{
            if(eTodoDB.getAction() === "createSiebling"){
                list = todo.List;
                console.log("Current list == "+list);
            }
            else{
                if(todo == undefined){
                    list= eTodoDB.getCurrentList();
                }
                else{
                    list = todo.Uid;
                }
            }

            $("[name=status]").filter("[value='N/A']").attr("checked",true).checkboxradio("refresh");
            $("[name=prio]").filter("[value='N/A']").prop("checked",true).checkboxradio("refresh");
            $("#description").val("");
            $("#search").val("");
            $("#dueTime").val("");
            $("#createTime").text("");
            $("#progress").val(0).slider("refresh");
            $("#owner").text("");
            $("#sharedWith").text("");
            $("#uid").val("");
            $("#comment").val("");

            $('input[name="list"]').val(list);
        }
    });

});


/*
 {
 "Uid": "103500790",
 "Description": "Köpa sista julklappen",
 "Status": "Planning",
 "Priority": "High",
 "DueTime": "2012-11-28",
 "Comment": "",
 "SharedWith": "tv",
 "CreateTime": "2012-11-09 20:29:04",
 "DoneTime": "",
 "Progress": "5",
 "Owner": "tv",
 "HasSubTodo": true
 }
 */

$.extend({
    getUrlVars: function(){
        var vars = [], hash;
        var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
        for(var i = 0; i < hashes.length; i++)
        {
            hash = hashes[i].split('=');
            vars.push(hash[0]);
            vars[hash[0]] = hash[1];
        }
        return vars;
    },
    getUrlVar: function(name){
        return $.getUrlVars()[name];
    }
});
