var eTodoDB = {
    type: "macintosh",
    lists: [],
    listDB: {},
    todoDB: {},
    setLists: function (_lists) {
        this.lists = _lists;
    },
    getLists: function () {
        return this.lists;
    },
    setAction: function (_action) {
        this.action = _action;
    },
    getAction: function () {
        return this.action;
    },
    setCurrentList: function (_list) {
        this.list = _list;
    },
    getCurrentList: function () {
        return this.list;
    },
    setTodoList: function (listName, list){
      console.log("Savingchildren to: "+listName)
      this.listDB[listName] = list;
    },
    setTodo: function (id, listName, todo){
        console.log("Saving todo: "+id)
        todo.List = listName;
        this.todoDB[id] = todo;
    },
    getTodo: function (id){
        //console.log("Saving todo: "+id)
        return this.todoDB[id];
    },
    setExpState:function(id,state){
        this.todoDB["exp_"+id] = state;
    },
    getExpState:function(id){
        if(this.todoDB["exp_"+id] == undefined){
            return false;
        }
        else{
            return this.todoDB["exp_"+id];
        }
    },
    getTodoList: function (listName){
        return this.listDB[listName];
    },
    setTodoList: function (listName,list){
        this.listDB[listName]=list;
    },
    updateTodoList: function (listName){
        if(this.listDB[listName] == undefined){
            console.log("Not available fetching todos");
            this.fetchTodos(listName,true);
            console.log("After call");
        }
        else{
            view.drawTodoList(this.listDB[listName]);
        }
    },
    fetchTodos: function(listName,draw){
        console.log("Fetching todos: "+listName);
        var l_url = "/eTodo/eWeb:listTodosJSON?search=&submit=Search&list="
            + encodeURIComponent(listName).replace(/%20/g, '+');
        console.log("Fetching: "+l_url);
        //l_url="http://192.168.0.12:8099/eTodo/eWeb:listTodosJSON?search=&submit=Search&list=hemma";
        $.ajaxSetup({ scriptCharset: "ISO-8859-1" , contentType: "application/json; charset=ISO-8859-1"});
        $.getJSON(l_url, function(data) {
            console.log("Got response for listname:" + listName + " - num children="+data.length);
            console.log(data);
            eTodoDB.setTodoList(listName,data);
            var i;
            for(i=0;i<data.length;i++) {
                eTodoDB.setTodo(data[i].Uid, listName, data[i]);
                if( eTodoDB.getExpState(data[i].Uid) == undefined){
                    eTodoDB.setExpState(data[i].Uid, false)
                }
                if(data[i].HasSubTodo){
                    eTodoDB.fetchTodos(data[i].Uid,false);
                }

            }
            if(draw){
                view.drawTodoList(eTodoDB.getTodoList(listName));
            }
            else{
                refreshList();
            }

        }).error(function(xhr, ajaxOptions, thrownError) {
                alert(xhr.status);
                alert(thrownError);
            });
    },
    removeBr: function(str){
        return str.replace(/\<br \/\>/g, '\n');
    }
};

/* attach a submit handler to the form */
function submitSearchForm() {


    //alert("here");

    /* get some values from elements on the page: */
    var $form = $( "#searchForm" ),
        description_val = $( '#description' ).val(),
        uid_val = $( 'input[name="uid"]' ).val(),
        status_val = $( 'input:radio[name="status"]:checked' ).val(),
        priority_val = $( 'input:radio[name="prio"]:checked' ).val(),
        dueTime_val = $( 'input[name="dueTime"]' ).val(),
        progress_val = $( 'input[name="progress"]' ).val(),
        createTime_val = $( 'input[name="createTime"]' ).val(),
        comment_val = $( '#comment' ).val(),
        list_val = $( 'input[name="list"]' ).val(),
        url = "/eTodo/eWeb:saveTodo";

    if(eTodoDB.getAction() === "edit"){
        url = "/eTodo/eWeb:saveTodo"
    }
    else{
        url = "/eTodo/eWeb:createTask"
    }
    //list_val = "jobbet";
    console.log("Saving todo");
    console.log("Descr: "+description_val);
    console.log("Uid: "+uid_val);
    console.log("Status: "+status_val);
    console.log("Prio: "+priority_val);
    console.log("Due: "+dueTime_val);
    console.log("Progr: "+progress_val);
    console.log("Comment: "+comment_val);
    console.log("List: "+list_val);
    /* Send the data using post and put the results in a div */

    if(eTodoDB.getAction()=="edit"){
        $.post( url, { desc: description_val, uid: uid_val, status: status_val, prio: priority_val, dueTime: dueTime_val, progress: progress_val, createTime: createTime_val, comment: comment_val},
            function( data ) {
                console.log("Updating: "+description);
                //$('.ui-dialog').dialog('close');
                var todo = eTodoDB.getTodo(uid_val);
                todo.Description = description_val;
                todo.Status = status_val;
                todo.Priority = priority_val;
                todo.DueTime = dueTime_val;
                todo.Progress = progress_val;
                todo.Comment = comment_val;
                eTodoDB.setTodo(uid_val,list_val,todo);
                $("#"+uid_val+"_descr").text(todo.Description);
                if (!(todo.HasSubTodo)) {
                    if (todo.Status == "Done") {
                        $("#"+uid_val+"_exp").attr("src","/images/checkbox_checked.png");
                    } else {
                        $("#"+uid_val+"_exp").attr("src","/images/checkbox_unchecked.png");
                    }
                }
            });
    }
    else{
        $.post( url, { desc: description_val, list: list_val, uid: uid_val, status: status_val, prio: priority_val, dueTime: dueTime_val, progress: progress_val, createTime: createTime_val, comment: comment_val},
            function( data ) {
                console.log("Saving: "+description_val);
                //$('.ui-dialog').dialog('close');
                var todo = {};
                todo.Uid = uid_val;
                todo.Description = description_val;
                todo.Status = status_val;
                todo.Priority = priority_val;
                todo.DueTime = dueTime_val;
                todo.Progress = progress_val;
                todo.Comment = comment_val;
                eTodoDB.setTodo(uid_val,list_val,todo);
                $("#"+uid_val+"_descr").text(todo.Description);
                //var list = eTodoDB.getTodoList(list_val);
                //list[list.length] = todo;
                //eTodoDB.setTodoList(list_val,list);
                //eTodoDB.updateTodoList(list_val);
                if(eTodoDB.getAction() == "createSibling"){
                    current = list_val;
                }
                eTodoDB.fetchTodos(list_val,false);

            });
        //console.log("Insert todo list: "+list_val+" todo:" +todo.Uid);
        /*view.insertTodo(list_val, todo);
        var old = current;
        current = list_val;
        view.expandList();
        view.expandList();
        current = old;*/
    }
}
