var view = {
    type:"macintosh",
    updateNavBar:function () {
        console.log("Nu kör vi");
        this.addNavBarItem("+");
        eTodoDB.getLists().forEach(this.addNavBarItem);
        $("#navbar").navbar();
    },
    addNavBarItem:function (name) {
        console.log("Adding: " + name);
        var navbarItem;

        if(name === "+"){
            navbarItem = "<li><a href=\"#edit2\" id=\"create\" onclick=\"createChildTodo()\">+</a></li>";
        }
        else{
            navbarItem = "<li><a href='#' onClick='navChange(\"" + name + "\")'>" + name + "</a></li>";
        }

        $("#apan2").append(navbarItem);
    },
    logList:function (todo) {
        console.log("Uid: " + todo.Uid + " Descr: " + todo.Description);
    },

    drawTodoList:function (list) {
        console.log("Nu kör vi - updating list");
        var i;

        $("#xyz").html("");
        for (i = 0; i < list.length; i++) {
            view.logList(list[i]);

            var str = view.generateRowHtml(list[i],"d");
            $("#xyz").prepend(str);
        }


        $("#xyz").listview("refresh");

    },
    refreshExpIcon:function(id){
        //$("#"+id+"_exp").html("<img src=\"/images/retract.png\" class=\"ui-li-icon\">");
        if(eTodoDB.getExpState(id)){
            console.log("Icon set to retract: "+id+"_exp");
            $("#"+id+"_exp").attr("src","/images/retract.png");
        }
        else{
            console.log("Icon set to expand: "+id+"_exp");
            $("#"+id+"_exp").attr("src","/images/expand.png");
        }
        console.log("Icon replaced: "+id+"_exp");
    },
    generateRowHtmlOrg:function(todo, theme){
        var str = "<li data-theme=\""+theme+"\" id=\"" + todo.Uid + "\"><a href=\"#\"  onClick=\"expList('"+todo.Uid+"')\">";
        if (todo.HasSubTodo) {
            str = str + "<img src=\"/images/expand.png\" class=\"ui-li-icon\" id=\"" + todo.Uid + "_exp\">";
        } else {
            if (todo.Status == "Done") {
                img = "/images/checkbox_checked.png";
            } else {
                img = "/images/checkbox_unchecked.png";
            }
        }
        str = str + todo.Description + "<span class=\"ui-li-count\">4</span></a><a href=\"#popupMenu\" data-rel=\"popup\" data-position-to=\"origin\" onClick=\"saveSelect(" + todo.Uid + ")\" data-transition=\"pop\">Expand</a></li>";
        return str;
    },
    generateRowHtml:function(todo, theme,parentId){
        var img = "/images/expand.png";

        var state = 0;

        if (todo.HasSubTodo) {
            img = "/images/expand.png";
        } else {
            if (todo.Status == "Done") {
                img = "/images/checkbox_checked.png";
            } else {
                img = "/images/checkbox_unchecked.png";
            }
        }
        //expanded +2
        state = state + 0;
        if(todo.Status == "Done"){
            descr = "done";
        }

        var todoRow = kendo.template($("#todoRow").html());
        var descr = "";
        //eTodoDB.fetchTodos(todo.Uid,false);
        console.log("Fetching children for: "+todo.Uid)
        var numChildren = eTodoDB.getTodoList(todo.Uid);
        if(numChildren == undefined){
            numChildren = "-";
        }
        else{
            numChildren = numChildren.length;
        }

        var str = todoRow({
            theme: theme,
            todoId: todo.Uid,
            image:img,
            description: todo.Description,
            url: "#",
            popupMenu: "#popupMenu",
            descrClass: descr,
            subTodos: numChildren,
            parent: parentId
        });
        return str;
    },
    /*$.each(data, function(key, val) {
     //<li><a href="#popupBasic" data-rel="popup" data-theme="a"><img src="/images/expand.png" alt="France" class="ui-li-icon">I'm just a div with bar classes and a mini inline<span class="ui-li-count">4</span></a></li>
     items.push('<li id="' + val.Uid + '">' + val.Description + '</li>');
     });*/
    insertTodo:function (prevElem, todo) {
        var oldTheme=$("#"+prevElem).attr("data-theme");
        var theme = oldTheme;
        console.log("Theme = "+theme);

        if(theme == "e"){
            theme="a"
        }
        if(theme == "d"){
            theme="e"
        }
        if(theme == "c"){
            theme="d"
        }
        if(theme == "b"){
            theme="c"
        }
        if(theme == "a"){
            theme="b"
        }
        if(theme === 'undefined'){
            console.log("UNDEF");
            theme="c";
        }
        var str = view.generateRowHtml(todo,theme);
        $("#"+prevElem).after(str);
    },
    removeTodo:function (todo) {
        $("#"+todo.Uid).html("");
    },
    refreshList:function () {
        $("#xyz").listview("refresh");
    }
}