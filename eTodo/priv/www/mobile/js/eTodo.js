var Panel = kendo.Class.extend({
	tabId : 'Not Set',
	panelDivId : 'Not Set',
	isExpanded : false,
	bkgExp : '',
	bkgRet : '',

	expand : function() {
		console.log("expand")
		$(this.panelDivId).slideDown();
		$(this.tabId).css("background", "url(" + this.bkgExp + ")");
		this.isExpanded = true;
	},
	retract : function() {
		console.log("retract")
		$(this.panelDivId).slideUp();
		$(this.tabId).css("background", "url(" + this.bkgRet + ")");
		this.isExpanded = false;
	},
	go : function() {

		if (this.isExpanded) {
			this.retract();
		} else {
			this.expand();
		}
	},
	init : function(tab, panel, backgroundExp, backgroundRet) {
		this.tabId = tab;
		this.panelDivId = panel;
		this.bkgExp = backgroundExp;
		this.bkgRet = backgroundRet;
		console.log("apa:" + this.tabId);
		//$("#settingsTab").click(this.go());
		console.log("setting ... " + this.tabId);
		//$(this.tabId).click(this.go());
		console.log("done setting click");
	}
});

function fetchTodos() {
	$.getJSON("/eTodo/eWeb:indexJSON?list=All+tasks&search=&submit=Search", function(data) {
		var items = [];

		$.each(data, function(key, val) {
			items.push('<li id="' + val.Uid + '">' + val.Description + '</li>');
		});

		$('<ul/>', {
			'id' : 'todoList',
			html : items.join('')
		}).appendTo('#todos');

	});
}
var items = [];

function fetchLists() {
	console.log("Fetching lists");
	$.getJSON("/eTodo/eWeb:listListsJSON?list=All+tasks&search=&submit=Search", function(data) {

		$.each(data, function(key, val) {
			items.push(val.list);
			//console.log(val.list);
			if(val.list != "Alla tasks"){
				appendTreeview(val.list,false);
			}
			
			
		});
		function onSelect(e) {
			console.log("Selected: " + $(e.item).find("> .k-link").text());
			//$(e.contentElement).;

		}

		function onActivate(e) {
			console.log("Activated: " + $(e.item).find("> .k-link").text());
		}

		function onContentLoad(e) {
			console.log("Content loaded in <b>" + $(e.item).find("> .k-link").text() + "</b> and starts with <b>" + $(e.contentElement).text().substr(0, 20) + "...</b>");
		}

		function onError(e) {
			console.error("Loading failed with " + e.xhr.statusText + " " + e.xhr.status);
		}
		$("#tabstrip").kendoTabStrip({
			select : onSelect,
			activate : onActivate,
			contentLoad : onContentLoad,
			error : onError
		});
	});
}
var settings;
$(function() {
	settings = new Panel("#settingsTab", "#settings", "/images/tab-down.png", "/images/tab-up.png");
	$("#settingsTab").click(function() {
		settings.go();
	});
	//fetchTodos();
	
});

function listName2Id(name){
	var id;
	id = name.replace(/ /g, '_');
	return id;
}

function appendTreeview(listName, active) {
	console.log("appendTreeview "+listName+" "+active)
	var l_url = "/eTodo/eWeb:listTodosJSON?search=&submit=Search&list="
		+ encodeURIComponent(listName).replace(/%20/g, '+');
	console.log("Data url: "+l_url);
	var homogeneous2 = new kendo.data.HierarchicalDataSource({
		transport : {
			read : {
				url : l_url,
				dataType : "json"
			},

			parameterMap : function(options, type) {
				return {
					list : options.Uid,
				}
			}

		},
		schema : {
			model : {
				id : "Uid",
				hasChildren : "HasSubTodo"
			}
		}
	});

	if (active) {
		$("#tabs").append("<li class='k-state-active'>" + listName + "</li>");
	} else {
		$("#tabs").append("<li>" + listName + "</li>");
	}
	$("#tabstrip").append("<div id='" + listName2Id(listName) + "'></div>");

	$("#" + listName2Id(listName) ).append(
			"<div id='" + listName2Id(listName) + "View' class='demo-section'></div>");

	$("#" + listName2Id(listName) + "View").kendoTreeView({
		template : kendo.template($("#treeview-template").html()),
		Checkbox : {
			checkChildren : true
		},
		checkboxTemplate : "<input type='checkbox' />",
		dataSource : homogeneous2,
		dataTextField : "Description"

	});
}