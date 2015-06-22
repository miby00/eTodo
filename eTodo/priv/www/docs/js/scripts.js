var activeId = location.pathname.substring(sitePath.length+1,location.pathname.length-5);
activeId = decodeURI(activeId);
$(function () {
    var elements = document.querySelectorAll('h1,h2,h3');
    var i;
    var sections = document.getElementById('pageNav');
    for (i = 0; i < elements.length; i++) {
        var e = elements[i];
        sections.innerHTML = sections.innerHTML + '<a class=\"anchor\" href="#' + e.id + '">' + e.innerHTML + '</a><br/>';
    }

    $(function() {
        var tree = $('#siteNav');
        tree.tree({
            closedIcon: '[+]',
            openedIcon: '[-]',
            data: data
        });

        tree.bind(
            'tree.click',
            function(event) {
                // The clicked node is 'event.node'
                var node = event.node;
                if(typeof node.url !== 'undefined'){
                    location.href = sitePath + node.url;
                }
            }
        );

        if(activeId.length>0){
            var node  = tree.tree('getNodeById', activeId);
            if(node !== undefined && node.parent !== undefined){
                tree.tree('openNode',node.parent);
                tree.tree('selectNode', node);
            }
        }
    });
});

mermaid.sequenceConfig = {
    diagramMarginX:50,
    diagramMarginY:10,
    boxTextMargin:5,
    noteMargin:10,
    messageMargin:35,
    mirrorActors:true,
    width:150,
    // Height of actor boxes
    height:30
};

mermaid.ganttConfig = {
    titleTopMargin:25,
    barHeight:20,
    barGap:4,
    topPadding:50,
    sidePadding:75,
    gridLineStartPadding:35,
    fontSize:11,
    numberSectionStyles:3
};
mermaid.initialize({startOnLoad:true});