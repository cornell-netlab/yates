var width = 800,
    height = 400;

var linksTot=[];
var nodesTot=[];
var pathSet=[];
var circleSet=[];
var circleTextSet=[];
var scalesSet=[];
var svgSet=[];
var forceSet=[];

function getanewgraph(filename ,whichComp) {
    var links;

    var nodes = {};
    var counts = {};
    var selected_link = null;
    var nodeRadius = 5;
    var thres=10;
    var curComp;



    function normalizeLink(l) {
        return l.source <= l.target ? [l.source, l.target] : [l.target, l.source];
    }

    function isSelected(d) {
        if (selected_link == null)
            return false;
        if (d === selected_link)
            return true;
        return false;
    }

    function getcolor(val) {
        if (val <= 25) return '#13edd9';
        if (val <= 50) return '#11f516';
        if (val <= 75) return '#fab105';
        return '#ff433a';
    }


    function getopa(val) {
        var comp = val;
        while (comp > 25) comp -= 25;
        return (comp + 25) / 50;
    }


    function capacityForNode(d) {
        return Math.log(d.capacity)*0.3;
    }

    function sqr(a)
    {
        return a*a;
    }
    function tick() {
        pathSet[curComp].selectAll('path').attr('d', function (d) {
            if (counts[d.norm] == 1)
                return "M" + d.source.x + "," + d.source.y + "L" + d.target.x + "," + d.target.y;
            var len= Math.sqrt(sqr(d.source.x-d.target.x)+sqr(d.source.y-d.target.y));
            var dr1 = (d.source.x+d.target.x)/2+(d.count)*35*(d.source.x-d.target.x)/len;
            var dr2 = (d.source.y+d.target.y)/2+(d.count)*35*(d.target.y-d.source.y)/len;
            return "M" + d.source.x + "," + d.source.y + " Q " +dr1+","+dr2+" "+ d.target.x + "," + d.target.y;
        });

        circleSet[curComp].selectAll('g').attr('transform', function (d) {
            return 'translate(' + d.x + ',' + d.y + ')';
        });
        circleTextSet[curComp].selectAll('g').attr('transform', function (d) {
            return 'translate(' + d.x + ',' + d.y + ')';
        });
    }


    function textForNode(d) {
        return "Utilization: " + d.util + "%"+ "Count: "+ d.count;
    }


    function startit(filename,whichComp) {
        var filteredLinks=linksTot[whichComp].filter(function(d) { return d.target.name[0]!='h' && d.source.name[0]!='h' && d.util>=thres; });
        var filteredNodes=d3.values(nodesTot[whichComp]).filter(function (d){return d.name[0]!='h'});
        var force=forceSet[whichComp];
        curComp=whichComp;

        // plot the scales
        var scdata = [];
        var tot = 100;
        var starting=25;
        var barlength = 600;
        var legWid=10;
        var scale=scalesSet[whichComp].selectAll("line").data(scdata,function(d){return d;});
        scale.exit().remove();
        for (var i = 0; i <= tot; i++)
            scdata.push(i * (100 / tot));
        scale = scalesSet[whichComp].selectAll("line").data(scdata);
            var sg = scale.enter();
            sg.append('line').style("stroke", function (d) {return getcolor(d); })
                .style("stroke-width", function (d) {if (thres==d) return 20; else return legWid;})
                .attr("y1", starting)
                .attr("y2", starting)
                .style("opacity", function (d) { if (thres==d) return 100; else return getopa(d); })
                .attr("x1", function (d, i) { return 100 + i * barlength / tot; })
                .attr("x2", function (d, i) { return 100 + (i + 1) * barlength / tot; })
                .on('mousedown',function (d){
                    thres=d;
                    startit(filename,whichComp);
                })
                .append("title")
                .text(function (d) { return d; });
            scale.exit().remove();

            var textin=["Congestion of "+filename];
            var curText=svgSet[whichComp].selectAll('text').data(textin, function (d) {return d;});
            curText.enter().
              append('text').attr('y', starting-legWid).attr('x',  barlength/2).text(function(d){return d;});
            curText.exit().remove();
            scale.enter().append('svg:text')
                .attr('x', function (d, i) { return 100 + i * barlength / tot; })
                .attr('y', starting+legWid*2)
                .attr('class', 'textid')
                .text(function (d, i) {
                    if (i % 10 == 0)
                        return d;
                    else
                        return "";
                });


        var path= pathSet[whichComp].selectAll('path').data(filteredLinks, function (d) {return d.source.name+"-"+d.target.name;});


        path.classed('class','links')
            .style('stroke-width', function (d) { return capacityForNode(d); })
            .style("fill", "none")
            .style('stroke', function (d) { return getcolor(d.util); })
            .style("opacity", function (d) { return getopa(d.util); })
            .append("title")
            .text(function (d) { return textForNode(d); });

        path.enter().append('svg:path')
            .classed('class','links')
            .style('stroke-width', function (d) { return capacityForNode(d); })
            .style("fill", "none")
            .style('stroke', function (d) { return getcolor(d.util); })
            .style("opacity", function (d) { return getopa(d.util); })
            .on('mousedown', function (d) {
                selected_link = d;
                d3.selectAll("svg").selectAll("g").selectAll("path")
                    .style("stroke-dasharray", function (d) { if (isSelected(d)) return "10,2"; })
                    .style("stroke-width", function (d) {return capacityForNode(d); })
                    .style("stroke", function (d) {return getcolor(d.util); });
            })
            .append("title")
            .text(function (d) { return textForNode(d); });

        path.exit().remove();


        circleTextSet[whichComp].selectAll('g').data([]).exit().remove();
        circleSet[whichComp].selectAll('g').data([]).exit().remove();
        var circleText = circleTextSet[whichComp].selectAll('g').data(filteredNodes, function (d) {return d.name;});

        circleText.enter().append('svg:g').append("text")
            .attr('x', -4)
            .attr('y', -8)
            .text(function (d) { return d.name; });
        circleText.exit().remove();

        var circle = circleSet[whichComp].selectAll('g').data(filteredNodes, function (d) {return d.name;});
        circle.enter().append('svg:g').append('circle')
            .attr('class', 'node')
            .attr('r', nodeRadius)
            .style('fill', '#000')
            .style('stroke', '#fff')
            .call(force.drag)
            .append("title")
            .text(function (d) { return d.name; });
        circle.exit().remove();

        force.start();
    }


    d3.json(filename, function (error, data) {
        if (error) console.warn(error);
        links = data;
        links.forEach(function (link) {
            var norm;
            link.rev= (link.source>link.target);
            norm= normalizeLink(link);
            counts[norm] = ++counts[norm] || 1;
            link.norm = norm;
            link.count = counts[norm];
            link.source = nodes[link.source] || (nodes[link.source] = { name: link.source, fname:filename });
            link.target = nodes[link.target] || (nodes[link.target] = { name: link.target, fname:filename });
        });
        if (forceSet[whichComp])
            forceSet[whichComp].stop();

        var force = d3.layout.force()
            .nodes(d3.values(nodes))
            .links(links)
            .size([width, height])
            .linkDistance(3)
            .charge(-1000)
            .on('tick', tick);
        forceSet[whichComp]=force;
        linksTot[whichComp]=links;
        nodesTot[whichComp]=nodes;
        var svg =svgSet[whichComp]|| d3.select(whichComp)
            .append('svg')
            .attr('oncontextmenu', true)
            .attr('width', width)
            .attr('height', height);

        var path = pathSet[whichComp] || svg.append('svg:g');
        var circle =circleSet[whichComp]|| svg.append('svg:g');
        var circleText =circleTextSet[whichComp]|| svg.append('svg:g');
        var scales =scalesSet[whichComp]|| svg.append('svg:g');
        pathSet[whichComp]=path;

        circleSet[whichComp]=circle;
        circleTextSet[whichComp]=circleText;
        scalesSet[whichComp]=scales;
        svgSet[whichComp]=svg;
        curComp=whichComp;
        startit(filename,whichComp);
    });
}
var idName='select1';
var oldSel = document.getElementById(idName);
while (oldSel.options.length > 1) {
    oldSel.remove(oldSel.options.length - 1);
}
var i=0;

d3.json("files.json", function (error, data) {
    if (error) alert(error);
    data.forEach(function (filename) {
        var opt = document.createElement('option');
        opt.text = filename.name;
        opt.value = i;
        i=i+1;

        oldSel.add(opt, null);
    })
});

var idName2='select2';
var oldSel2 = document.getElementById(idName2);
while (oldSel2.options.length > 1) {
    oldSel2.remove(oldSel2.options.length - 1);
}
var i2=0;
var rs=[];
d3.json("files.json", function (error, data) {
    if (error) alert(error);
    data.forEach(function (filename) {
        var opt = document.createElement('option');
        opt.text = filename.name;
        opt.value = i2;
        i2=i2+1;
        rs.push(filename.name);

        oldSel2.add(opt, null);
    })
});

function selectTopo(inobj,which) {
    getanewgraph(rs[parseInt(inobj)],which);
}
