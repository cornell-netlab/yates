var width = 3000,
    height = 1500;

function getanewgraph(filename ,base) {
    var links;

    var nodes = {};
    var counts = {};
    var selected_link = null;
    var nodeRadius = 15;
    var force;
    var thres=10;
    var linksTot=[];
    var nodesTot=[];
    var bases=[];
    var pathSet=[];
    var circleSet=[];
    var scalesSet=[];
    var svgSet=[];
    var curFile;



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
        return Math.log(d.capacity)*1.5;
    }

    function sqr(a)
    {
        return a*a;
    }
    function tick() {
        pathSet[curFile].selectAll('path').attr('d', function (d) {
            if (counts[d.norm] == 1)
                return "M" + d.source.x + "," + d.source.y + "L" + d.target.x + "," + d.target.y;
            var len= Math.sqrt(sqr(d.source.x-d.target.x)+sqr(d.source.y-d.target.y));
            var dr1 = (d.source.x+d.target.x)/2+(d.count)*100*(d.source.x-d.target.x)/len;
            var dr2 = (d.source.y+d.target.y)/2+(d.count)*100*(d.target.y-d.source.y)/len;
            return "M" + d.source.x + "," + d.source.y + " Q " +dr1+","+dr2+" "+ d.target.x + "," + d.target.y;
        });

        circleSet[curFile].selectAll('g').attr('transform', function (d) {
            return 'translate(' + d.x + ',' + d.y + ')';
        });
    }


    function textForNode(d) {
        return "Utilization: " + d.util + "%"+ "Count: "+ d.count;
    }


    function startit(filename) {
        var filteredLinks=linksTot[filename].filter(function(d) { return d.target.name[0]!='h' && d.source.name[0]!='h' && d.util>=thres; });
        var filteredNodes=d3.values(nodesTot[filename]).filter(function (d){return d.name[0]!='h'});
        curFile=filename;

        // plot the scales
        var scdata = [];
        var tot = 100;
        var starting=bases[filename];
        var barlength = 2700;
        var legWid=60;
        var scale=scalesSet[filename].selectAll("line").data(scdata,function(d){return d;});
        scale.exit().remove();
        for (var i = 0; i <= tot; i++)
            scdata.push(i * (100 / tot));
        scale = scalesSet[filename].selectAll("line").data(scdata);
            var sg = scale.enter();
            sg.append('line').style("stroke", function (d) {return getcolor(d); })
                .style("stroke-width", function (d) {if (thres==d) return 100; else return legWid;})
                .attr("y1", starting)
                .attr("y2", starting)
                .style("opacity", function (d) { if (thres==d) return 100; else return getopa(d); })
                .attr("x1", function (d, i) { return 100 + i * barlength / tot; })
                .attr("x2", function (d, i) { return 100 + (i + 1) * barlength / tot; })
                .on('mousedown',function (d){
                    thres=d;
                    startit(filename);
                })
                .append("title")
                .text(function (d) { return d; });
            scale.exit().remove();

            svgSet[filename].append('svg:text').attr('y', starting-legWid).attr('x',  barlength/2.4).attr('class', 'textid').text("Congestion of "+filename);
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


        var path= pathSet[filename].selectAll('path').data(filteredLinks, function (d) {return d.source.name+"-"+d.target.name;});


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

        var circle = circleSet[filename].selectAll('g').data(filteredNodes, function (d) {return d.name;});

        var g = circle.enter().append("svg:g");
        g.append('svg:circle')
            .attr('class', 'node')
            .attr('r', nodeRadius)
            .style('fill', '#000')
            .style('stroke', '#fff')
            .call(force.drag)
            .append("title")
            .text(function (d) { return d.name; });

        g.append("text")
            .attr('x', -4)
            .attr('y', -8)
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
            link.source = nodes[link.source] || (nodes[link.source] = { name: link.source });
            link.target = nodes[link.target] || (nodes[link.target] = { name: link.target });
        });
        force = d3.layout.force()
            .nodes(d3.values(nodes))
            .links(links)
            .size([width, height])
            .linkDistance(50)
            .charge(-5000)
            .on('tick', tick);
        linksTot[filename]=links;
        nodesTot[filename]=nodes;
        bases[filename]=base;
        var svg = d3.select('body')
            .append('svg')
            .attr('oncontextmenu', true)
            .attr('width', width)
            .attr('height', height);

        var path = svg.append('svg:g');
        var circle = svg.append('svg:g');
        var scales = svg.append('svg:g');
        pathSet[filename]=path;
        circleSet[filename]=circle;
        scalesSet[filename]=scales;
        svgSet[filename]=svg;
        curFile=filename;
        startit(filename,base);
    });
}

d3.json("files.json", function (error, data) {
    if (error) alert(error);
    var base=150;
    data.forEach(function (filename) {
        getanewgraph(filename.name,base);
        base=base;
    })
});
