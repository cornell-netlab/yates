#!/usr/bin/env python

print '<!DOCTYPE html> \n\
<html>\n\
<head>\n\
</head>\n\
<body>\n\
<script>\n\
function toggledisplay(id) {\n\
    var el = document.getElementById(id);\n\
    if (el.style.display=="inline-block") {\n\
        el.style.display="none";\n\
    }\n\
    else {\n\
        el.style.display="inline-block";\n\
    }\n\
}\n\
function chksel(id)\n\
    {\n\
        var tab = document.getElementById(id);\n\
        var chk = tab.getElementsByTagName("input");\n\
        var num = chk.length;\n\
        for (var i = 0; i < num; i++)\n\
        {\n\
            status = chk[i].getAttribute("type");\n\
            if ( status == "checkbox") {\n\
                chk[i].checked = !chk[i].checked;\n\
                if (chk[i].onclick){\n\
                    chk[i].onclick();\n\
                }\n\
            }\n\
        }\n\
    }\n\
</script>\n\
 <style type="text/css">\n\
 td { width: 20px; overflow: hidden; }\n\
 table { width : 100%; table-layout: fixed; }\n\
 </style>\n\
'

#topologies = ['cube5h1EvenDemand', 'cube5h1FarDemand', 'cube5h1NearDemand', 'cycle10EvenDemand', 'cycle10FarDemand', 'cycle10NearDemand', 'gnm20m60EvenDemand', 'gnm20m60FarDemand', 'gnm20m60NearDemand', 'grid5h0EvenDemand', 'grid5h0FarDemand', 'grid5h0NearDemand', 'grid5h1EvenDemand', 'grid5h1FarDemand', 'grid5h1NearDemand', 'PA30per3EvenDemand', 'PA30per3NearDemand']
topologies = ['grid5h1EvenDemand', 'grid5h1FarDemand', 'grid5h1NearDemand']

schemes = ['spf', 'ecmp', 'ksp', 'mcf', 'raeke', 'vlb', 'semimcfecmp', 'semimcfksp', 'semimcfmcf', 'semimcfraeke', 'semimcfvlb', 'akecmp', 'akksp', 'akmcf', 'akraeke', 'akvlb']

print 'Use ctrl-F5 (force refresh) to reload... <hr>'
print 'MAX CONGESTION'
print '<input type=checkbox onclick="chksel(\'maxcong\');"/>' + "Show All"
print '<hr>'
print '<div id="maxcong">'
print '<table style="width:100%">'
print '<tr>'
print '<td>'
print '</td>'
for topo in topologies:
    print '<td>'
    print '<input type=checkbox id="chkmaxcong'+ topo +'"onclick="toggledisplay(\'imgmaxcong' + topo +'\');" />'
    print '<label for="chkmaxcong' + topo + '">'+topo+'</label>'
    print '</td>'
print '</tr>'
print '</table>'
print '</div>'

print '<hr>'
print '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>'
print '<div id="flat">'
for topo in topologies:
      print '<div id="imgmaxcong'+topo+'" style="display:none">\n\t\t<img src="'+topo+'/MaxCongestionVsIterations.svg">\n <br>'+topo + '\n</div>'
print '</div>'


print '<hr>'
print 'THROUGHPUT'
print '<input type=checkbox onclick="chksel(\'tput\');"/>' + "Show All"
print '<hr>'
print '<div id="tput">'
print '<table style="width:100%">'
print '<tr>'
print '<td>'
print '</td>'
for topo in topologies:
    print '<td>'
    print '<input type=checkbox id="chktput'+ topo +'"onclick="toggledisplay(\'imgtput' + topo +'\');" />'
    print '<label for="chktput' + topo + '">'+topo+'</label>'
    print '</td>'
print '</tr>'
print '</table>'
print '</div>'

print '<hr>'
print '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>'
print '<div id="flat">'
for topo in topologies:
      print '<div id="imgtput'+topo+'" style="display:none">\n\t\t<img src="'+topo+'/TotalThroughputVsIterations.svg">\n <br>'+topo + '\n</div>'
print '</div>'

print '<hr>'
print 'EDGE CONGESTION (based on simulation)'
print "Show all: "
for topo in topologies:
    print '<input type=checkbox onclick="chksel(\''+topo+'\');"/>' + topo
print '<hr>'
for topo in topologies:
    print '<div id="' + topo + '">'
    print '<table style="width:100%">'
    print '<tr>'
    print '<td>'
    print topo
    print '</td>'
    for scheme in schemes:
        print '<td>'
        print '<input type=checkbox id="chk'+ topo + scheme+'" onclick="toggledisplay(\'img' + topo + scheme+'\');" />'
        print '<label for="chk' + topo + scheme + '">'+scheme+'</label>'
        print '</td>'
    print '</tr>'
    print '</table>'
    print '</div>'

print '<hr>'
print '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>'
print '<div id="flat">'
for topo in topologies:
    for scheme in schemes:
        print '<div id="img'+topo+scheme+'" style="display:none">\n\t\t<img height="400" width="400" src="'+topo+'/link_cong_'+scheme+'.svg">\n <br>'+topo +' : '+ scheme+'\n</div>'
print '</div>'


print '<hr>'
print 'EXPECTED EDGE CONGESTION (based on routing scheme without capacity constraints - congestion can be greater than 1) <br>'
print "Show all: "
for topo in topologies:
    print '<input type=checkbox onclick="chksel(\'exp'+topo+'\');"/>' + topo
print '<hr>'
for topo in topologies:
    print '<div id="exp' + topo + '">'
    print '<table style="width:100%">'
    print '<tr>'
    print '<td>'
    print topo
    print '</td>'
    for scheme in schemes:
        print '<td>'
        print '<input type=checkbox id="chkexp'+ topo + scheme+'" onclick="toggledisplay(\'imgexp' + topo + scheme+'\');" />'
        print '<label for="chkexp' + topo + scheme + '">'+scheme+'</label>'
        print '</td>'
    print '</tr>'
    print '</table>'
    print '</div>'

print '<hr>'
print '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>'
print '<div id="flat">'
for topo in topologies:
    for scheme in schemes:
        print '<div id="imgexp'+topo+scheme+'" style="display:none">\n\t\t<img height="400" width="400" src="'+topo+'/link_cong_exp_'+scheme+'.svg">\n <br>'+topo +' : '+ scheme+'\n</div>'
print '</div>'

print '</body>\n</html>'
