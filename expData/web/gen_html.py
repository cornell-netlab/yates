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

topologies = ['cube5h1EvenDemand', 'cube5h1FarDemand', 'cube5h1NearDemand', 'cycle10EvenDemand', 'cycle10FarDemand', 'cycle10NearDemand', 'gnm20m60EvenDemand', 'gnm20m60FarDemand', 'gnm20m60NearDemand', 'grid5h0EvenDemand', ' grid5h0FarDemand', 'grid5h0NearDemand', 'grid5h1EvenDemand', 'grid5h1FarDemand', 'grid5h1NearDemand', 'PA30per3EvenDemand', 'PA30per3NearDemand']

schemes = ['spf', 'ecmp', 'mcf', 'raeke', 'vlb', 'semimcfecmp', 'semimcfmcf', 'semimcfraeke', 'semimcfvlb', 'akecmp', 'akraeke', 'akmcf', 'akvlb']

print "Show all for: "
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
print '</body>\n</html>'
