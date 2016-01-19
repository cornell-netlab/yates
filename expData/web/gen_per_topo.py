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
schemes = ['spf', 'ecmp', 'ksp', 'mcf', 'raeke', 'vlb', 'semimcfecmp', 'semimcfksp', 'semimcfmcf', 'semimcfraeke', 'semimcfvlb']

names = {"cong_max" : "MAX CONGESTION",
        "cong_mean" : "MEAN CONGESTION",
        "cong_k50" : "MEDIAN CONGESTION",
        "cong_k90" : "90th %ile CONGESTION",
        "time" : "SOLVER TIME",
        "tput" : "THROUGHPUT",
        "tput_loss_fail" : "FAILURE LOSS",
        "tput_loss_cong" : "CONGESTION LOSS",
        "latency" : "LATENCY",
        "churn" : "CHURN",
        "numpaths" : "NUMBER OF PATHS"}

images = {"cong_max" : "MaxCongestionVsIterations",
        "cong_mean" : "MeanCongestionVsIterations",
        "cong_k50" : "k50CongestionVsIterations",
        "cong_k90" : "k90CongestionVsIterations",
        "time" : "TimeVsIterations",
        "tput" : "TotalThroughputVsIterations",
        "tput_loss_fail" : "FailureLossVsIterations",
        "tput_loss_cong" : "CongestionLossVsIterations",
        "latency" : "LatencyCDF",
        "churn" : "ChurnVsIterations",
        "numpaths" : "NumPathsVsIterations"}

for exp in sorted(names.keys()):
    print names[exp]
    print '<input type=checkbox id="chk'+exp+'" onclick="toggledisplay(\'img'+exp+'\');" />'
    print '<label for="chk'+exp+'"></label>'
    print '<hr>'
    print '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>'
    print '<div id="flat">'
    print '<div id="img'+exp+'" style="display:none">\n\t\t<img src="'+images[exp]+'.svg">\n <br>\n</div>'
    print '</div>'

print '<hr>'
print 'EDGE CONGESTION (based on simulation)<br>'
print "Show all: "
print '<input type=checkbox onclick="chksel(\'edge\');"/>'
print '<hr>'
print '<div id="edge">'
print '<table style="width:100%">'
print '<tr>'
print '<td>'
print '</td>'
for scheme in schemes:
    print '<td>'
    print '<input type=checkbox id="chkedge'+ scheme+'" onclick="toggledisplay(\'imgedge' + scheme+'\');" />'
    print '<label for="chkedge' + scheme + '">'+scheme+'</label>'
    print '</td>'
print '</tr>'
print '</table>'
print '</div>'

print '<hr>'
print '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>'
print '<div id="flat">'
for scheme in schemes:
    print '<div id="imgedge'+scheme+'" style="display:none">\n\t\t<img height="400" width="400" src="link_cong_'+scheme+'.svg">\n <br> '+ scheme+'\n</div>'
print '</div>'

print '<hr>'
print 'EXPECTED EDGE CONGESTION (based on routing scheme without capacity constraints - congestion can be greater than 1)<br>'
print "Show all: "
print '<input type=checkbox onclick="chksel(\'expedge\');"/>'
print '<hr>'
print '<div id="expedge">'
print '<table style="width:100%">'
print '<tr>'
print '<td>'
print '</td>'
for scheme in schemes:
    print '<td>'
    print '<input type=checkbox id="chkexpedge'+ scheme+'" onclick="toggledisplay(\'imgexpedge' + scheme+'\');" />'
    print '<label for="chkexpedge' + scheme + '">'+scheme+'</label>'
    print '</td>'
print '</tr>'
print '</table>'
print '</div>'

print '<hr>'
print '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>'
print '<div id="flat">'
for scheme in schemes:
    print '<div id="imgexpedge'+scheme+'" style="display:none">\n\t\t<img height="400" width="400" src="link_cong_exp_'+scheme+'.svg">\n <br> '+ scheme+'\n</div>'
print '</div>'


print '</body>\n</html>'
