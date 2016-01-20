import sys
import os

names = {"cong_max" : "MAX CONGESTION",
        "cong_mean" : "MEAN CONGESTION",
        "cong_k50" : "MEDIAN CONGESTION",
        "cong_k90" : "90th %ile CONGESTION",
        "time" : "SOLVER TIME",
        "tput" : "THROUGHPUT",
        "tput_loss_fail" : "FAILURE LOSS",
        "tput_loss_cong" : "CONGESTION LOSS",
        "latency" : "LATENCY",
        "tmchurn" : "TM CHURN",
        "recchurn" : "RECOVERY CHURN",
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
        "tmchurn" : "TMChurnVsIterations",
        "recchurn" : "RecoveryChurnVsIterations",
        "numpaths" : "NumPathsVsIterations"}


for metric in names.keys():
  with open(sys.argv[1]+"/"+metric+".html","w") as f:
    f.write('<!DOCTYPE html> \n\
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
    ')
    f.write(names[metric] + ' <br>\n')

    tl = []
    for topo in next(os.walk(sys.argv[1]))[1]:
        if topo not in ['web']:
            tl.append(topo)

    for topo in sorted(tl):
        f.write( topo)
        f.write( '<input type=checkbox id="chk'+topo+'"onclick="toggledisplay(\'img'+topo+'\');" />')
        f.write( '<label for="chk'+topo+'"></label>')
        f.write( '<hr>')
        f.write( '<style>\n#flat {width:100%; margin:0 auto 0 auto; text-align:center;}\n#flat div \n{\ndisplay:inline-block;\n}\n</style>')
        f.write( '<div id="flat">')
        f.write( '<div id="img'+topo+'" style="display:none">\n\t\t<img src="'+topo+'/'+images[metric]+'.svg">\n <br>\n</div>')
        f.write( '</div>')


