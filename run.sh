for topo in abilene; do
    echo $topo
    params="24 -deloop -budget 3 -scalesyn"
    #./Simulate_Driver.native -all data/topologies/${topo}.dot data/demands/${topo}.txt data/demands/${topo}.txt data/hosts/${topo}.host ${params} >> out.txt
    #./plot_exp_data.sh ${topo}
    python ./expData/web/gen_per_topo.py > ./expData/${topo}/index.html
    mv expData/${topo} expData/${topo}$(echo "${params}" | tr -d '[[:space:]]')
done

python ./expData/web/gen_results_browser.py ./expData > expData/index.html
