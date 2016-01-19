for topo in grid5h1EvenDemand ; do
    echo $topo
    for params in "10 -deloop -budget 3 -scalesyn" "10 -deloop -budget 3 -scalesyn -fail-time 10" "10 -deloop -budget 3 -scalesyn -fail-time 10 -lr-delay 10" "10 -deloop -budget 3 -scalesyn -fail-time 10 -lr-delay 10 -gr-delay 30" ; do
        echo $params
        out_dir=${topo}_$(echo "${params}" | tr '[[:space:]]' '_')
        ./Simulate_Driver.native -all data/topologies/${topo}.dot data/demands/${topo}.txt data/demands/${topo}.txt data/hosts/${topo}.host ${params} -out ${out_dir} >> out.txt
        ./plot_exp_data.sh ${topo} ${out_dir}
        python ./expData/web/gen_per_topo.py > ./expData/${out_dir}/index.html
    done
done

python ./expData/web/gen_results_browser.py ./expData > expData/index.html
