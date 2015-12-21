for topo in grid5h1EvenDemand grid5h1FarDemand grid5h1NearDemand; do
    echo $topo
    ./Simulate_Driver.native -all data/gen/${topo}.dot data/gen/${topo}.txt data/gen/${topo}.txt    data/gen/${topo}.host 10 -deloop >> out.txt
    ./plot_exp_data.sh ${topo} &
done
