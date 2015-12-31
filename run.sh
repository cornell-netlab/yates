for topo in abilene; do
    echo $topo
    ./Simulate_Driver.native -all data/topologies/${topo}.dot data/demands/${topo}.txt data/demands/${topo}.txt data/hosts/${topo}.host 10 -deloop >> out.txt
    ./plot_exp_data.sh ${topo} &
done
