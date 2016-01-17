for topo in abilene; do
    echo $topo
    ./Simulate_Driver.native -all data/topologies/${topo}.dot data/demands/${topo}.txt data/demands/${topo}.txt data/hosts/${topo}.host 10 -deloop -fail-time 30 -lr-delay 10 -budget 3 -scalesyn >> out.txt
    ./plot_exp_data.sh ${topo} &
done
