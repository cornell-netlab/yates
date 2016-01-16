for topo in 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00 1.10 1.20 1.30 1.40 1.50 1.60 1.70 1.80 1.90 2.00
do
    echo $topo
    Mod=`echo $topo | tr . x`
    echo $Mod
    cp data/gen/grid5h0EvenDemand.dot data/gen/grid5h0EvenDemand${Mod}.dot
    echo "./Simulate_Driver.native -all data/gen/grid5h0EvenDemand${Mod}.dot prediction/matrix/grid5h0-matrix/grid5h0-${topo} prediction/matrix/grid5h0-matrix/grid5h0_error_${topo} data/gen/grid5h0EvenDemand.host 10 -deloop "
    ./Simulate_Driver.native -all data/gen/grid5h0EvenDemand${Mod}.dot prediction/matrix/grid5h0-matrix/grid5h0-${topo} prediction/matrix/grid5h0-matrix/grid5h0_error_${topo} data/gen/grid5h0EvenDemand.host 10 -deloop 
    ./plot_exp_data.sh grid5h0EvenDemand${Mod} &
    #rm data/gen/grid5h0EvenDemand${Mod}.dot
done
