topo_name=$1 #=grid5h1EvenDemand
for err in 0.00 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00 1.10 1.20 1.30 1.40 1.50 1.60 1.70 1.80 1.90 2.00
do
    echo $err
    Mod=`echo $err | tr . x`
    echo $Mod
    ./Simulate_Driver.native -all data/topologies/${topo_name}.dot prediction/matrix/${topo_name}-matrix/${topo_name}_mergelen_12 prediction/matrix/${topo_name}-matrix/${topo_name}_mergelen_12_error_${err} data/hosts/${topo_name}.host 10 -deloop -scalesyn -out ${topo_name}${Mod} &
    #./plot_exp_data.sh ${topo_name} ${topo_name}${Mod} &
done
