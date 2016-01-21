topo_name=grid5h0EvenDemand
matrix_name=grid5h0
for err in 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00 1.10 1.20 1.30 1.40 1.50 1.60 1.70 1.80 1.90 2.00
do
    echo $err
    Mod=`echo $err | tr . x`
    echo $Mod
    ./Simulate_Driver.native -all data/gen/${topo_name}.dot prediction/matrix/${matrix_name}-matrix/${matrix_name} prediction/matrix/${matrix_name}-matrix/${matrix_name}_error_${err} data/gen/${topo_name}.host 10 -deloop -scalesyn -out ${topo_name}${Mod}
    ./plot_exp_data.sh ${topo_name}${Mod} &
done
