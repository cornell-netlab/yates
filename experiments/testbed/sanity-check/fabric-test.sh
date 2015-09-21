#!/bin/bash
. ~/utils/colors.sh
. ~/utils/common.sh
LOG_FILE=$LOG_DIR/fabric.log
rm $LOG_FILE
echo $BLUE"Check all-to-all connectivity between nodes with pings"
echo "Press any key to continue." $RESTORE
read x
for d in `seq 1 12`;
do
	echo " --> $d"
	for s in `seq 1 12`;
	do
		ssh atlas-$s -C "ping -c 2 10.0.0.$d" >> $LOG_FILE 2>&1 &
	done
done    
sleep 5
num_pass=`grep "packet loss" $LOG_FILE | grep " 0%" | wc -l`
if [[ num_pass -ne 144 ]] ; then
	sleep 5
fi

num_pass=`grep "packet loss" $LOG_FILE | grep " 0%" | wc -l`
if [[ num_pass -ne 144 ]] ; then
	echo $RED"Fabric disconnected $num_pass / 144"$RESTORE
else
	echo $GREEN"Fabric connected $num_pass / 144"$RESTORE
fi
