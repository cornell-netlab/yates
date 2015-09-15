#!/bin/bash
. ../utils/colors.sh
. ../utils/common.sh
LOG_FILE=$LOG_DIR/fabric.log
rm $LOG_FILE
echo $BLUE"Check all-to-all connectivity between nodes with pings"
echo "Press any key to continue." $RESTORE
read x
for s in `seq 1 12`;
do
	echo "$s -->"
	for d in `seq 1 12`;
	do
		ssh atlas-$s -C "ping -c 2 10.0.0.$d" >> $LOG_FILE 2>&1 &
	done
done    
sleep 10
num_fails=`grep "packet loss" $LOG_FILE | grep " 0%" | wc -l`
if [[ num_fails -ne 24 ]] ; then
	echo $RED"Fabric disconnected"$RESTORE
else
	echo $GREEN"Fabric connected"$RESTORE
fi
