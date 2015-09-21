#!/bin/sh
set -x
ulimit -n 4096
ABILENE_DIR=~/atlas-abilene
RUN_ID=$1
GEN_REP=$2
SCALE=$3
MODEL=$4
DYN_RT=$5
FACTOR=$6
TRAFFIC_GEN=$7
NODE_ID=`hostname | cut -d '-' -f 2`
LOADED=`lsmod | grep modkulfi`
if [ "$#" -ne 7 ]
then
	echo "Usage: $0 <run_id> <regenerate_script?(0/1)> <time-scale-down> <model> <flow-scale-up factor> <traffic_generator scripted_tcp/scripted_udp/realtime>"
	exit 0
fi

if [ -z "$LOADED" ]
then
	echo "modkulfi not loaded. Exiting."
	exit 0
fi

if [ "$GEN_REP" -eq 1 ]
then
	rm -rf $ABILENE_DIR
	scp -r olympic:$ABILENE_DIR ./
	cd $ABILENE_DIR
	if [ "$TRAFFIC_GEN" = "scripted_tcp" ]
	then
		./replay-script-gen/tcp-custom $NODE_ID $MODEL $SCALE $DYN_RT $FACTOR
		chmod +x ./replay_script.sh
	fi
	if [ "$TRAFFIC_GEN" = "scripted_udp" ]
	then
		./replay-script-gen/udp-iperf $NODE_ID $MODEL $SCALE $DYN_RT $FACTOR
		chmod +x ./replay_script.sh
	fi
else
	cd $ABILENE_DIR
fi

sleep 2
./sync-client -s olympic
# exit 0
if [ "$TRAFFIC_GEN" = "scripted_tcp" ] || [ "$TRAFFIC_GEN" = "scripted_udp" ]
then
	echo "Using scripted replay"
	./replay_script.sh $DYN_RT
else
	echo "Using new traffic generator"
	./traffic-generator $NODE_ID $MODEL $SCALE $DYN_RT $FACTOR $RUN_ID > traffic_gen_$NODE_ID.log
fi
scp flow-time-* olympic:~/results/$RUN_ID/
rm flow-time-*
scp src-* olympic:~/results/$RUN_ID/
rm src-*
scp traffic_gen_* olympic:~/results/$RUN_ID/
rm traffic_gen_*
