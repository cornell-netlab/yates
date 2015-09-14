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
LOADED=`lsmod | grep modkulfi`
if [ "$#" -ne 6 ]
then
	echo "Usage: $0 <run_id> <regenerate_script?(0/1)> <time-scale-down> <model> <flow-scale-up factor>"
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
	./replay-script-gen/tcp-custom `hostname | cut -d '-' -f 2` $MODEL $SCALE $DYN_RT $FACTOR
	chmod +x ./replay_script.sh
else
	cd $ABILENE_DIR
fi
sleep 2
./sync-client -s olympic
exit 0
./replay_script.sh $DYN_RT
scp flow-time-* olympic:~/results/$RUN_ID/
rm flow-time-*
scp src-* olympic:~/results/$RUN_ID/
rm src-*
