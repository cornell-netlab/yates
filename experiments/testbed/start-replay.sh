#!/bin/bash
. colors.sh
set -x
ATLAS_ABILENE_SCRIPT=~/atlas-node-start-rep.sh
ABILENE_DIR=~/atlas-abilene
SYNC_DIR=~/sync
RUN_ID=$1
GEN_REP=$2
SCALE=$3
MODEL=$4
DYN_RT=$5
FACTOR=$6
if [ "$#" -ne 6 ]
then
	echo ${RED}"Usage: $0 <run_id> <regenerate_script?(0/1)> <time-scale-down> <TM model> <Dynamic RT(0/1)> <scale-up factor>"${RESTORE}
	exit 0
fi

if [ -z "$RUN_ID" ]
then 
	echo ${RED} "No RUN_ID specified. Using 1"${RESTORE}
	RUN_ID=1
fi

if [ "$GEN_REP" -gt 1 ]
then 
	echo ${RED} "Invalid GEN_REP (0/1) specified. Exiting"${RESTORE}
	exit 0
fi

if [ "$SCALE" -lt 0 ]
then
	echo ${RED} "Invalid scale : Scale original 5 mins to x secs"${RESTORE}
	exit 0
fi

if [ "$MODEL" -gt 5 ]
then
	echo ${RED} "Invalid TM model: 1:realOD, 2: simpleGravityOD, 3:simpleTomogravityOD, 4:generalGravityOD, 5:generalTomogravityOD"${RESTORE}
	exit 0
fi

pushd $SYNC_DIR
make
popd
cp $SYNC_DIR/sync-client $ABILENE_DIR/
cp $SYNC_DIR/replay-script-gen $ABILENE_DIR/

DIR=~/results/$RUN_ID
if [ -d "$DIR" ]; then
	echo ${CYAN}"Dir exists. Backing up"${RESTORE}
	mv $DIR $DIR.old
fi
MOD=`dsh -M -g atlas-abilene -c "sudo lsmod | grep kulfi " | wc -l`
if [ "$MOD" -ne 12 ]
then
	echo ${RED}"modkulfi not loaded on all servers. Exiting [$MOD]."${RESTORE}
	exit 0
fi

pkill -9 sync-server
$SYNC_DIR/sync-server -n 12 &
mkdir -p ~/results/$RUN_ID
dsh -M -g atlas-abilene -c "scp olympic:$ATLAS_ABILENE_SCRIPT ./ ; chmod +x $ATLAS_ABILENE_SCRIPT ; $ATLAS_ABILENE_SCRIPT $RUN_ID $GEN_REP $SCALE $MODEL $DYN_RT $FACTOR" &
while true
do
	$SYNC_DIR/sync-server -n 12 -p 7000
done

