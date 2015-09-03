#!/bin/bash
. colors.sh
KULFI_GIT_DIR=$HOME/git/kulfi
ATLAS_KULFI_CONFIG=~/atlas-kulfi-configure
LOG_DIR=~/log/
cp $KULFI_GIT_DIR/agent/agent.py $ATLAS_KULFI_CONFIG/

echo ${CYAN}------ BUILDING: Kernel Module -----${RESTORE}
scp -r $KULFI_GIT_DIR/kernel/ atlas-1:~/tmp/ > $LOG_DIR/configure.log
ssh atlas-1 "pushd tmp/kernel && make" > $LOG_DIR/km.log
scp atlas-1:~/tmp/kernel/modkulfi.ko $ATLAS_KULFI_CONFIG/

dsh -M -g atlas-abilene -c "rm -rf atlas-kulfi ; scp -r olympic:$ATLAS_KULFI_CONFIG ./ ; $ATLAS_KULFI_CONFIG/configure.sh"
sleep 1
# Verify modkulfi is loaded on all servers
MOD=`dsh -M -g atlas-abilene -c "sudo lsmod | grep kulfi " | wc -l`

echo ${CYAN}"modkulfi loaded on $MOD hosts."
if [ "$MOD" -eq 12 ]
then
	echo ${GREEN}SUCCESS${RESTORE}
else
	echo ${RED}FAIL${RESTORE}
fi
sudo ifconfig em3 10.0.0.100
