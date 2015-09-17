#!/bin/bash
. utils/colors.sh
. utils/common.sh
cp $KULFI_GIT_DIR/agent/agent.py $ATLAS_KULFI_CONFIG/
KMUPDATE=$1

if ! [[ -z $KMUPDATE ]]; then
	echo ${CYAN}------ Copy Kernel Module -----${RESTORE}
	rm -rf $ATLAS_KULFI_CONFIG/kernel
	cp -r $KULFI_GIT_DIR/kernel/ $ATLAS_KULFI_CONFIG/
fi

rm -rf $ATLAS_KULFI_CONFIG/routes
cp -r $KULFI_GIT_DIR/routes/ $ATLAS_KULFI_CONFIG/

dsh -M -g atlas-abilene -c "rm -rf $ATLAS_KULFI_CONFIG ; scp -r olympic:$ATLAS_KULFI_CONFIG ./ ; $ATLAS_KULFI_CONFIG/configure.sh $KMUPDATE"
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
$ATLAS_KULFI_CONFIG/arp.sh
./configure-switches.sh
