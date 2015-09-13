#!/bin/bash
. colors.sh
. common.sh
rm $ATLAS_KULFI_CONFIG/agent.py
rm $ATLAS_KULFI_CONFIG/modkulfi.ko
rm $ABILENE_DIR/replay-script-gen
rm $ABILENE_DIR/sync-client

pushd sync
make clean
popd
dsh -M -g atlas-abilene -c "pkill -fx \"python.*agent.py\" ; sudo rmmod modkulfi 2> /dev/null ; killall sync-client"
# Verify modkulfi is loaded on all servers
MOD=`dsh -M -g atlas-abilene -c "sudo lsmod | grep kulfi " | wc -l`

echo "modkulfi loaded on $MOD hosts."
if [ "$MOD" -eq 0 ]
then
	echo ${GREEN}SUCCESS${RESTORE}
else
	echo ${RED}FAIL${RESTORE}
fi
