#!/bin/bash
rm atlas-abilene/replay-script-gen atlas-abilene/sync-client
rm atlas-kulfi-configure/agent.py atlas-kulfi-configure/modkulfi.ko
cd sync
make clean
cd -
cd atlas-abilene
make clean
rm replay_script.sh
cd

