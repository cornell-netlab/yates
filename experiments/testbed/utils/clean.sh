#!/bin/bash
pushd ~
pushd atlas-abilene
make clean
popd
rm atlas-kulfi-configure/agent.py atlas-kulfi-configure/modkulfi.ko
pushd sync
make clean
popd
pushd atlas-abilene
make clean
rm replay_script.sh
popd
popd
