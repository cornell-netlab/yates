#!/bin/bash
pushd ~
pushd atlas-abilene
make clean
popd
rm atlas-kulfi-configure/agent.py atlas-kulfi-configure/modkulfi.ko
rm -rf atlas-kulfi-configure/kernel
rm -rf atlas-kulfi-configure/routes
pushd sync
make clean
popd
pushd atlas-abilene
make clean
rm replay_script.sh
popd
popd
