#!/bin/bash
. ~/utils/colors.sh
. ~/utils/common.sh
echo $BLUE"Check uptime of nodes"$RESTORE
dsh -M -g atlas-abilene -c "uptime"
