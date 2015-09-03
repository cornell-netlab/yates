#!/bin/bash
for s in `seq 1 3`;
do
	ssh root@sw$s "bash -s" < ./switch/split/sw$s.sh
	ssh root@sw$s "bash -s" < ./switch/agent-network/sw$s.sh
done
