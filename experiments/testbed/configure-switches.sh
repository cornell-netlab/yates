#!/bin/bash
SPLIT=$1
for s in `seq 1 3`;
do
	if ! [[ -z $SPLIT ]]; then
		echo "splitting"
		cat ~/.ssh/id_rsa.pub | ssh root@sw$s "mkdir .ssh && cat - > .ssh/authorized_keys"
		ssh root@sw$s "bash -s" < ./switch/split/sw$s.sh
	fi
	ssh root@sw$s "bash -s" < ./switch/agent-network/sw$s.sh
done
