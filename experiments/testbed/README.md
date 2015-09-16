Testbed Experiments
===================
One time:
	```
	./configure-switches.sh split
	```

1. Configure the switches and servers for kufi

	```
	<start controller with new scheme>
	./configure-kulfi.sh
	```

2. Start traffic replay
	```
	./start-replay.sh <params>
	```
3. Deconfigure
	```
	./deconfigure-kulfi.sh
	```
