all: replay-script-gen.cpp sync-server.cpp sync-client.cpp
	g++ replay-script-gen.cpp -o replay-script-gen
	g++ replay-script-gen-udp-iperf.cpp -o replay-script-gen-udp-iperf
	g++ sync-server.cpp -o sync-server
	g++ sync-client.cpp -o sync-client

clean:
	$(RM) replay-script-gen replay-sync-gen-udp-iperf sync-server sync-client
