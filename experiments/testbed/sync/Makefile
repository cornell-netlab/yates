all: replay-script-gen.cpp sync-server.cpp sync-client.cpp
	g++ replay-script-gen.cpp -o replay-script-gen
	g++ sync-server.cpp -o sync-server
	g++ sync-client.cpp -o sync-client

clean:
	$(RM) replay-script-gen sync-server sync-client 
