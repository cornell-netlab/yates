all:
	cd tcp && $(MAKE)
	cd replay-script-gen && $(MAKE)
	g++ -O3 traffic-generator.cpp -o traffic-generator

clean:
	cd tcp && $(MAKE) clean
	cd replay-script-gen && $(MAKE) clean
	rm traffic-generator
