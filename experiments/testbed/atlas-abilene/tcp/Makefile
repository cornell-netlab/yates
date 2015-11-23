all: client.cpp server.cpp 
	gcc -Wall -o client client.cpp -lrt -lm
	gcc -Wall -o server server.cpp -lpthread

clean:
	$(RM) client server 

