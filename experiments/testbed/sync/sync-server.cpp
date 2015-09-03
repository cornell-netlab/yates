#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <netinet/in.h>
#include <resolv.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <pthread.h>

int main(int argc, char* argv[]){
    int opt;
    int listen_port = 6000; // default port to listen on
    struct sockaddr_in my_addr;
    int sockfd;
    int tmp_int;
    int** client;
    int num_clients = 2;
    sockaddr_in sock_addr;
    socklen_t addr_size = sizeof(sockaddr_in);
    char buffer[256];
    int read_len;

    while ((opt = getopt (argc, argv, "p:n:")) != -1){
        switch (opt)
        {
            case 'p':
                listen_port = atoi(optarg);
                break;
            case 'n':
                num_clients = atoi(optarg);
                break;
            case '?':
                if (optopt == 'p' || optopt == 'n')
                    fprintf (stderr, "Option -%c requires an argument.\n", optopt);
                else if (isprint (optopt))
                    fprintf (stderr, "Unknown option `-%c'.\n", optopt);
                else
                    fprintf (stderr,
                            "Unknown option character `\\x%x'.\n",
                            optopt);
                return 1;
            default:
                abort ();
        }
    }

    client = (int **)malloc(num_clients * sizeof(int *));
    // Create socket
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if(sockfd == -1){
        printf("Error creating socket %d\n", errno);
        return -1;
    }

    // Set socket options
    tmp_int = 1;
    if( (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char*)&tmp_int, sizeof(int)) == -1 )||
            (setsockopt(sockfd, SOL_SOCKET, SO_KEEPALIVE, (char*)&tmp_int, sizeof(int)) == -1 ) ){
        printf("Error setting options %d\n", errno);
        return -1;
    }

    my_addr.sin_family = AF_INET ;
    my_addr.sin_port = htons(listen_port);

    memset(&(my_addr.sin_zero), 0, 8);
    my_addr.sin_addr.s_addr = INADDR_ANY ;

    // Bind server socket
    if(bind(sockfd, (sockaddr*)&my_addr, sizeof(my_addr)) == -1 ){
        fprintf(stderr,"Error binding to socket %d\n", errno);
        return -1;
    }

    // Start listening for client connections
    if(listen(sockfd, SOMAXCONN) == -1){
        fprintf(stderr, "Error listening %d\n", errno);
        return -1;
    }

    // Wait for clients and process them in individaual threads
    for(int i=0; i < num_clients; i++){
        printf("waiting for a connection\n");
        client[i] = (int*)malloc(sizeof(int)); // will be freed in handler
        if((*client[i] = accept(sockfd, (sockaddr*)&sock_addr, &addr_size)) != -1){
            fprintf(stderr, "accepted connection\n");
            memset(buffer, 0, 256);
            read_len = read(*client[i], buffer, 256);
            fprintf(stderr, "%s", buffer);
        }
        else {
            fprintf(stderr, "Failed to accept connection\n");
        }
    }

    fprintf(stderr, "Done...\n");

    for(int i=0; i< num_clients; i++){
        read_len = write(*client[i], "Go!", 3);
        shutdown(*client[i], SHUT_WR);
        close(*client[i]);
        free(client[i]);
    }
    free(client);
    close(sockfd);

    return 0;
}


