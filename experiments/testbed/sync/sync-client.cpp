#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <netinet/in.h>
#include <resolv.h>
#include <netdb.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>

int name_to_ip(char* hostname , char* ip_addr)
{
    struct addrinfo hints, *res;
    struct in_addr addr;
    int err;

    memset(&hints, 0, sizeof(hints));
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_family = AF_INET;
    if( (err = getaddrinfo(hostname, NULL, &hints, &res)) != 0){
        fprintf(stderr, "getaddrinfo host: %s error: %d\n", hostname, err);
        return err;
    }

    addr = ((struct sockaddr_in *)(res->ai_addr))->sin_addr;

    strcpy(ip_addr, inet_ntoa(addr));
    freeaddrinfo(res);
    return 0;
}

int main(int argc, char* argv[]){
    // Set server details
    int server_port = 6000;
    char server_addr[32] = "localhost";
    char server_name[128];

    int opt;

    char buffer[256];
    int read_len;

    while ((opt = getopt (argc, argv, "s:p:")) != -1){
        switch (opt)
        {
            case 'p':
                server_port = atoi(optarg);
                break;
            case 's':
                memset(server_addr, 0, 32);
                strcpy(server_name, optarg);
                break;
            case '?':
                if (optopt == 's' || optopt == 'p'){
                    fprintf (stderr, "Option -%c requires an argument.\n", optopt);
                }
                else if (isprint (optopt)) {
                    fprintf (stderr, "Unknown option `-%c'.\n", optopt);
                }
                else {
                    fprintf (stderr, "Unknown option character `\\x%x'.\n", optopt);
                }
                return 1;
            default:
                abort ();
        }
    }

    if(name_to_ip(server_name, server_addr) != 0){
        fprintf(stderr, "Error in resolving server address\n");
        return 1;
    }
    int sockfd;
    struct sockaddr_in my_addr;

    int * p_int;
    int err;

    // Create socket
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if(sockfd == -1){
        printf("Error creating socket %d\n",errno);
        return 1;
    }


    // Set socket options
    p_int = (int*)malloc(sizeof(int));
    *p_int = 1;
    if( (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char*)p_int, sizeof(int)) == -1 )||
            (setsockopt(sockfd, SOL_SOCKET, SO_KEEPALIVE, (char*)p_int, sizeof(int)) == -1 ) ){
        printf("Error setting socket options %d\n", errno);
        free(p_int);
        return 1;
    }
    free(p_int);

    // Set server details in socket
    my_addr.sin_family = AF_INET ;
    my_addr.sin_port = htons(server_port);

    memset(&(my_addr.sin_zero), 0, 8);
    my_addr.sin_addr.s_addr = inet_addr(server_addr);

    // Connect to server
    if(connect(sockfd, (struct sockaddr*)&my_addr, sizeof(my_addr)) == -1 ){
        if((err = errno) != EINPROGRESS){
            fprintf(stderr, "Error connecting socket %d %s\n", errno, server_addr);
            return -1;
        }
    }

    read_len = write(sockfd, "Ready!", 7);
    if(read_len < 0){
        fprintf(stderr, "Error writing to socket\n");
    }
    shutdown(sockfd, SHUT_WR);

    memset(buffer, 0, 256);
    read_len = read(sockfd, buffer, 255);
    if(read_len < 0){
        fprintf(stderr, "Error reading from socket\n");
    }
    fprintf(stderr, "%s\n", buffer);
    close(sockfd);
    return 0;
}

