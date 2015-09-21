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

typedef struct _logger_conf{
    int period; // update interval (seconds)
} logger_conf;

void* handle_client(void*);
void* report_logger(void*);
void usage();

// Total bytes received in logger_conf.period interval
long bytes_rcvd;
pthread_mutex_t br_lock;

int main(int argc, char* argv[]){
    int opt;
    int listen_port = 5000; // default port to listen on
    struct sockaddr_in my_addr;
    int sockfd;
    int tmp_int;
    int* client;
    sockaddr_in sock_addr;
    socklen_t addr_size = sizeof(sockaddr_in);
    pthread_t thread_id=0;

    while ((opt = getopt (argc, argv, "p:")) != -1){
        switch (opt)
        {
            case 'p':
                listen_port = atoi(optarg);
                break;
            case '?':
                if (optopt == 'p')
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

    // bytes received in logger period
    bytes_rcvd = 0;
    pthread_mutex_init(&br_lock, NULL);

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
    if( bind( sockfd, (sockaddr*)&my_addr, sizeof(my_addr)) == -1 ){
        fprintf(stderr,"Error binding to socket %d\n", errno);
        return -1;
    }

    // Start listening for client connections
    if(listen(sockfd, SOMAXCONN) == -1){
        fprintf(stderr, "Error listening %d\n", errno);
        return -1;
    }

    // Spawn logger thread
    logger_conf log_conf;
    log_conf.period = 1;
    pthread_create(&thread_id, 0, &report_logger, &log_conf);
    pthread_detach(thread_id);

    // Wait for clients and process them in individaual threads
    while(true){
        // printf("waiting for a connection\n");
        client = (int*)malloc(sizeof(int)); // will be freed in handler
        if((*client = accept(sockfd, (sockaddr*)&sock_addr, &addr_size))!= -1){
            pthread_create(&thread_id, 0, &handle_client, (void*) client);
            pthread_detach(thread_id);
        }
        else{
            fprintf(stderr, "Error accepting %d\n", errno);
        }
    }

    close(sockfd);
    pthread_mutex_destroy(&br_lock);
    return 0;
}

void* handle_client(void* lp){
    int *client = (int*)lp;

    char buffer[3600];
    int buffer_len = 3600;
    int bytes_read, bytes_written;

    // keep reading while there is data to read
    do{
        memset(buffer, 0, buffer_len);
        if((bytes_read = recv(*client, buffer, buffer_len, 0))== -1){
            fprintf(stderr, "Error receiving data %d\n", errno);
            return 0;
        }
        // printf("Received bytes %d\n", bytes_read);
        // Update total bytes received
        pthread_mutex_lock(&br_lock);
        bytes_rcvd += bytes_read;
        pthread_mutex_unlock(&br_lock);
    } while (bytes_read > 0);

    bytes_written = write(*client, "Done!", 5);
    if(bytes_written < 0){
        fprintf(stderr, "Error in writing to client socket\n");
    }
    shutdown(*client, SHUT_WR);
    close(*client);
    free(client);
    return 0;
}

void* report_logger(void *conf){
    logger_conf* log_conf = (logger_conf*) conf;
    long brcvd;
    int t = 0;
    while(true){
        sleep(log_conf->period);
        pthread_mutex_lock(&br_lock);
        brcvd = bytes_rcvd;
        bytes_rcvd = 0;
        pthread_mutex_unlock(&br_lock);
        fprintf(stdout, "Time: %ds\t Bytes: %ld\tRate: %e bps\n", t, brcvd, (float)brcvd*8/log_conf->period);
	fflush(stdout);
        t += log_conf->period;
    }
    return 0;
}

void usage(){
    fprintf(stdout, "./server <port>\n");
}
