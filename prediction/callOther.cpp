#include <iostream>
#include <stdlib.h>
#include <stdio.h>

int main()
{
    FILE * fsize=fopen("numOfHosts.txt", "r");
    int n;
    system("mkdir matrix");

    while (fscanf(fsize, "%i", &n))
    {
        char command[200];
        sprintf(command, "./libpredict/bin/predict 2 1000 %i matrix/syn-%i 1.0", n, n);
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("*now running %s ********************************************88\n", command);
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        printf("********\n");
        system(command);
    }

    fclose(fsize);
    return 0;

}
