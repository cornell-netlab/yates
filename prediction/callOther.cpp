#include <iostream>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char ** argv)
{
    FILE * fsize=fopen("numOfHosts.txt", "r");
    int n;
    system("mkdir matrix");
    if (argc<2)
    {
        printf("error! too few args\n");
        return 0;
    }

    while (fscanf(fsize, "%i", &n))
    {
        char command[200];
        sprintf(command, "./libpredict/bin/predict 2 1000 %i matrix/syn-%i 1.0 %s", n, n, argv[1]);
        printf("\n********\n");
        printf("*now running %s \n", command);
        printf("********\n\n\n");
        system(command);
    }

    fclose(fsize);
    return 0;

}
