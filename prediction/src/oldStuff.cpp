#include <string>

#include <cstdlib>
#include <ctime>

//Read the file in my own format. 
int readFile_fast(std::string filename, int * n, int * d, double ** X_dat, double * Y_dat)
{
	srand(0);
	FILE * fin = fopen(filename.c_str(), "r");
	*n = 0;
	*d = 0;
	int totf;
    int frlt=0;

	while (true)
	{
		frlt=fscanf(fin, "%lf", Y_dat + (*n));
		if (Y_dat[*n] < -50) break;
		frlt=fscanf(fin, "%i", &totf);
		for (int i = 0; i<totf; i++)
		{
			int a;
			frlt=fscanf(fin, "%i", &a);
			frlt=fscanf(fin, "%lf", &X_dat[*n][a]);
			if (a > *d)
				*d = a;
		}
		(*n)++;
	}
	(*d)++;
    if (frlt<-1)
        printf("err\n");
	return 0;
}
