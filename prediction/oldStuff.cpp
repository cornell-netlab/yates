
//Read the file in my own format. 
int readFile_fast(string filename, int * n, int * d, double ** X_dat, double * Y_dat)
{
	srand(0);
	FILE * fin = fopen(filename.c_str(), "r");
	*n = 0;
	*d = 0;
	int totf;
	while (true)
	{
		fscanf(fin, "%lf", Y_dat + (*n));
		if (Y_dat[*n] < -50) break;
		fscanf(fin, "%i", &totf);
		for (int i = 0; i<totf; i++)
		{
			int a;
			fscanf(fin, "%i", &a);
			fscanf(fin, "%lf", &X_dat[*n][a]);
			if (a > *d)
				*d = a;
		}
		(*n)++;
	}
	(*d)++;
	return 0;
}
