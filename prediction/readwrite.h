#pragma once
void writeDemandMatrix(string filename, int row, int col, double ** m, int period)
{
	//row * col floats
	FILE * fActual = fopen(filename.c_str(), "w");
	for (int i = period; i < row; i++)
	{ 
		for (int j = 0; j < col; j++)
			fprintf(fActual, "%.2lf ", m[j][i]);
		fprintf(fActual, "\n");
	}
	fclose(fActual);
}
void computePatterns(double ** m)
{
	//First number is the number of weekly patterns, 
	//currently set it to be 23, ignore the last one
	//Then, there are 2016 numbers for one week pattern. 
	
	//write it to the file!

	FILE * fPattern = fopen("patterns", "w");

	fprintf(fPattern, "23\n");
	for (int i = 0; i < 23; i++)
	{
		for (int j = 0; j < 2016; j++)
		{
			double sum = 0;
			for (int k = 0; k < 144; k++)
				sum += m[k][j+i*2016];
			sum /= 144;
			fprintf(fPattern, "%.2lf  ", sum);
		}
		fprintf(fPattern, "\n");
	}
	//Then, a number for the distribution of weekly mean value of each host. 
	//There are 23 weeks, so there are 23*144 numbers. 
	fprintf(fPattern, "-1\n");
	fprintf(fPattern, "%i\n", 23 * 144);
	for (int i = 0; i < 23; i++)
		for (int k = 0; k < 144;k++)
		{
			double sum = 0;
			for (int j = 0; j < 2016; j++)
				sum += m[k][j + i * 2016];
			sum /= 2016;
			fprintf(fPattern, "%.2lf ", sum);
		}

	fclose(fPattern);
}
int getRandNum(int n)
{
	return abs((rand() * 59999 + rand()) % n);
}

double getRandExp(double lambda)
{
	int acc=1234567;
	double prob = ((double)getRandNum(acc)) / acc;
	double a = -log(1 - prob) / lambda;
	return a;
}

void generateSyntheticData(int row, int hosts, double ** m)
{
	//return hosts*hosts
	FILE * fPattern = fopen("patterns","r");
	int nWeeks;
	fscanf(fPattern, "%i", &nWeeks);
	double ** totflow = new double *[nWeeks];
	for (int curWeek = 0; curWeek < nWeeks;curWeek++)
	{
		totflow[curWeek] = new double[2016];
		for (int i = 0; i < 2016; i++)
			fscanf(fPattern, "%lf", &totflow[curWeek][i]);
	}
	int nMean;
	fscanf(fPattern, "%*i%i", &nMean);
	double * saveMean = new double[nMean];
	for (int i = 0; i < nMean; i++)
		fscanf(fPattern, "%lf", &saveMean[i]);

	//m[col][row];
	int pickedTotPattern;
	double* meanVal = new double[hosts];
	double * Tin = new double[hosts];
	double * Tout = new double[hosts];
	for (int i = 0; i < hosts; i++)
		meanVal[i] = saveMean[getRandNum(nMean)];
	double tot_in, tot_out;
    tot_in = 0;
    tot_out = 0;
    for (int j = 0; j < hosts;j++)
    {
        Tin[j] = getRandExp(1);
        Tout[j] = getRandExp(1);
        tot_in += Tin[j];
        tot_out += Tout[j];
    }
    for (int j = 0; j < hosts; j++)
    {
        Tin[j] /= tot_in;
        Tout[j] /= tot_out;
    }

	for (int i = 0; i < row; i++)
	{
		if (i % 2016 == 0)
		{ 
			pickedTotPattern = getRandNum(nWeeks);
			printf("picked patter week=-------%i-------\n", pickedTotPattern);
		}
		for (int j = 0; j < hosts; j++)
			for (int k = 0; k < hosts; k++)
				m[j*hosts + k][i] = totflow[pickedTotPattern][i] * Tin[j] * Tout[k];
	}
	fclose(fPattern);
	for (int i = 0; i < nWeeks; i++)
		delete[] totflow[i];
	delete[] totflow, meanVal, saveMean, Tin, Tout;
}

