#pragma once
#include <random>
double abso(double a)
{
	if (a<0) return -a;
	return a;
}

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
	return abso((rand() * 59999 + rand()) % n);
}

double getRandExp(double lambda)
{
	int acc=1234567;
	double prob = ((double)getRandNum(acc)) / acc;
	double a = -log(1 - prob) / lambda;
	return a;
}


double probabilityDensity(double x, double mean, double sigma)
{
	return (1.0 / sigma / sqrt(2 * 3.141592653589793238) * exp(-(x - mean)*(x - mean) / 2 / sigma / sigma));
}


void generateW(double * x, double mean, int len)
{
	std::default_random_engine generator;
	double sigma = mean / 2;
	double sigmaJump = mean / 4;
	std::normal_distribution<double> jumpdistribution(0, sigmaJump);

	x[0] = mean;
	double curP = probabilityDensity(x[0], mean, sigma);

	for (int i = 1; i < len; i++)
	{
		x[i] = x[i - 1];
		double nextX = x[i - 1] + jumpdistribution(generator);
		double nextP = probabilityDensity(nextX, mean, sigma);
		if (nextP >= curP)
		{
			curP = nextP;
			x[i] = nextX;
		}
		else
		{
			double u = (0.0+abso(getRandNum(10000))) / 10000;
			if (u < nextP / curP)
			{
				curP = nextP;
				x[i] = nextX;
			}
		}
		if (x[i] < 0)
		{
			x[i] = 0;
			curP = probabilityDensity(0, mean, sigma);
		}
	}
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
	double sumOfMean = 0;
	for (int i = 0; i < nMean; i++)
	{
		fscanf(fPattern, "%lf", &saveMean[i]);
		sumOfMean += saveMean[i];
	}
	sumOfMean /= nMean;
	for (int i = 0; i < nMean; i++)
		saveMean[i] /= (sumOfMean / 1000);

	//m[col][row];
	int pickedTotPattern;
	double ** Tin = new double*[hosts];
	double ** Tout = new double*[hosts];



	for (int i = 0; i < hosts; i++)
	{
		Tin[i] = new double[row];
		Tout[i] = new double[row];
		generateW(Tin[i], saveMean[getRandNum(nMean)], row);
		generateW(Tout[i], saveMean[getRandNum(nMean)], row);
	}
		/*
	for (int i = 0; i < hosts; i++)
	{
		printf("No%i ----------%lf        ", i, Tin[i][0]);
		for (int j = 0; j < 100;j++)
			printf("%.2lf -- ", Tin[i][j]);
		double avg = 0;
		for (int j = 0; j < row; j++)
			avg += Tin[i][j] / row;
		printf("avg == %.2lf\n\n", avg);
	}
			*/
	double tot_in, tot_out;
	for (int i = 0; i < row; i++)
	{
        tot_in = 0;
        tot_out = 0;
		for (int j = 0; j < hosts;j++)
		{
			tot_in += Tin[j][i];
			tot_out += Tout[j][i];
		}
		for (int j = 0; j < hosts; j++)
		{
			Tin[j][i] /= tot_in;
			Tout[j][i] /= tot_out;
		}
	}
	/*
	for (int i = 0; i < 5; i++)
	{
		printf("No%i ---------- \n\n\n", i);
		for (int j = 0; j < 100;j++)
			printf("%.6lf -- ", Tin[i][j]);
	}*/

	for (int i = 0; i < row; i++)
	{
		if (i % 2016 == 0)
		{ 
			pickedTotPattern = getRandNum(nWeeks);
			printf("picked patter week=-------%i-------\n", pickedTotPattern);
		}
		for (int j = 0; j < hosts; j++)
			for (int k = 0; k < hosts; k++)
				m[j*hosts + k][i] = totflow[pickedTotPattern][i] * Tin[j][i] * Tout[k][i];
	}
	fclose(fPattern);
	for (int i = 0; i < nWeeks; i++)
		delete[] totflow[i];
	delete[] totflow,  saveMean, Tin, Tout;
}

