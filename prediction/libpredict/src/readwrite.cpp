#include <random>
#include <assert.h>
#include "readwrite.h"
#include "kiss_fft.h"


double abso(double a)
{
	if (a<0) return -a;
	return a;
}

void getData(double** dataM, int last, int pickwhich)
{
  int numOfModel = 5;
  std::string a;
  char buf[1000];
  for (int i = 1; i <= last; i++)
    {
      if (i<10)
	sprintf(buf, "data//X0%i", i);
      else
	sprintf(buf, "data//X%i", i);
      a = buf;
      printf("%s\n", a.c_str());
      FILE * f = fopen(a.c_str(), "r");
      for (int j = 0; j<2016; j++)
	{
	  for (int k = 0; k<144; k++)
	    {
	      for (int count = 0; count<numOfModel; count++)
		{
		  double tmp;
		  int frlt=fscanf(f, "%lf", &tmp);
		  if (frlt<0)
		    frlt++;
		  if (count == pickwhich)
		    dataM[k][(i - 1) * 2016 + j] = tmp;
		}
	    }
	}
    }
}

void writeDemandMatrix(std::string filename, int row, int col, double ** m, int period, double scale, bool risk, double ** dataM)
{
	//row * col floats
  
	FILE * fActual = fopen(filename.c_str(), "w");
	for (int i = period; i < row; i++)
	{ 
		for (int j = 0; j < col; j++)
		{
			double cur = m[j][i];
			if ((risk) && (cur < dataM[j][i - 1]))
				cur = dataM[j][i - 1];
			fprintf(fActual, "%lf ", cur*2.666666*scale);
		}
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


double probabilityDensity(double x, double mean, double sigma)
{
	return (1.0 / sigma / sqrt(2 * 3.141592653589793238) * exp(-(x - mean)*(x - mean) / 2 / sigma / sigma));
}

double uniondist(double mean)
{
	if (getRandNum(100) < 50)
		return mean*0.8 + mean*0.2*getRandNum(20) / 20;
	else
		return -mean*0.8 - mean*0.2*getRandNum(20) / 20;
}

void generateW(double * x, double mean, int len)
{
	std::default_random_engine generator;
	double sigma = sqrt(mean);
	double sigmaJump = mean / 4;
	std::normal_distribution<double> jumpdistribution(0, sigmaJump);

	x[0] = mean;
	double curP = probabilityDensity(x[0], mean, sigma);

	for (int i = 1; i < len; i++)
	{
		x[i] = x[i - 1];
		double nextX = x[i - 1];
		if (getRandNum(100) <= 1)
			nextX += uniondist(mean);
		else 
			nextX +=jumpdistribution(generator);
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

double cxnorm(kiss_fft_cpx a)
{
	return sqrt(a.r*a.r + a.i*a.i);
}

double uniform_rand(double a, double b)
{
	double range = 100000;
	return a + ((double)getRandNum(int(range))) / range * (b - a);
}

void generateSyntheticData(int row, int hosts, double ** m, std::string prefix, std::string topofile)
{
    char filedir[200];
    sprintf(filedir,"%s%s", prefix.c_str(),"patterns");
    printf("dir=%s!\n", filedir);
	FILE * fPattern = fopen(filedir,"r");
	assert(fPattern != NULL && "Unable to open petterns file");
	int nWeeks;
	int frlt=fscanf(fPattern, "%i", &nWeeks);
	assert(frlt >= 0);
	double ** totflow = new double *[nWeeks];
	for (int curWeek = 0; curWeek < nWeeks;curWeek++)
	{
		totflow[curWeek] = new double[2016];
		for (int i = 0; i < 2016; i++)
			frlt=fscanf(fPattern, "%lf", &totflow[curWeek][i]);
	}

	const int nfft = 2016;
	kiss_fft_cfg cfg = kiss_fft_alloc(nfft, false, 0, 0);
	kiss_fft_cfg cfg2 = kiss_fft_alloc(nfft,true, 0, 0);
	kiss_fft_cpx cx_in[nfft], cx_out[nfft];


	for (int curWeek = 0; curWeek < nWeeks; curWeek++)
	{
		for (int i = 0; i < nfft; i++)
		{
			cx_in[i].r = totflow[curWeek][i];
			cx_in[i].i = 0;
		}
		kiss_fft(cfg, cx_in, cx_out);
		for (int i = 0; i < nfft; i++)
		{
			cx_out[i].r /= nfft;
			cx_out[i].i /= nfft;
		}
		int position[nfft];
		for (int i = 0; i < nfft; i++)
			position[i] = i;

		for (int i = 0; i < nfft - 1; i++)
			for (int j = i + 1; j < nfft; j++)
			{
				double n1 = cxnorm(cx_out[position[i]]);
				double n2 = cxnorm(cx_out[position[j]]);
				if (n1 < n2)
				{
					int tmp;
					tmp = position[i];
					position[i] = position[j];
					position[j] = tmp;
				}
			}
		for (int i = 6; i < nfft; i++)
		{
			double a = uniform_rand(0, 3);
			cx_out[position[i]].r *= a;
			cx_out[position[i]].i *= a;
		}
		kiss_fft(cfg2, cx_out, cx_in);
		for (int i = 0; i < nfft; i++)
			totflow[curWeek][i] = cx_in[i].r;
	}


    /*
	FILE * fFFTout = fopen("fftout", "w");
	for (int curWeek = 0; curWeek < nWeeks; curWeek++)
	{
		for (int i = 0; i < 2016; i++)
			fprintf(fFFTout, "%.2lf  ", totflow[curWeek][i]);
		fprintf(fFFTout, "\n");
	}
	fclose(fFFTout);
    */

	int nMean;
    char filedir2[200];
    sprintf(filedir2,"%s%s", prefix.c_str(),"pareto");
    printf("dir=%s!\n", filedir2);
	FILE * fpareto = fopen(filedir2, "r");
	frlt=fscanf(fpareto, "%i", &nMean);

	double * saveMean = new double[nMean];
	for (int i = 0; i < nMean; i++)
		frlt=fscanf(fpareto, "%lf", &saveMean[i]);

	//m[col][row];
	int pickedTotPattern=0;
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
	double maxDist = 0;
	double ** graph=NULL;
	if (topofile.length() > 1)
	{
		FILE* topof=fopen(topofile.c_str(), "r");
		int topo_n, topo_m;
        int tmp_rlt;
		tmp_rlt=fscanf(topof, "%i%i", &topo_n, &topo_m);
        if (tmp_rlt<0)
          printf("error! in fscanf\n");
		graph = new double *[topo_n];
		for (int i = 0; i < topo_n; i++)
			graph[i] = new double[topo_n];
		for (int i = 0; i < topo_n; i++)
			for (int j = 0; j < topo_n; j++)
				if (i != j)
					graph[i][j] = 214748368;
				else
					graph[i][j] = 0;
		for (int i = 0; i < topo_m; i++)
		{
			int va, vb;
			double edge_len;
			double tmp_rlt=fscanf(topof, "%i%i%lf", &va, &vb, &edge_len);
            if (tmp_rlt<0)
              printf("error! in fscanf\n");
			graph[va][vb] = edge_len;
			graph[vb][va] = edge_len;
		}
		for (int k = 0; k < topo_n; k++)
			for (int i = 0; i < topo_n; i++)
				if (k != i)
					for (int j = 0; j < topo_n; j++)
						if ((k != j) && (i != j))
							if (graph[i][j]>graph[i][k] + graph[k][j])
								graph[i][j] = graph[i][k] + graph[k][j];
		for (int i = 0; i < topo_n; i++)
			for (int j = 0; j < topo_n; j++)
				if (maxDist<graph[i][j])
					maxDist = graph[i][j];
		fclose(topof);
	}

	for (int i = 0; i < row; i++)
	{
		if (i % 2016 == 0)
		{ 
			//pickedTotPattern = getRandNum(nWeeks);
			pickedTotPattern = (i/2016)%20;
			//printf("picked patter week=-------%i-------\n", pickedTotPattern);
		}
		for (int j = 0; j < hosts; j++)
			for (int k = 0; k < hosts; k++)
				if (maxDist == 0)
					m[j*hosts + k][i] = totflow[pickedTotPattern][i % 2016] * Tin[j][i] * Tout[k][i];
				else
					m[j*hosts + k][i] = totflow[pickedTotPattern][i%2016] * Tin[j][i] * Tout[k][i]* exp(-graph[j][k]/maxDist/2);
	}
	fclose(fPattern);
	fclose(fpareto);
	for (int i = 0; i < nWeeks; i++)
		delete[] totflow[i];
	delete[] totflow;
	delete[] saveMean;
	delete[] Tin;
	delete[] Tout;
}

