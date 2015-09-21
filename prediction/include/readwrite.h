#pragma once
#include <random>
#include "kiss_fft.h"

double abso(double a);

void writeDemandMatrix(std::string filename,
		       int row, int col,
		       double ** m, int period,
		       double scale, bool risk=false,
		       double ** dataM=NULL);

void computePatterns(double ** m);

int getRandNum(int n);

double getRandExp(double lambda);


double probabilityDensity(double x, double mean, double sigma);

double uniondist(double mean);

void generateW(double * x, double mean, int len);

double cxnorm(kiss_fft_cpx a);

double uniform_rand(double a, double b);

void generateSyntheticData(int row, int hosts, double ** m);
