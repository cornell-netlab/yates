#include "all_algs.h"
#include "readwrite.h"


#include <stdio.h> 
#include <stdlib.h>
#include <time.h>
#include <string>
#include <string.h>
#include <math.h>
#include <algorithm>
#include <vector>
#include <iostream> 
#include <cassert>

using namespace std;

/*
  General useful functions.
*/
double sqr(double a)
{
	return a*a;
}

void  getLoss(double * regret, double * serve, double * predict, int length, int penalty)
{
	for (int i = 0; i < length; i++)
	{
		regret[0] += log(abso(serve[i] - predict[i] + 1.0));
		regret[1] += sqr(log(abso(serve[i] - predict[i] + 1.0)));
		if (predict[i] >= serve[i])
			regret[2] += predict[i];
		else
			regret[2] += serve[i] + (serve[i] - predict[i])*penalty;
		regret[3] += abso(serve[i] - predict[i]);
	}
}

double getRandFloat()
{
	double a = getRandNum(10000);
	if (rand() % 2 == 0)
		a = -a;
	return a / 10000;
}

void writeRandomDouble(double * w, int d)
{
	for (int i = 0; i < d; i++)
		w[i] = getRandFloat();
}

void fullGradient(double ** X_dat, double * Y_dat, double * gradAns, int d, int n,
	gradientStepFunctionType gradientStep, void * modelPara, void * additionalStuff)
{
	memset(gradAns, 0, sizeof(double)*d);
	for (int i = 0; i < n; i++)
		gradientStep(X_dat[i], Y_dat[i], gradAns, d, true, modelPara, additionalStuff);
	for (int i = 0; i < d; i++)
		gradAns[i] /= n;
}


double inner(double *w, double * x, int d)
{
	double sum = 0;
	for (int i = 0; i < d; i++)
		sum += w[i] * x[i];
	return sum;
}

void proximalUpdate(double * w, int d, double thres)
{
	thres = abso(thres);
	for (int i = 0; i < d; i++)
		if (w[i]>thres)
			w[i] -= thres;
		else if (w[i] < -thres)
			w[i] += thres;
		else w[i] = 0;
}

/*
  UniVR: a variance reduced SGD algorithm.
  Currently the function below supports d-dimensional model only.
  In order to support neural nets, I wrote another special one.
*/
void uniVR(double ** X_dat, double * Y_dat, int d, int n,
	gradientStepFunctionType gradientStep, objCalFunctionType objCal, void * modelPara, void * additionalStuff)
{
	double eta = ((double *)additionalStuff)[3];
	double beta = ((double *)additionalStuff)[4];
	double stopErr = ((double *)additionalStuff)[5];
	double sigma = ((double*)additionalStuff)[0];
	double lastVal = 10000000000;
	double* wtilde = new double[d];
	double* utilde = new double[d];
	double * g1 = new double[d];
	double * g2 = new double[d];
	double * save = new double[d];
	double * w = (double *)modelPara;
	int m = n / 4 + 1;
	memcpy(wtilde, w, sizeof(double)*d);
	while (true)
	{
		memset(save, 0, sizeof(double)*d);
		fullGradient(X_dat, Y_dat, utilde, d, n, gradientStep, modelPara, additionalStuff);
		for (int t = 0; t < m; t++)
		{
			int i;
			i = getRandNum(n);
			gradientStep(X_dat[i], Y_dat[i], g1, d, false, modelPara, additionalStuff);
			gradientStep(X_dat[i], Y_dat[i], g2, d, false, (void*)wtilde, additionalStuff);
			for (int j = 0; j < d; j++)
				w[j] -= eta*(g1[j] - g2[j] + utilde[j]);
			if (sigma > 0)
				proximalUpdate(w, d, sigma*eta);
			for (int j = 0; j < d; j++)
				save[j] += w[j];
		}
		for (int j = 0; j < d; j++)
			wtilde[j] = save[j] / m;
		m = int(m* beta);
		double curObj = objCal(X_dat, Y_dat, d, n, modelPara, additionalStuff);
		//printf("%.5lf  eta=%.5lf  w0=%.5lf  w1=%.5lf  w2=%.5lf\n", curObj, eta, w[0], w[1], w[2]);
		if (curObj > lastVal)
		{
			m = n / 4;
			//careful, should not change eta here, if we decide to do the auto tuning. 
			eta = eta / 5;
		}
		if (abso(curObj - lastVal) < stopErr)
			break;
		lastVal = curObj;
	}
	delete[] wtilde;
	delete[] utilde;
	delete[] g1;
	delete[] g2;
	delete[] save;
}


/*
  Naive predicting the last value.
  There is no training
*/
void lastOnePrediction(double * x, double * predictY, int d, void * modelPara, void * additionalStuff)
{
	*predictY = x[1];
}

/*
  Linear Regression Code:
*/


double linearRegressionObjCal(double ** X_dat, double * Y_dat, int d, int n, void * modelPara, void * additionalStuff)
{
	double * w = (double *)modelPara;
	double sum = 0;
	for (int i = 0; i < n; i++)
		sum += 0.5 / n* sqr(inner(w, X_dat[i], d) - Y_dat[i]);
	double sigma = ((double *)additionalStuff)[0];
	double lambda = ((double*)additionalStuff)[1];
	for (int i = 0; i < d; i++)
		sum += sigma*abso(w[i]) + lambda / 2 * sqr(w[i]);
	return sum;
}

void linearRegressionGradientStep(double *x, double y, double * gradAns, int d, bool cumu, void * modelPara, void * additionalStuff)
{
	double *w = (double *)modelPara;
	if (!cumu)
		memset(gradAns, 0, sizeof(double)*d);
	double lambda = ((double *)additionalStuff)[1];
	double tmp = inner(w, x, d) - y;
	for (int i = 0; i < d; i++)
		gradAns[i] += x[i] * tmp + lambda*w[i];
}

void linearRegressionTrain(double ** X_dat, double * Y_dat, int d, int n, double avg, void * modelPara, void * additionalStuff)
{
	double * w = (double *)modelPara;
	((double *)additionalStuff)[2] = avg;
	writeRandomDouble(w, d);
	uniVR(X_dat, Y_dat, d, n, linearRegressionGradientStep, linearRegressionObjCal, modelPara, additionalStuff);
}

void linearRegressionPredict(double * x, double * predictY, int d, void * modelPara, void * additionalStuff)
{
	double * w = (double *)modelPara;
	*predictY = inner(w, x, d);
}




//Train a model
void trainModel(trainModelFunctionType trainMethod,
	double ** serve, int numOfFeature, int length, void * modelPara, void * additionalStuff)
{
	if (numOfFeature > length - 1)
	{
		printf("ERROR! Not enough data.\n");
		return;
	}
	double avg = 1.0;
	for (int i = 0; i < length; i++)
		avg += serve[i][0];
	avg /= length;
	if (avg < 1)
		avg = 1.0;
	int dataLen = length - numOfFeature;
	double ** X_dat = new double*[dataLen];
	double * Y_dat = new double[dataLen];
	for (int i = numOfFeature; i < length; i++)
	{
		X_dat[i - numOfFeature] = new double[numOfFeature + 1];
		X_dat[i - numOfFeature][0] = 1.0;
		double maxV = 1;
		for (int j = 1; j <= numOfFeature; j++)
		{
			X_dat[i - numOfFeature][j] = serve[i][-j] / avg;
			if (maxV < X_dat[i - numOfFeature][j])
				maxV = X_dat[i - numOfFeature][j];
		}
		for (int j = 0; j <= numOfFeature; j++)
			X_dat[i - numOfFeature][j] /= maxV;
		Y_dat[i - numOfFeature] = serve[i][0] / avg / maxV;
	}
	trainMethod(X_dat, Y_dat, numOfFeature + 1, dataLen, avg, modelPara, additionalStuff);
	for (int i = 0; i < dataLen; i++)
		delete X_dat[i];
	delete X_dat;
	delete Y_dat;
}

double predictOneModel(predictNextFunctionType predictMethod, double * serve, int numOfFeature, int length, void * modelPara, void * addtionalStuff)
{
	double * x = new double[numOfFeature + 1];
	double avg = ((double *)addtionalStuff)[2];
	if (avg == 0)
		printf("error! average field is 0.\n");
	x[0] = 1;
	for (int j = 1; j <= numOfFeature; j++)
		x[j] = serve[length - j] / avg;
	double output;
	predictMethod(x, &output, numOfFeature + 1, modelPara, addtionalStuff);
	delete[] x;
	return output*avg;
}
