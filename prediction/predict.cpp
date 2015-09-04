//++Try the tutorial online for combination of C++ and Ocaml
//++Try to add openCV code + compile the code
//+ Finish neural nets code
#include <stdio.h> 
#include <stdlib.h>
#include <time.h>
#include <string>
#include <algorithm>
#include <vector>
#include "openCVFunctions.h"
#define numOfFile 24
#define rowInFile 2016
#define maxCol 144
#define maxRow rowInFile*numOfFile
#define numOfModel 5
#define penalty 10
using namespace std;
typedef double(*objCalFunctionType)(double * w, double ** X_dat, double * Y_dat, int d, int n, void * additionalStuff);
typedef void(*gradientStepFunctionType) (double *ans, double*w, int i,int d,  bool cumu, double * X, double Y, void * additionalStuff);
typedef double(*predictMethodFunctionType) (double ** X_dat, double* Y_dat, int d, int n, void * additionalStuff);


struct ffNeuralNetsStruct //fully connected, naive feedforward neural nets.
{
	int layers; //input+?hidden+output
	double*** weightMatrix;
	int * numOfUnits;//how many units are there?
};

/*
Read Data:
*/
void getData(double** dataM, int last=numOfFile)
{
	string a;
	char buf[1000];
	for (int i = 1; i <= last; i++)
	{
		if (i<10)
			sprintf_s(buf, "D:\\Dropbox\\box\\CodeRepo\\VisualStudio\\Routing\\data\\X0%i", i);
		else
			sprintf_s(buf, "D:\\Dropbox\\box\\CodeRepo\\VisualStudio\\Routing\\data\\X%i", i);
		a = buf;
		printf("%s\n", a.c_str());
		FILE * f;
		fopen_s(&f, a.c_str(), "r");
		for (int j = 0; j<rowInFile; j++)
		{
			for (int k = 0; k<maxCol; k++)
			{
				for (int count = 0; count<numOfModel; count++)
				{
					double tmp;
					fscanf_s(f, "%lf", &tmp);
					if (count == 1)
						dataM[k][(i - 1) * 2016 + j] = tmp;
				}
			}
		}
	}
}

/*
Compute Median, remove spikes. 
*/
void getMedianCol(double ** dataM, double ** medianM, int rows, int curCol, int l)
//!+ TODO: I can make it much faster!
{
	vector<double> v;
	for (int i = 0; i < l; i++)
		medianM[curCol][i] = dataM[curCol][i];
	for (int i = 0; i < l * 2;i++)
		v.push_back(dataM[curCol][i]);

	for (int i = l; i < rows-l; i++)
	{
		v.push_back(dataM[curCol][i + l]);
		std::nth_element(v.begin(), v.begin() + l, v.end());
		medianM[curCol][i] = v[l];
		auto it = std::find(v.begin(), v.end(), dataM[curCol][i - l]);
		v.erase(it);
	}
}

void getMedianTable(double ** dataM, double ** medianM, int rows, int cols, int l=4)
{
	for (int i = 0; i < cols; i++)
		getMedianCol(dataM, medianM, rows, i, l);
}

/*
General useful functions. 
*/
double sqr(double a)
{
	return a*a;
}

double * getLoss(double * serve, double * predict, int length)
{
	//!TODO: This regret variable should be deleted somewhere..
	double* regret = new double[4];
	for (int i = 0; i < length; i++)
	{
		regret[0] += log(abs(serve[i] - predict[i] + 1.0));
		regret[1] += sqr(log(abs(serve[i] - predict[i] + 1.0)));
		if (predict[i] >= serve[i])
			regret[2] += predict[i];
		else
			regret[2] += serve[i] + (serve[i] - predict[i])*penalty;
		regret[3] += abs(serve[i] - predict[i]);
	}
	return regret;
}


void writeRandomDouble(double * w, int d)
{
	for (int i = 0; i < d; i++)
	{
		w[i] = rand() % 100;
		if (rand() % 2 == 0)
			w[i] = -w[i];
		w[i] = w[i] / 100;
	}
}

void fullGradient(double * utilde, double *w, int d, int n, double ** X_dat, double * Y_dat, 
	gradientStepFunctionType gradientStep, void * additionalStuff)
{
	memset(utilde, 0, sizeof(double)*d);
	for (int i = 0; i < n; i++)
		gradientStep(utilde, w, i, d, true, X_dat[i], Y_dat[i], additionalStuff);
	for (int i = 0; i < d; i++)
		utilde[i] /= n;
}

int getRandNum(int n)
{
	return (rand() * 59999 + rand()) % n;
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
	thres = abs(thres);
	for (int i = 0; i < d; i++)
		if (w[i]>thres)
			w[i] -= thres;
		else if (w[i] < -thres)
			w[i] += thres;
		else w[i] = 0;
}

/*
UniVR: a variance reduced SGD algorithm. 
*/
double* uniVR(double * w, double ** X_dat, double *Y_dat, int d, double eta, int n, double beta, double stopErr,
	gradientStepFunctionType gradientStep, objCalFunctionType objCal, void * additionalStuff)
{
	double sigma = ((double*)additionalStuff)[0];
	double lastVal = -1;
	double* wtilde = new double[d];
	double* utilde = new double[d];
	double * g1 = new double[d];
	double * g2 = new double[d];
	double * save = new double[d];
	int m = n / 4+1;
	memcpy(wtilde, w, sizeof(double)*d);
	while (true)
	{
		memset(save, 0, sizeof(double)*d);
		fullGradient(utilde, wtilde, d, n, X_dat, Y_dat, gradientStep, additionalStuff);
		for (int t = 0; t < m; t++)
		{
			int i;
			i = getRandNum(n);
			gradientStep(g1, w, i, d, false, X_dat[i], Y_dat[i], additionalStuff);
			gradientStep(g2, wtilde, i, d,false, X_dat[i], Y_dat[i], additionalStuff);
			for (int j = 0; j < d; j++)
				w[j] -= eta*(g1[j] - g2[j] + utilde[j]);
			if (sigma>0)
				proximalUpdate(w, d, sigma*eta);
			for (int j = 0; j < d; j++)
				save[j] += w[j];
		}
		for (int j = 0; j < d; j++)
			wtilde[j] = save[j] / m;
		m = int (m* beta);
		double curObj = objCal(w, X_dat, Y_dat, d, n, additionalStuff);
		printf("%.5lf\n", curObj);
		if (abs(curObj - lastVal) < stopErr)
			break;
		lastVal = curObj;
	}
	delete[] wtilde, utilde, g1, g2,save;
	return w;
}

/*
Naive predicting the last value. 
*/
double lastOnePrediction(double ** X_dat, double * Y_dat, int d, int n, void * additionalStuff)
{
	return X_dat[n][1];
}

/*
Linear Regression Code:
*/

double linearRegressionObjCal(double *w, double ** X_dat, double * Y_dat, int d, int n, void * additionalStuff)
{
	double sum = 0;
	for (int i = 0; i < n; i++)
		sum += 0.5 / n* sqr(inner(w, X_dat[i], d) - Y_dat[i]);
	double sigma = ((double *)additionalStuff)[0];
	double lambda = ((double*)additionalStuff)[1];
	for (int i = 0; i < d; i++)
		sum += sigma*abs(w[i]) + lambda / 2 * sqr(w[i]);
	return sum;
}

void linearRegressionGradientStep(double *ans, double*w, int i, int d,bool cumu, double * x, double y, void * additionalStuff)
{
	if (!cumu)
		memset(ans, 0, sizeof(double)*d);
	double lambda = ((double *)additionalStuff)[1];
	double tmp = inner(w, x, d) - y;
	for (int i = 0; i < d; i++)
		ans[i] += x[i] * tmp+lambda*w[i];
}

double linearRegression(double ** X_dat, double * Y_dat, int d, int n, void * additionalStuff)
{
	//! Better compare the results with the results of ipython
	//TODO: this function should be able to automatically change its stepsize. 
	//Here the step size eta should be carefully chosen. Currently it's 0.01
	double eta = 0.01;
	double errStop = 0.00001;
	double sigma = 0;
	double * w = new double[d];
	writeRandomDouble(w, d);
	printf("%.4lf\n", linearRegressionObjCal(w,X_dat, Y_dat, d, n,additionalStuff));
	uniVR(w, X_dat, Y_dat, d, eta, n, 2.0,errStop,linearRegressionGradientStep,linearRegressionObjCal, additionalStuff);
	printf("%.4lf\n", linearRegressionObjCal(w,X_dat, Y_dat, d, n,additionalStuff));
	double output = inner(w, X_dat[n],d);
	delete[] w;
	return output;
}


/*
Logistic regression:
*/
double logisticRegressionObjCal(double *w, double ** X_dat, double * Y_dat, int d, int n, void * additionalStuff)
{
	double sum = 0;
	for (int i = 0; i < n; i++)
		sum += log(1.0+exp(-inner(w, X_dat[i], d)*Y_dat[i]));
	sum /= n;
	double sigma = ((double *)additionalStuff)[0];

	for (int i = 0; i < d;i++)
		sum+= sigma * abs(w[i]);
	return sum;
}

void logisticRegressionGradientStep(double *ans, double*w, int i, int d,bool cumu, double * x, double y, void * additionalStuff)
{
	if (!cumu)
		memset(ans, 0, sizeof(double)*d);
	double tmp = -inner(w, x, d)*y;
	tmp = -exp(tmp) / (1.0 + exp(tmp));
	for (int i = 0; i < d; i++)
		ans[i] += y*x[i] * tmp;
}

double logisticRegression(double ** X_dat, double * Y_dat, int d, int n, void* additionalStuff)
{
	//Here the step size eta should be carefully chosen. Currently it's 0.01
	//Here the L1 regularizer sigma should be carefully chosen. Currently it's 0.01
	//!+This method has a serious bug
	//It can only deal with binary prediction!!
	double sigma = ((double*)additionalStuff)[0];
	double eta = 0.01;
	double errStop = 0.00001;
	double * w = new double[d];
	writeRandomDouble(w, d);
	printf("%.4lf\n", logisticRegressionObjCal(w,X_dat, Y_dat, d, n,additionalStuff));
	uniVR(w, X_dat, Y_dat, d, eta, n, 2.0, errStop, logisticRegressionGradientStep,logisticRegressionObjCal, additionalStuff);
	printf("%.4lf\n", logisticRegressionObjCal(w,X_dat, Y_dat, d, n,additionalStuff));
	//!+bug is here.
	double output = inner(w, X_dat[n],d);
	delete[] w;
	return output;
}


/*
Neural Nets:
*/

double neuralNetsObjCal(double *w, double ** X_dat, double * Y_dat, int d, int n, void * additionalStuff)
{
	double sum = 0;
	return sum;
}

void neuralNetsGradientStep(double *ans, double*w, int i, int d,bool cumu, double * x, double y, void * additionalStuff)
{
	if (!cumu)
		memset(ans, 0, sizeof(double)*d);
	double tmp = -inner(w, x, d)*y;
	tmp = -exp(tmp) / (1.0 + exp(tmp));
	for (int i = 0; i < d; i++)
		ans[i] += y*x[i] * tmp;
}

double neuralNets(double ** X_dat, double * Y_dat, int d, int n, void* additionalStuff)
{
	//boundary decisions:
	//	-100%,-90%, ... 0% ... , 100%
	/*
	double eta = 0.01;
	double errStop = 0.00001;
	double * w = new double[d];
	writeRandomDouble(w, d);
	printf("%.4lf\n", logisticRegressionObjCal(w,X_dat, Y_dat, d, n,additionalStuff));
	uniVR(w, X_dat, Y_dat, d, eta, n, 2.0, errStop, logisticRegressionGradientStep,logisticRegressionObjCal, additionalStuff);
	printf("%.4lf\n", logisticRegressionObjCal(w,X_dat, Y_dat, d, n,additionalStuff));
	//!output should be the maximum of the sum of two neighbor units. For the first one, double it. 
	*/
	//double output = inner(w, X_dat[n],d);
	//delete[] w;
	double output = 0;
	return output;
}

/*
Prediction code, for predicting the next value based on historical value. 
*/
double predictNext(predictMethodFunctionType predictionMethod,
	double * serve, int numOfFeature, int length, void * additionalStuff)
{
	//TODO: current framework is not efficient.
	if (numOfFeature > length - 1)
	{
		printf("ERROR! Not enough data.\n");
		return 0;
	}
	double avg = 0;
	for (int i = 0; i < length; i++)
		avg += serve[i];
	avg /= length;
	int dataLen = length - numOfFeature;
	double ** X_dat = new double*[dataLen+1];
	double * Y_dat = new double[dataLen];
	for (int i = numOfFeature; i <=length; i++)
	{
		X_dat[i - numOfFeature] = new double[numOfFeature+1];
		X_dat[i - numOfFeature][0] = 1;
		for (int j = 1; j <= numOfFeature; j++)
		{
			X_dat[i - numOfFeature][j] = serve[i - j]/avg;
		}
		if (i<length)
			Y_dat[i - numOfFeature] = serve[i]/avg;
	}
	double output=predictionMethod(X_dat,Y_dat, numOfFeature+1,dataLen,additionalStuff)*avg;
	for (int i = 0; i < dataLen + 1; i++)
		delete[] X_dat[i];
	delete[] X_dat, Y_dat;
	return output;
}


int main()
{
	srand(time(NULL));
	printf("size=%i\n", sizeof(int));
	double** dataM = new double *[maxCol];
	double** medianM = new double *[maxCol];
	for (int i = 0; i < maxCol; i++)
	{
		dataM[i] = new double[maxRow];
		medianM[i] = new double[maxRow];
	}
	int readFiles = 1;
	int totRow = readFiles*rowInFile;
	getData(dataM,readFiles);
	
	//Remove spikes:
	//	getMedianTable(dataM, medianM, totRow, maxCol,3);
	
	//Possible choices:
	//  lastOnePrediction,
	//	linearRegression, (including elastic net, ridge, lasso) should set sigma/lambda correctly.
	//	logisticRegression, (L1-regularized) should set sigma correctly

	double * additionalStuff = new double[2];
	additionalStuff[0] = 0.01;  //sigma
	additionalStuff[1] = 0.001; //lambda

	double ans=predictNext(lastOnePrediction, dataM[0], 10, 1000,NULL);
	printf("%.5lf\n",ans);

	ans=predictNext(linearRegression, dataM[0], 10, 1000,additionalStuff);
	printf("%.5lf\n",ans);

	ans=predictNext(logisticRegression, dataM[0], 10, 1000,additionalStuff);
	printf("%.5lf\n",ans);

	
	struct ffNeuralNetsStruct * ffNeuralNetsinfo;
	ffNeuralNetsinfo = new ffNeuralNetsStruct;
	ffNeuralNetsinfo->layers = 3; // only one hidden layer. 
	ffNeuralNetsinfo->numOfUnits = new int[3];
	int featureUsed = 10;
	ffNeuralNetsinfo->numOfUnits[0] = featureUsed; // use 10 previous numbers
	ffNeuralNetsinfo->numOfUnits[1] = 10;			 //10 hidden units
	ffNeuralNetsinfo->numOfUnits[2] = 21;			 //21 output categories (ordered)
	ffNeuralNetsinfo->weightMatrix = new double **[ffNeuralNetsinfo->layers-1]; // fully connected
	for (int i = 0; i < ffNeuralNetsinfo->layers-1; i++)
	{
		//here "+1" means the additional constant term 
		ffNeuralNetsinfo->weightMatrix[i] = new double*[ffNeuralNetsinfo->numOfUnits[i]+1]; 
		for (int j = 0; j < ffNeuralNetsinfo->numOfUnits[i]; j++)
		{
			ffNeuralNetsinfo->weightMatrix[i][j] = new double[ffNeuralNetsinfo->numOfUnits[i+1]];
			for (int k = 0; k < ffNeuralNetsinfo->numOfUnits[i + 1]; k++)
			{
				ffNeuralNetsinfo->weightMatrix[i][j][k] = ((double)(rand() % 1000)) / 1000; //random init weights
				if (rand() % 2 == 0)
					ffNeuralNetsinfo->weightMatrix[i][j][k] *= -1;
			}
		}
	}
	ans=predictNext(neuralNets, dataM[0], featureUsed, 1000,ffNeuralNetsinfo);
	printf("%.5lf\n",ans);

	for (int i = 0; i < maxCol; i++)
	{
		delete[] dataM[i];
		delete[] medianM[i];
	}
	delete[] dataM;
	delete[] medianM;
	delete additionalStuff;
	delete ffNeuralNetsinfo;
	return 0;
}
