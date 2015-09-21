#include "predict.h"
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

using namespace std;

/*
Read Data:
*/
void getData(double** dataM, int last, int pickwhich)
{
	int numOfModel = 5;
	string a;
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

/*
Compute Median, remove spikes.
*/
void getMedianCol(double ** dataM, double ** medianM, int rows, int curCol, int l)
//One can make it faster by using balanced binary tree, but maybe it's not that important here.
{
	vector<double> v;
	for (int i = 0; i < l; i++)
		medianM[curCol][i] = dataM[curCol][i];
	for (int i = 0; i < l * 2; i++)
		v.push_back(dataM[curCol][i]);

	for (int i = l; i < rows - l; i++)
	{
		v.push_back(dataM[curCol][i + l]);
		std::nth_element(v.begin(), v.begin() + l, v.end());
		medianM[curCol][i] = v[l];
		std::vector<double>::iterator it = std::find(v.begin(), v.end(), dataM[curCol][i - l]);
		v.erase(it);
	}
}

void getMedianTable(double ** dataM, double ** medianM, int rows, int cols, int l)
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
			if (sigma>0)
				proximalUpdate(w, d, sigma*eta);
			for (int j = 0; j < d; j++)
				save[j] += w[j];
		}
		for (int j = 0; j < d; j++)
			wtilde[j] = save[j] / m;
		m = int(m* beta);
		double curObj = objCal(X_dat, Y_dat, d, n, modelPara, additionalStuff);
		printf("%.5lf  eta=%.5lf  w0=%.5lf  w1=%.5lf  w2=%.5lf\n", curObj, eta, w[0], w[1], w[2]);
		if (curObj > lastVal)
		{
			m = n / 4;
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



/*
Logistic regression: */
double logisticRegressionObjCal(double ** X_dat, double * Y_dat, int d, int n, void * modelPara, void * additionalStuff)
{
	double sum = 0;
	double * w = (double *)modelPara;
	for (int i = 0; i < n; i++)
		sum += log(1.0 + exp(-inner(w, X_dat[i], d)*Y_dat[i]));
	sum /= n;
	double sigma = ((double *)additionalStuff)[0];

	for (int i = 0; i < d; i++)
		sum += sigma * abso(w[i]);
	return sum;
}

void logisticRegressionGradientStep(double * x, double y, double *gradAns, int d, bool cumu, void * modelPara, void * additionalStuff)
{
	if (!cumu)
		memset(gradAns, 0, sizeof(double)*d);
	double * w = (double *)modelPara;
	double tmp = -inner(w, x, d)*y;
	tmp = -exp(tmp) / (1.0 + exp(tmp));
	for (int i = 0; i < d; i++)
		gradAns[i] += y*x[i] * tmp;
}

void logisticRegressionTrain(double ** X_dat, double * Y_dat, int d, int n, double avg, void * modelPara, void* additionalStuff)
{
	double * w = (double *)modelPara;
	((double *)additionalStuff)[2] = avg;
	writeRandomDouble(w, d);
	uniVR(X_dat, Y_dat, d, n, logisticRegressionGradientStep, logisticRegressionObjCal, modelPara, additionalStuff);
}

void logisticRegressionPredict(double * x, double * predictY, int d, void * modelPara, void * additionalStuff)
{
	//!This method has a serious bug
	//It can only deal with binary prediction!!
	//Should change it into ordered discretized category
	double * w = (double *)modelPara;
	*predictY = inner(w, x, d);
}

//Train a model
void trainModel(trainModelFunctionType trainMethod,
	double * serve, int numOfFeature, int length, void * modelPara, void * additionalStuff)
{
	if (numOfFeature > length - 1)
	{
		printf("ERROR! Not enough data.\n");
		return;
	}
	double avg = 1.0;
	for (int i = 0; i < length; i++)
		avg += serve[i];
	avg /= length;
	if (avg < 1)
		avg = 1.0;
	int dataLen = length - numOfFeature;
	double ** X_dat = new double*[dataLen];
	double * Y_dat = new double[dataLen];
	for (int i = numOfFeature; i <length; i++)
	{
		X_dat[i - numOfFeature] = new double[numOfFeature + 1];
		X_dat[i - numOfFeature][0] = 1.0;
		double maxV = 1;
		for (int j = 1; j <= numOfFeature; j++)
		{
			X_dat[i - numOfFeature][j] = serve[i - j] / avg;
			if (maxV < X_dat[i - numOfFeature][j])
				maxV = X_dat[i - numOfFeature][j];
		}
		for (int j = 0; j <= numOfFeature; j++)
			X_dat[i - numOfFeature][j] /= maxV;
		Y_dat[i - numOfFeature] = serve[i] / avg/maxV;
	}
	trainMethod(X_dat, Y_dat, numOfFeature + 1, dataLen, avg, modelPara, additionalStuff);
	for (int i = 0; i < dataLen; i++)
		delete[] X_dat[i];
	delete[] X_dat;
    delete[] Y_dat;
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

int goerr()
{
	printf("err!\n");
	return 0;
}

int doit(int argc, char ** argv)
{
	srand(0);

	printf("Menu:\n");
	printf("Command: 1 col w file\n");
	printf("    Example: 1 0 2 abi\n");
	printf("    This means read from first w weeks for abilene data's col-th column.\n");
	printf("    Please ensure data/X01-X0w is in the current directory.\n");
	printf("    Will write the actual data to file.\n");
	printf("    Will write the predicted data to file_predictionAlgName.\n");
	printf("Command: 2 r h file scale\n");
	printf("    Example: 2 2000 3 synthetic_1 2.0\n");
	printf("    This generates r rows of data for h hosts.\n");
	printf("    please choose scale comparing with Abilene data.\n");
	printf("    That is, scale=1.0 if using Abilene, scale=100.0, if using some network with huge traffic.\n");
	printf("    Please ensure 'patterns' file is in the current directory.\n");
	printf("    Will write the actual data to file.\n");
	printf("    Will write the predicted data to file_predictionAlgName.\n");

	bool includeLastOneModel = false;
	bool includeLinearRegressionModel = false;
	bool includeElasticNetRegressionModel = true;

	if (argc < 5) return goerr();
	int dataCode;
	sscanf(argv[1],"%i", &dataCode);
	int col;
	int readFiles = 2;
	int totRow;
	int hosts;
	int period = 1000;
	double scale = 1.0;


	if (dataCode == 1)
	{
		col = 144;
		sscanf(argv[3], "%i", &readFiles);
		totRow = readFiles * 2016;
	}
	else
	{
		sscanf(argv[2], "%i", &totRow);
		totRow += period;
		sscanf(argv[3], "%i", &hosts);
		sscanf(argv[5], "%lf", &scale);
		col = hosts*hosts;
	}
	double ** dataM = new double *[col];
	double ** outM = new double *[col];
	for (int i = 0; i < col; i++)
	{
		dataM[i] = new double[totRow];
		outM[i] = new double [totRow];
	}
	if (dataCode == 1)
	{
		int pickwhich;
		sscanf(argv[2], "%i", &pickwhich);
		getData(dataM, readFiles,pickwhich);
	}
	else
		generateSyntheticData(totRow, hosts, dataM);

	writeDemandMatrix(string(argv[4]), totRow, col, dataM, period, scale);


	
	//Compute patterns:
	//	if (readFiles == 24) computePatterns(dataM);
	//Remove spikes:
	//	getMedianTable(dataM, medianM, totRow, maxCol,3);

	//Possible choices:
	//  lastOnePrediction,
	//	linearRegression, (including elastic net, ridge, lasso) should set sigma/lambda correctly.
	//	logisticRegression, (L1-regularized) should set sigma correctly
	//  random Forest, etc. from openCV ml package. 

	double * additionalStuff = new double[10];
	additionalStuff[0] = 0;  //sigma
	additionalStuff[1] = 0; //lambda
	additionalStuff[2] = 1; // average
	additionalStuff[3] = 0.0002; //eta
	additionalStuff[4] = 2.0; //beta
	additionalStuff[5] = 0.001; //stop Err

	


	if (includeLastOneModel)
	{
		printf("Current ---------------- LastOneModel!\n");
		for (int i = 0; i <col ; i++)
		{
			printf("i=%i  ", i);
			for (int j = 0; j < totRow; j++)
			{
				if (j < period)
					outM[i][j] = dataM[i][(j>1)?(j-1):0];
				else
					outM[i][j] = predictOneModel(lastOnePrediction, dataM[i], 2, j, NULL, additionalStuff);
			}
		}
		writeDemandMatrix(string(argv[4])+string("_lastOne"), totRow, col, outM, period, scale);
	}
	
	int nLinearRegressionFeatures = 10;
	double * linearRegressionW = new double[nLinearRegressionFeatures+1]; //include the constant parameter;
	int trainPeriod = 250;

	if (includeLinearRegressionModel)
	{
		printf("Current ---------------- LinearRegressionModel!\n");
		for (int i = 0; i <col ; i++)
		{
			printf("i=%i  ", i);
			for (int j = 0; j < totRow; j++)
			{
				if (j < period)
					outM[i][j] = dataM[i][(j>1)?(j-1):0];
				else
				{
					if (j%trainPeriod==0)
						trainModel(linearRegressionTrain, &dataM[i][j-trainPeriod], nLinearRegressionFeatures, trainPeriod, linearRegressionW, additionalStuff);
					outM[i][j] = predictOneModel(linearRegressionPredict, dataM[i], nLinearRegressionFeatures, j, linearRegressionW, additionalStuff);
				}
			}
		}
		writeDemandMatrix(string(argv[4])+string("_LinearRegression"), totRow, col, outM, period, scale);
		writeDemandMatrix(string(argv[4])+string("_LinearRegression_riskAverse"), totRow, col, outM, period, scale, true, dataM);
	}

	nLinearRegressionFeatures = 10;
	additionalStuff[0] = 0.2;  //sigma
	additionalStuff[1] = 0.1; //lambda
	if (includeElasticNetRegressionModel)
	{
		printf("Current ---------------- ElasticNetRegressionModel!\n");
		for (int i = 0; i <col ; i++)
		{
			printf("i=%i  ", i);
			for (int j = 0; j < totRow; j++)
			{
				if (j < period)
					outM[i][j] = dataM[i][(j>1)?(j-1):0];
				else
				{
					if (j%trainPeriod==0)
						trainModel(linearRegressionTrain, &dataM[i][j-trainPeriod], nLinearRegressionFeatures, trainPeriod, linearRegressionW, additionalStuff);
					outM[i][j] = predictOneModel(linearRegressionPredict, dataM[i], nLinearRegressionFeatures, j, linearRegressionW, additionalStuff);
				}
			}
		}
		writeDemandMatrix(string(argv[4])+string("_ElasticNetRegression"), totRow, col, outM, period, scale);
		writeDemandMatrix(string(argv[4])+string("_ElasticNetRegression_riskAverse"), totRow, col, outM, period, scale, true, dataM);
	}


	/*
	double ans;

	int nLogisticRegressionFeatures = 10;
	double * logisticRegressionW = new double[nLogisticRegressionFeatures + 1];
	trainModel(logisticRegressionTrain, dataM[0], nLogisticRegressionFeatures, 1000, logisticRegressionW, additionalStuff);
	ans = predictOneModel(logisticRegressionPredict, dataM[0], nLogisticRegressionFeatures, 1000, logisticRegressionW, additionalStuff);
	printf("Logistic, %.5lf\n", ans);
*/

	/*
	int nRandomForestFeatures = 10;
	Ptr<RTrees> rtrees = RTrees::create();
	trainModel(randomForestTrain,dataM[0], nRandomForestFeatures, 3000, &rtrees, additionalStuff);
	for (int i = 1000; i < 1050;i++)
	{ 
		ans=predictOneModel(randomForestPredict, dataM[0], nRandomForestFeatures, i, &rtrees, additionalStuff);
		printf("randomForest, %.5lf actual=%.5lf\n", ans, dataM[0][i]);
	}*/

	for (int i = 0; i < col; i++)
		delete[] dataM[i];
	delete[] dataM;
	delete additionalStuff;
	return 0;
}

