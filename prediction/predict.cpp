//how to add it to ocaml?
#include <stdio.h> 
#include <stdlib.h>
#include <time.h>
#include <string>
#include <string.h>
#include <math.h>
#include <algorithm>
#include <vector>
#include "openCVFunctions.h"
#define numOfFile 24
#define rowInFile 2016
#define maxCol 144
#define maxRow rowInFile*numOfFile
typedef double(*objCalFunctionType)(double ** X_dat, double * Y_dat, int d, int n, void * modelPara, void * additionalStuff);
typedef void (*gradientStepFunctionType) (double * x, double y, double *gradAns, int d,  bool cumu, void * modelPara, void * additionalStuff);
typedef void (*trainModelFunctionType) (double ** X_dat, double* Y_dat, int d, int n, double avg, void * modelPara, void * additionalStuff);
typedef void (*predictNextFunctionType) (double * x, double * predictY, int d,  void * modelPara , void * additionalStuff);

struct ffNeuralNetsStruct //fully connected, naive feedforward neural nets.
{
	int layers; //input+?hidden+output
	double*** weightMatrix;
	int * numOfUnits;//how many units are there?
};

/*
double abs(double a)
{
  if (a<0) return -a;
  return a;
}*/

/*
Read Data:
*/
void getData(double** dataM, int last=numOfFile)
{
	int numOfModel = 5;
	string a;
	char buf[1000];
	for (int i = 1; i <= last; i++)
	{
		if (i<10)
			sprintf(buf, "D:\\Dropbox\\box\\CodeRepo\\VisualStudio\\Routing\\data\\X0%i", i);
		else
			sprintf(buf, "D:\\Dropbox\\box\\CodeRepo\\VisualStudio\\Routing\\data\\X%i", i);
		a = buf;
		printf("%s\n", a.c_str());
		FILE * f=fopen( a.c_str(), "r");
		for (int j = 0; j<rowInFile; j++)
		{
			for (int k = 0; k<maxCol; k++)
			{
				for (int count = 0; count<numOfModel; count++)
				{
					double tmp;
					fscanf(f, "%lf", &tmp);
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
//One can make it faster by using balanced binary tree, but maybe it's not that important here.
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
        std::vector<double>::iterator it = std::find(v.begin(), v.end(), dataM[curCol][i - l]);
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

void  getLoss(double * regret, double * serve, double * predict, int length, int penalty)
{
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
}

int getRandNum(int n)
{
	return (rand() * 59999 + rand()) % n;
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
		gradientStep(X_dat[i], Y_dat[i], gradAns, d, true,modelPara,additionalStuff);
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
	int m = n / 4+1;
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
			gradientStep(X_dat[i], Y_dat[i], g2, d, false, (void* ) wtilde, additionalStuff);
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
		double curObj = objCal(X_dat, Y_dat, d, n, modelPara, additionalStuff);
		printf("%.5lf  eta=%.5lf  w0=%.5lf  w1=%.5lf  w2=%.5lf\n", curObj, eta, w[0], w[1], w[2]);
		if (abs(curObj - lastVal) < stopErr)
			break;
		lastVal = curObj;
	}
	delete[] wtilde, utilde, g1, g2,save;
}

//This is for neural nets only. 
//!+TODO
void uniVR_NN()
{
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


double linearRegressionObjCal(double ** X_dat, double * Y_dat,  int d, int n, void * modelPara, void * additionalStuff)
{
	double * w = (double *) modelPara;
	double sum = 0;
	for (int i = 0; i < n; i++)
		sum += 0.5 / n* sqr(inner(w, X_dat[i], d) - Y_dat[i]);
	double sigma = ((double *)additionalStuff)[0];
	double lambda = ((double*)additionalStuff)[1];
	for (int i = 0; i < d; i++)
		sum += sigma*abs(w[i]) + lambda / 2 * sqr(w[i]);
	return sum;
}

void linearRegressionGradientStep(double *x, double y, double * gradAns, int d,bool cumu, void * modelPara, void * additionalStuff)
{
	double *w = (double *)modelPara;
	if (!cumu)
		memset(gradAns, 0, sizeof(double)*d);
	double lambda = ((double *)additionalStuff)[1];
	double tmp = inner(w, x, d) - y;
	for (int i = 0; i < d; i++)
		gradAns[i] += x[i] * tmp+lambda*w[i];
}

void linearRegressionTrain(double ** X_dat, double * Y_dat, int d, int n, double avg, void * modelPara, void * additionalStuff)
{
	double * w = (double *)modelPara;
	((double *)additionalStuff)[2] = avg;
	writeRandomDouble(w, d);
	uniVR(X_dat, Y_dat, d,n, linearRegressionGradientStep,linearRegressionObjCal, modelPara, additionalStuff);
}

void linearRegressionPredict(double * x, double * predictY, int d, void * modelPara, void * additionalStuff)
{
	double * w = (double *)modelPara;
	*predictY = inner(w, x, d);
}



/*
Logistic regression: */
double logisticRegressionObjCal(double ** X_dat, double * Y_dat,  int d, int n, void * modelPara, void * additionalStuff)
{
	double sum = 0;
	double * w = (double *)modelPara;
	for (int i = 0; i < n; i++)
		sum += log(1.0+exp(-inner(w, X_dat[i], d)*Y_dat[i]));
	sum /= n;
	double sigma = ((double *)additionalStuff)[0];

	for (int i = 0; i < d;i++)
		sum+= sigma * abs(w[i]);
	return sum;
}

void logisticRegressionGradientStep(double * x, double y, double *gradAns,  int d,bool cumu, void * modelPara, void * additionalStuff)
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
	uniVR(X_dat, Y_dat, d,n, logisticRegressionGradientStep,logisticRegressionObjCal, modelPara, additionalStuff);
}

void logisticRegressionPredict(double * x, double * predictY, int d, void * modelPara, void * additionalStuff)
{
	//!This method has a serious bug
	//It can only deal with binary prediction!!
	//Should change it into ordered discretized category
	double * w = (double *)modelPara;
	*predictY = inner(w, x, d);
}

/*
void randomForestTrain(double * x, double * preditcY, int d, void * modelPara, void * additionalStuff)
{
    Ptr<RTrees> rtrees= (Ptr<RTrees> ) modelPara;
    rtrees->setMaxDepth(4);
    rtrees->setMinSampleCount(2);
    rtrees->setRegressionAccuracy(0.f);
    rtrees->setUseSurrogates(false);
    rtrees->setMaxCategories(16);
    rtrees->setPriors(Mat());
    rtrees->setCalculateVarImportance(false);
    rtrees->setActiveVarCount(1);
    rtrees->setTermCriteria(TermCriteria(TermCriteria::MAX_ITER, 5, 0));
    rtrees->train(prepare_train_data());
}
*/


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



//Train a model
void trainModel(trainModelFunctionType trainMethod,
	double * serve, int numOfFeature, int length, void * modelPara, void * additionalStuff)
{
	if (numOfFeature > length - 1)
	{
		printf("ERROR! Not enough data.\n");
		return;
	}
	double avg = 0;
	for (int i = 0; i < length; i++)
		avg += serve[i];
	avg /= length;
	int dataLen = length - numOfFeature;
	double ** X_dat = new double*[dataLen];
	double * Y_dat = new double[dataLen];
	for (int i = numOfFeature; i <length; i++)
	{
		X_dat[i - numOfFeature] = new double[numOfFeature+1];
		X_dat[i - numOfFeature][0] = 1.0;
		for (int j = 1; j <= numOfFeature; j++)
			X_dat[i - numOfFeature][j] = serve[i - j]/avg;
		Y_dat[i - numOfFeature] = serve[i]/avg;
	}
	trainMethod(X_dat,Y_dat, numOfFeature+1,dataLen,avg, modelPara, additionalStuff);
	for (int i = 0; i < dataLen; i++)
		delete[] X_dat[i];
	delete[] X_dat, Y_dat;
}

double predictModel(predictNextFunctionType predictMethod, double * serve, int numOfFeature, int length, void * modelPara, void * addtionalStuff)
{
	double * x = new double[numOfFeature + 1];
	double avg = ((double *)addtionalStuff)[2];
	if (avg==0)
		printf("error! average field is 0.\n");
	x[0] = 1;
	for (int j = 1; j <= numOfFeature; j++)
		x[j] = serve[length - j] / avg;
	double output;
	predictMethod(x, &output, numOfFeature + 1, modelPara, addtionalStuff);
	delete[] x;
	return output*avg;
}

int readFile_fast(string filename, int * n, int * d , double ** X_dat, double * Y_dat)
{
	srand(0);
	FILE * fin=fopen(filename.c_str(), "r");
	*n = 0;
	*d = 0;
	int totf;
	while (true)
	{
		fscanf(fin, "%lf", Y_dat+(*n));
		if (Y_dat[*n] < -50) break;
		fscanf(fin, "%i", &totf);
		for (int i = 0; i<totf; i++)
		{
			int a;
			double b;
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


int main()
{
	struct aa
	{
		double a;
		double b;
		double c;
		double d;
		double e[5];
	} * f;
	f = new struct aa;
	f->a = 12345;
	f->b = 67890;
	f->c = 135;
	f->d = 5;
	double* g = (double *)f;
	
	printf("%lf  %lf  %lf %lf\n", g[0], g[1], g[2], g[3]);

	srand(0);
	printf("size=%i\n", (int)sizeof(int));
    newStuff();
    return 0;

	string filename = "a9a2.dat";
	//int n, d;
	int maxn = 40000;
	int maxd = 200;

	double * additionalStuff = new double[10];
	additionalStuff[0] = 0;  //sigma
	additionalStuff[1] = 0; //lambda
	additionalStuff[2] = 1; // average
	additionalStuff[3] = 0.001; //eta
	additionalStuff[4] = 2.0; //beta
	additionalStuff[5] = 0.000001; //beta
	int nLinearRegressionFeatures = 100;
	double * linearRegressionW = new double[nLinearRegressionFeatures];
	memset(linearRegressionW, 0, sizeof(double)*nLinearRegressionFeatures);
	/*
	double ** X_dat=new double * [maxn];
	for (int i = 0; i < maxn; i++)
	{
		X_dat[i] = new double[maxd];
		memset(X_dat[i],0,sizeof(double)*maxd);
	}
	double * Y_dat;
	Y_dat = new double [maxn];
	readFile_fast(filename, &n, &d, X_dat, Y_dat);
	linearRegressionTrain(X_dat,Y_dat,d,n,1.0,linearRegressionW,additionalStuff);
	*/





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


	double ans =predictModel(lastOnePrediction, dataM[0], 100, 1000, NULL, additionalStuff);
	printf("%.5lf\n",ans);

	
	
	int totalLen = 1099;
	double * series;
	series = dataM[0];

	trainModel(linearRegressionTrain, series, nLinearRegressionFeatures, totalLen,linearRegressionW,additionalStuff);

	ans = predictModel(linearRegressionPredict, series, nLinearRegressionFeatures, totalLen+1, linearRegressionW, additionalStuff);
	printf("%.5lf\n",ans);

	int nLogisticRegressionFeatures = 10;
	double * logisticRegressionW = new double[nLogisticRegressionFeatures+1];
	trainModel(logisticRegressionTrain, dataM[0], nLogisticRegressionFeatures, 1000,logisticRegressionW,additionalStuff);
	ans = predictModel(logisticRegressionPredict, dataM[0], nLogisticRegressionFeatures, 1000, logisticRegressionW, additionalStuff);
	printf("%.5lf\n",ans);




    /*
    int nRandomForestFeatures=2;
    for (int i=0;i<1000-nRandomForestFeatures;i++)

    Ptr<RTrees> rtrees;
    trainModel(randomForestTrain,dataM[0], nRandomForestFeatures, 1000, rtrees, additionalStuff);
    ans=predictModel(randomForestPredict, dataM[0], nRandomForestFeatures, 1000, rtrees, additionalStuff);
    */




	
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
	//ans=trainModel(neuralNets, dataM[0], featureUsed, 1000,ffNeuralNetsinfo);
	//printf("%.5lf\n",ans);

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
