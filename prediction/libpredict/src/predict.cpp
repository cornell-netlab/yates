#include "predict.h"
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
#include "polyfit.h"
#include "openCVFunctions.h"

using namespace std;

void predict_part(std::string filename, int totRow, int col, double ** dataM, int period, double scale, double noiselevel) {

	bool includeLastOneModel = true;
	bool includeFFT = true;
	bool includePolyFit = true;
	bool includeRidgeRegressionModel = true;
	bool includeLassoRegressionModel = true;
	bool includeRandomForest = false;

	double ** outM = new double *[col];
	for (int i = 0; i < col; i++) {
		outM[i] = new double[totRow];
	}

	writeDemandMatrix(filename, totRow, col, dataM, period, scale);
	writeDemandMatrix(filename + string("+notaALG+wthBurnIn"), totRow, col, dataM, 0, scale);

    if (noiselevel>1e-12){
      for (int i = 0; i < col; i++)
          for (int j = 0; j < totRow; j++)
          {
              if (rand() % 2 == 0)
                  outM[i][j] = dataM[i][j]  *(1+ noiselevel*double(getRandNum(10000)) / 10000);
              else 
                  outM[i][j] = dataM[i][j]  *(1- noiselevel*double(getRandNum(10000)) / 10000);
          }
      writeDemandMatrix(filename + string("_noisy"), totRow, col, outM, period, scale);
    }


	//Possible choices:
	//  lastOnePrediction,
	//	linearRegression, (including elastic net, ridge, lasso) should set sigma/lambda correctly.
	//  random Forest, etc. from openCV ml package. 

	double * additionalStuff = new double[10];
	additionalStuff[0] = 0;  //sigma
	additionalStuff[1] = 0; //lambda
	additionalStuff[2] = 1; // average
	additionalStuff[3] = 0.002; //eta
	additionalStuff[4] = 2.0; //beta
	additionalStuff[5] = 1e-3; //stop Err

	if (includeLastOneModel)
	{
		printf("Current ---------------- Last One Model!\n");
		for (int i = 0; i < col; i++)
		{
			double loss = 0;
			double thissum = 0;
			for (int j = 0; j < totRow; j++)
			{
				if (j < period)
					outM[i][j] = dataM[i][(j >= 1) ? (j - 1) : 0];
				else
					outM[i][j] = dataM[i][j - 1];
				loss += abso(outM[i][j] - dataM[i][j]);
				thissum += dataM[i][j];
			}
			printf("Last one model error : %.6lf\n", loss/thissum);
		}
		writeDemandMatrix(filename + string("_lastOne"), totRow, col, outM, period, scale);
	}

	double * linearRegressionW = new double[period + 1]; //include the constant parameter;
	int nLinearRegressionFeatures;


	//enumerate n_feature =10, 25, 50, 100, 250, 500
	//enumerate_lambda= 1e-1, 1e-2, 1e-3, 1e-4, 1e-5,1e-6,1e-7
	//enumerate_ridge= 1e-1, 1e-2, 1e-3, 1e-4, 1e-5,1e-6,1e-7

	int* belongto = new int[totRow];
	int fold = 3;
	for (int i = 0; i < totRow; i++)
		belongto[i] = rand() % fold;

	//ridge
	double **cross_train_x = new double* [totRow];
	//double **cross_test_x = new double* [totRow];

	//int possible_n_f[6] = { 10,25,50,100,250,500 };
	//double possible_reg[7] = { 1e-1, 1e-2, 1e-3, 1e-4, 1e-5,1e-6, 1e-7 };
	int possible_n_f[1] = { 30 };
	double possible_reg[2] = { 1e-3,1e-5};
	//int n_f = 1;
	//int n_reg = 2;
	if (includeRidgeRegressionModel)
	{
		printf("Current ---------------- Ridge Regression!\n");
		for (int curCol = 0; curCol < col; curCol++)
		{
			printf("Doing col %i\n", curCol);
			/*
			printf("Doing cross validation for picking regularizer and n_feature.\n");
			int final_pick[2];
			double best_avg_loss = 100000000;
			for (int pick_feature_iter = 0; pick_feature_iter < n_f; pick_feature_iter++)
				for (int pick_reg_iter = 0; pick_reg_iter < n_reg;pick_reg_iter++)
				{
					additionalStuff[1] = possible_reg[pick_reg_iter]; //lambda
					nLinearRegressionFeatures = possible_n_f[pick_feature_iter];
					printf("Testing n_feature=%i, regularizer=%lf\n", nLinearRegressionFeatures, additionalStuff[1]);
					double avgLoss = 0;
					for (int curfold = 0; curfold < fold; curfold++)
					{
						printf("currrent fold=%i\t", curfold);
						int aggrTrain = 0;
						int aggrTest = 0;
						for (int iter = nLinearRegressionFeatures+1; iter < period;iter++)
							if (belongto[iter] == curfold)
							{
								cross_test_x[aggrTest] = &(dataM[curCol][iter]);
								aggrTest++;
							}
							else
							{
								cross_train_x[aggrTrain] = &(dataM[curCol][iter]);
								aggrTrain++;
							}
						trainModel(linearRegressionTrain, cross_train_x, nLinearRegressionFeatures, aggrTrain, linearRegressionW, additionalStuff);
						double loss = 0;
						double thissum = 0;
						for (int iter = 0; iter < aggrTest; iter++)
						{
							double predicted = predictOneModel(linearRegressionPredict, cross_test_x[iter], nLinearRegressionFeatures, 0, linearRegressionW, additionalStuff);
							loss += abso(predicted-cross_test_x[iter][0]);
							thissum += cross_test_x[iter][0];
						}
						loss /= thissum;
						printf("Cross validation error : %.6lf\n", loss);
						avgLoss += loss;
					}
					printf("avg Loss =%.6lf\n", avgLoss/fold);
					if (avgLoss < best_avg_loss)
					{
						best_avg_loss = avgLoss;
						final_pick[0] = pick_feature_iter;
						final_pick[1] = pick_reg_iter;
					}
				}
			*/
			additionalStuff[1] = possible_reg[0]; //lambda
			nLinearRegressionFeatures = possible_n_f[0];
			for (int iter = 0; iter < period; iter++)
				cross_train_x[iter] = &(dataM[curCol][iter]);
			trainModel(linearRegressionTrain, cross_train_x, nLinearRegressionFeatures, period, linearRegressionW, additionalStuff);

			for (int j = 0; j < totRow; j++)
			{
				if (j < period)
					outM[curCol][j] = dataM[curCol][(j>=1) ? (j - 1) : 0];
				else
					outM[curCol][j] = predictOneModel(linearRegressionPredict, dataM[curCol], nLinearRegressionFeatures, j, linearRegressionW, additionalStuff);
			}
		}
		writeDemandMatrix(filename + string("_RidgeRegression"), totRow, col, outM, period, scale);
		writeDemandMatrix(filename + string("_RidgeRegression_riskAverse"), totRow, col, outM, period, scale, true, dataM);
	}
	if (includeLassoRegressionModel)
	{
		printf("Current ---------------- Lasso Regression!\n");
		for (int curCol = 0; curCol < col; curCol++)
		{
			printf("Doing col %i\n", curCol);
			/*
			printf("Doing cross validation for picking regularizer and n_feature.\n");
			int final_pick[2];
			double best_avg_loss = 100000000;
			for (int pick_feature_iter = 0; pick_feature_iter < n_f; pick_feature_iter++)
				for (int pick_reg_iter = 0; pick_reg_iter < n_reg;pick_reg_iter++)
				{
					additionalStuff[0] = possible_reg[pick_reg_iter]; //sigma
					nLinearRegressionFeatures = possible_n_f[pick_feature_iter];
					printf("Testing n_feature=%i, regularizer=%lf\n", nLinearRegressionFeatures, additionalStuff[1]);
					double avgLoss = 0;
					for (int curfold = 0; curfold < fold; curfold++)
					{
						printf("currrent fold=%i\t", curfold);
						int aggrTrain = 0;
						int aggrTest = 0;
						for (int iter = nLinearRegressionFeatures+1; iter < period;iter++)
							if (belongto[iter] == curfold)
							{
								cross_test_x[aggrTest] = &(dataM[curCol][iter]);
								aggrTest++;
							}
							else
							{
								cross_train_x[aggrTrain] = &(dataM[curCol][iter]);
								aggrTrain++;
							}
						trainModel(linearRegressionTrain, cross_train_x, nLinearRegressionFeatures, aggrTrain, linearRegressionW, additionalStuff);
						double loss = 0;
						double thissum = 0;
						for (int iter = 0; iter < aggrTest; iter++)
						{
							double predicted = predictOneModel(linearRegressionPredict, cross_test_x[iter], nLinearRegressionFeatures, 0, linearRegressionW, additionalStuff);
							loss += abso(predicted-cross_test_x[iter][0]);
							thissum += cross_test_x[iter][0];
						}
						loss /= thissum;
						printf("Cross validation error : %.6lf\n", loss);
						avgLoss += loss;
					}
					printf("avg Loss =%.6lf\n", avgLoss/fold);
					if (avgLoss < best_avg_loss)
					{
						best_avg_loss = avgLoss;
						final_pick[0] = pick_feature_iter;
						final_pick[1] = pick_reg_iter;
					}
				}
			*/
			additionalStuff[0] = possible_reg[1]; //sigma
			nLinearRegressionFeatures = possible_n_f[0];
			for (int iter = 0; iter < period; iter++)
				cross_train_x[iter] = &(dataM[curCol][iter]);
			trainModel(linearRegressionTrain, cross_train_x, nLinearRegressionFeatures, period, linearRegressionW, additionalStuff);

			for (int j = 0; j < totRow; j++)
			{
				if (j < period)
					outM[curCol][j] = dataM[curCol][(j>=1) ? (j - 1) : 0];
				else
					outM[curCol][j] = predictOneModel(linearRegressionPredict, dataM[curCol], nLinearRegressionFeatures, j, linearRegressionW, additionalStuff);
			}
		}
		writeDemandMatrix(filename + string("_LassoRegression"), totRow, col, outM, period, scale);
		writeDemandMatrix(filename + string("_LassoRegression_riskAverse"), totRow, col, outM, period, scale, true, dataM);
	}
	if (includePolyFit) {
		printf("Current ---------------- PolyFit!\n");
		for (int curCol = 0; curCol < col; curCol++)
		{
			for (int iter = 0; iter < period;iter++)
				outM[curCol][iter] = dataM[curCol][(iter>=1) ? (iter - 1) : 0];
			int batch = 5;
			int deg = 3;
			for (int iter = period; iter < totRow; iter++)
				if ((iter-period) %batch==0)
				{
					std::vector<double> x;
					std::vector<double> y;
					for (int i = 0; i < batch; i++) {
						x.push_back(i);
						y.push_back(dataM[curCol][iter - batch + i]);
					}

					std::vector<double> coeff;
					coeff=polyfit(x,y,deg);
					for (int i = 0; i < batch; i++) {
						std::vector<double> guess {double(batch+i)};
						std::vector<double> mg = polyval(coeff, guess);
						outM[curCol][iter + i] = mg[0];
					}
				}
		}
		writeDemandMatrix(filename + string("_PolyFit"), totRow, col, outM, period, scale);
		writeDemandMatrix(filename + string("_PolyFit_riskAverse"), totRow, col, outM, period, scale, true, dataM);
	}

	if (includeFFT)
	{
		printf("Current ---------------- FFT!\n");
		//week = 2016
		//day = 288
		//1 hour = 12
		const int nfft = 288;
		assert(period >= nfft);
		kiss_fft_cfg cfg = kiss_fft_alloc(nfft, false, 0, 0);
		kiss_fft_cfg cfg2 = kiss_fft_alloc(nfft,true, 0, 0);
		kiss_fft_cpx cx_in[nfft], cx_out[nfft];
		for (int curCol = 0; curCol < col; curCol++)
		{
			for (int iter = 0; iter < period;iter++)
				outM[curCol][iter] = dataM[curCol][(iter>=1) ? (iter - 1) : 0];
			for (int iter = period; iter < totRow; iter++)
			{
				for (int i = 0; i < nfft; i++)
				{
					cx_in[i].r = dataM[curCol][iter - nfft+i];
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
				for (int i = 10; i < nfft; i++)
				{
					cx_out[position[i]].r = 0;
					cx_out[position[i]].i = 0;
				}
				kiss_fft(cfg2, cx_out, cx_in);
				outM[curCol][iter] = cx_in[0].r;
			}
		}
		writeDemandMatrix(filename + string("_FFT"), totRow, col, outM, period, scale);
		writeDemandMatrix(filename + string("_FFT_riskAverse"), totRow, col, outM, period, scale, true, dataM);
	}
	if (includeRandomForest) {
		printf("Current ---------------- Random Forest!\n");
		const int nfeature = 50;
		int train_len = 300;
		Mat samples = Mat::zeros(train_len, nfeature, CV_32F);
		Mat ans = Mat::zeros(train_len, 1, CV_32F);

		for (int curCol = 0; curCol < col; curCol++)
		{
			for (int iter = 0; iter < period;iter++)
				outM[curCol][iter] = dataM[curCol][(iter>=1) ? (iter - 1) : 0];
			for (int iter = period; iter < totRow; iter++)
			{
				for (int i = iter - train_len; i < iter; i++) //i is the answer point
				{
					int pos = i - (iter - train_len);
					for (int j = 1; j <= nfeature; j++)  // j is one of the features
						samples.at<float>(pos, j - 1) = dataM[curCol][i - j];
					ans.at<float> (pos,0)=dataM[curCol][i];
				}
				Ptr<TrainData> table = TrainData::create(samples, ROW_SAMPLE, ans);
				Ptr<RTrees> rtrees = RTrees::create();
				rtrees->setMaxDepth(4);
				rtrees->setMinSampleCount(2);
				rtrees->setRegressionAccuracy(0.f);
				rtrees->setUseSurrogates(false);
				rtrees->setMaxCategories(16);
				rtrees->setPriors(Mat());
				rtrees->setCalculateVarImportance(false);
				rtrees->setActiveVarCount(1);
				rtrees->setTermCriteria(TermCriteria(TermCriteria::MAX_ITER, 5, 0));
				rtrees->train(table);

				Mat testSample(1, nfeature, CV_32FC1);
				for (int j = 1; j <= nfeature; j++)
					testSample.at<float>(j) = outM[curCol][iter - j];
				outM[curCol][iter] = rtrees->predict(testSample);
			}
		}
		writeDemandMatrix(filename + string("_RandomForest"), totRow, col, outM, period, scale);
		writeDemandMatrix(filename + string("_RandomForest_riskAverse"), totRow, col, outM, period, scale, true, dataM);

	}
	



	// clean up
	for (int i = 0; i < col; i++) {
		delete (outM[i]);
	}
	delete outM;
	delete belongto;
}



//Command: col w file firstbunchRow scale
//Example: 0 2 abi 1016 1.0
//		   2 3 4   5    6
//This means read from first w weeks for abilene data's col-th column.
//firstbunchRow means only consider the firstbunchRows instead of thousands of them
//please choose scale comparing with Abilene data.
//Please ensure data/X01-X0w is in the current directory.
//Will write the actual data to file.
//Will write the predicted data to file_predictionAlgName.
void mygenerate(int pickwhich, string outputfile, int totRow, double scale, int period, double noiselevel)
{
	srand(0);
	totRow += period;
	int readFiles = totRow / 2016 + 1;
	assert(readFiles <= 23);

	double ** dataM;
	int col = 144;

	assert(totRow < readFiles * 2016);

	dataM = new double *[col];
	for (int i = 0; i < col; i++) {
		dataM[i] = new double[totRow + 2016];
	}

	getData(dataM, readFiles, pickwhich);

	predict_part(outputfile, totRow, col, dataM, period, scale, noiselevel);

	for (int i = 0; i < col; i++) {
		delete (dataM[i]);
	}
	delete dataM;

}


//Command: r h file scale dir
//         2 3 4     5    6
//Example: 2000 3 synthetic_1 2.0
//This generates r rows of data for h hosts.
//please choose scale comparing with Abilene data.
//That is, scale=1.0 if using Abilene, scale=100.0, if using some network with huge traffic.
//Please ensure 'patterns', 'pareto' are in the dir/ directory.
//Will write the actual data to file.
//Will write the predicted data to file_predictionAlgName.
void mysynthetic(string outputfile, int n_host, int totRow, double scale, string datafiledir, int period, double noiselevel, string topofile)
{
	srand(0);

	int col = n_host*n_host;
	totRow += period;

	double ** dataM = new double *[col];
	for (int i = 0; i < col; i++)
		dataM[i] = new double[totRow + 2016];

	generateSyntheticData(totRow, n_host, dataM, datafiledir, topofile);

	predict_part(outputfile, totRow, col, dataM, period, scale, noiselevel);

	for (int i = 0; i < col; i++)
		delete[] dataM[i];
	delete[] dataM;

}
