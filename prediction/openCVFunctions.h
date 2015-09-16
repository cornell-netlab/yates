#include "core/core.hpp"
#include "imgproc/imgproc.hpp"
#include "ml/ml.hpp"
#include "highgui/highgui.hpp"


#include <iostream>
#ifdef HAVE_OPENCV_OCL
#define _OCL_KNN_ 1 // select whether using ocl::KNN method or not, default is using
#define _OCL_SVM_ 1 // select whether using ocl::svm method or not, default is using
#include "opencv2/ocl/ocl.hpp"
#endif
using namespace cv;

/*
Random Forest: */

void randomForestTrain(double ** X_dat, double * Y_dat, int d, int n, double avg, void * modelPara, void * additionalStuff)
{
  
	vector<int> trainedPointsMarkers;
	Mat samples = Mat::zeros(n, d, CV_32F);
	Mat ans = Mat::zeros(n, 1, CV_32F);
	for (int i = 0; i < n; i++)
	{
		for (int j = 0; j < d; j++)
			samples.at<float>(i, j) = X_dat[i][j];
		ans.at<float>(i, 0) = Y_dat[i];
	}

	/*
	Ptr<TrainData> table = TrainData::create(samples, ROW_SAMPLE, ans);
	Ptr<RTrees>* rtrees= (Ptr<RTrees> *) modelPara;
	(*rtrees)->setMaxDepth(4);
	(*rtrees)->setMinSampleCount(2);
	(*rtrees)->setRegressionAccuracy(0.f);
	(*rtrees)->setUseSurrogates(false);
	(*rtrees)->setMaxCategories(16);
	(*rtrees)->setPriors(Mat());
	(*rtrees)->setCalculateVarImportance(false);
	(*rtrees)->setActiveVarCount(1);
	(*rtrees)->setTermCriteria(TermCriteria(TermCriteria::MAX_ITER, 5, 0));
	(*rtrees)->train(table);
	*/
}
void randomForestPredict(double * x, double * predictY, int d, void * modelPara, void * additionalStuff)
{
  /*
  Ptr<RTrees> *rtrees= (Ptr<RTrees>* ) modelPara;
  
  Mat testSample(1, d, CV_32FC1);
  for (int j = 0; j < d; j++)
    testSample.at<float>(j) = x[j];
  
  *predictY = (*rtrees)->predict(testSample);
  */
}
