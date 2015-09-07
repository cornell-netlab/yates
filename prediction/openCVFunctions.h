#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"
#include "opencv2/ml.hpp"
#include "opencv2/highgui.hpp"
#include <iostream>
#ifdef HAVE_OPENCV_OCL
#define _OCL_KNN_ 1 // select whether using ocl::KNN method or not, default is using
#define _OCL_SVM_ 1 // select whether using ocl::svm method or not, default is using
#include "opencv2/ocl/ocl.hpp"
#endif
using namespace std;
using namespace cv;
using namespace cv::ml;

void newStuff()
{
    vector<Point> trainedPoints;
    vector<int> trainedPointsMarkers;
    trainedPoints.push_back(Point(1,1));
    trainedPoints.push_back(Point(1,0));
    trainedPoints.push_back(Point(0,0));
    trainedPoints.push_back(Point(0,1));

    trainedPointsMarkers.push_back(1);
    trainedPointsMarkers.push_back(1);
    trainedPointsMarkers.push_back(0);
    trainedPointsMarkers.push_back(0);
    Mat samples;
    printf("pts.size()=%i\n",(int)trainedPointsMarkers.size());
    Mat(trainedPoints).reshape(1,(int)trainedPointsMarkers.size()).convertTo(samples,CV_32F);
    Ptr<TrainData> table=TrainData::create(samples, ROW_SAMPLE, Mat(trainedPointsMarkers));
    /*
    printf("%i  %i\n", samples.rows, samples.cols);
    cout<< samples<<endl;
    cout<<Mat(trainedPoints)<<endl;
    samples.at<float>(1,0)= 5;
    cout<< samples<<endl;
    printf("%lf \n", samples.at<float>(1,0));
    */
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

    Mat testSample( 1, 2, CV_32FC1 );
    testSample.at<float>(0) = 0.8;
    testSample.at<float>(1) = 2.0;

    int response = (int)rtrees->predict( testSample );
    printf("res=%i\n",response);

}
