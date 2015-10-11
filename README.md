# PML_Oct
Practical Machine Learning October Course Project

**Background**

One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from Razor inertial measurement units (IMUs) to measure the quality of performance.  These sensors were mounted in the user’s glove, armband, lumbar belt and dumbbell. The moment-to-moment data collected from these sensors were raw accelerometer, gyroscope and magnetometer readings and calculated features on the Euler angles (roll, pitch and yaw), as well. The six subjects were asked to perform one-arm barbell lifts correctly and incorrectly in 5 different ways. 

The goal of this class project is to predict the manner in which six subjects did the bicep curl exercise. The report describes the data and how the data was pre-processed.  A random forest model was built using the caret package in R to predict the quality of the outcome.  The model was cross-validated using ten-fold cross-validation run five times.  

Both the in-sample and out-of-sample error were less than 1 percent with the in-sample error slightly better.  Model itself took about three hours running time on an 8 core processor and about nine hours on a 4 core processor. Because of the high level of accuracy of the final model, other options were not pursued. 

The model predicted correctly the twenty test cases.


