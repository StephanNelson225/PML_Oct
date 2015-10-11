---
title: "QualitativeActivityRecognitionProject"
author: "Steve Nelson"

output: html_document
---



```{r, echo=FALSE, results='hide'}
startTime<-Sys.time()
startTime
```



```{r, echo=FALSE, warning=FALSE, message=FALSE, results='hide'}
library(doParallel)
detectCores()
cl<-makeCluster(6)
registerDoParallel(cl)
getDoParWorkers()
#source - https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
```
### Qualitative Activity Recognition


#### Synopsis
The goal of the project is to predict which of five techniques a participant applies when performing a bicep curl. One technique of performance is correct while the other four techniques are incorrect. 

The data exploration and subsequent preprocess of the data were very important.  Holes were discovered in the data and were filled through preprocessing. Whole sections of data that were zeroed out were imputed.  All of the data was centered and scaled. 

The alogithm used in this analysis was the "Random Forest" method in the caret package.  The data was cross-validated via 10 fold cross-validation run five times.  

The resulting prediction was over 99.206 percent accurate on training data and 99.205 percent accurate on the test data.  The testing data was surprisingly close to having less error than the training data.  Subtracting there errors from 100 percent, the measure of the in-sample error is 0.794 percent while the out-of-sample error is just 0.795 percent. 
 
When using the prediction algorithm developed here, the 20 test cases were all predicted correctly. 

Thanks are expressed to the developer of this data.  Source: "Qualitative Activity Recognition of Weight Lifting Exercises", Eduardo Velloso, Lancaster University,Lancaster, UK; Andreas Bulling,  Max Planck Institute for Informatics Saarbrücken, Germany, Hans Gellersen, Lancaster University,Lancaster, UK,  Wallace Ugulino, Pontifical Catholic University of Rio de Janeiro Rio de Janeiro, Brazil, Hugo Fuks,Pontifical Catholic University of Rio de Janeiro, Rio de Janeiro, Brazil.

### Purpose of Research
The purpose of this project is to predict which flaw, if any, a test subject may have in their technique for bicep curls.  The researchers have defined a class of motions that result in properly executed curls and have also identified sets of motions where curls are executed improperly.  The "classe" variable in the data set identifies what you might call the quality of the performance.  A curl can be performed:

classe "A"  Correctly 

classe "B"  Incorrectly: throwing elbows to the front  

classe "C"  Incorrectly: lifting the dumbbell only halfway

classe "D"  Incorrectly: lowering the dumbbell only halfway 

classe "E"  Incorrectly: throwing the hips to the front    

In developing this dataset, participants were supervised by an experienced weightlifter
to make sure the execution complied to the techniques to be simulated. The exercises were performed by six male participants aged between 20-28 years, with little
weight lifting experience. 

#### Detection of Mistakes in Exercises

The goal of experiment was to assess whether correct lifting technique and specific mistakes could be detected in weight-lifting exercises by using activity recognition techniques. Six users were measured with wearable sensors performing the same bicep curl activity correctly and with the set of four common mistakes.  Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl with the five different techniques.

The signals from four 9-degree of freedom Razor inertial measurement units (IMUs) were used.  These sensors were mounted in the user's glove, armband, lumbar belt and dumbbell. The moment-to-moment data collected from these sensors were raw accelerometer, gyroscope and magnetometer readings and calculated features on the Euler angles (roll, pitch and yaw), as well. "[Euler Angles,https://en.wikipedia.org/wiki/Euler_angles]" 


### Data Exploration

The data frame was 19,622 rows by 160 columns.  In general, the rows referred to the sensor observations at a specific moment in time as measured in tiny fractions of a second within an activity window. The time measures were measured in 858 activity windows. The activity window appears to start as the subject raises the dumbell from a half-up position and ends when the dumbell is returned to that position after first reaching a point when the bicep muscle is fully contracted.

There were thirty-eight measures for each of the sensor locations: arm, belt, forearm and dumbbell.  Thirteen measures for each of the locations appear to be raw moment-to-moment data while twenty-five measures are summary measures for each of these data variables.  These summary values are only calculated once over the length of the activity window.


####  Overview  
Reviewing the observations for each combination of subject and classe, we find that subjects Adelmo and Jeremy provided the most observation while the observations from Pedro were the fewest.  Classe "A", representing correct lifting had the most observations; classe "D" representing partial lowering of the dumbell was represented the least.   


```{r, echo=FALSE}
bicep<-read.csv("bicep.csv", stringsAsFactors=FALSE, header=TRUE)
a<-table(bicep$classe, bicep$user_name)
addmargins(a)
#Rersource: http://stackoverflow.com/questions/14991476/find-the-non-zero-values-and-frequency-of-those-values-in-r


```


#### Which Variables to Keep as Predictors
The desired model will predict performance quality at a particular moment of time rather predicting the quality of performance over the activity window.  Thus, the variables related to identifying the activity windows such as date, time, timestamp and window number are not necessary for the model design data set.   

Because the summary measure discussed above only appeared at the end of a each window, there are many missing values or na's in these columns.  Since they are not raw data, these 100 measures are not included in the model design data set. 

The variables that will be kept include the thirteen raw measures for each of the four sensors and the classe variable, for a total of 53 variables. Their names are shown below.


```{r, eval=TRUE, echo=FALSE}
fewer<- as.vector(c(8:11, 37:49, 60:68,84:86,102,113:124,140,151:160))# subset to relevant variables
p_bicep<-bicep[,fewer]
p_bicep$classe<-as.factor(p_bicep$classe)
names(p_bicep)

```


###Data Preprocessing

The first step in preprocessing was to eliminate the variables that weren't of value to the modeling, the seven identifier variables at the start of the data set and the 100 [4 X 25] other unnecessary variables.  This was accomplished by simple subsetting.

Next, there was evidence of systematic flaws in the raw data.  This is evident in the heatmap below (Figure 1). In particular, values for subject Jeremy and Adelmo in several variables suggest sensor failure or transcription error.  Otherwise there would be signals other than zero at some point in each of the windows for those subjects.  These zero values are modified with the "sample function" which calculates the mean and standard deviation of the non-zero values and randomly applies them to the values that are zero.


    
Finally, the preProcess function in caret is used to center and scale all of the variables.


#### Heatmap Highlighting Zero Values
Many zero values in variables show no distinct pattern but there is a definite pattern to several variables.
```{r, "heatmap", fig.height=7, fig.width=12, message=FALSE, echo=FALSE, cache=TRUE}
library(ggplot2)
library(cutoffR)
bicep<-read.csv("bicep.csv", stringsAsFactors=FALSE, header=TRUE)
fewerZ<- as.vector(c(2, 7, 8:11, 37:49, 60:68,84:86,102,113:124,140,151:160))#need to remove 2 and 7 for subsequent analysis.
p_bicep<-bicep[,fewerZ]
aa<-as.vector(names(p_bicep))
p_bicep[p_bicep==0]<-NA # makes them missing values
aa<-as.vector(aa) 
#function to assemble missing value data

sqr = seq(1:55)
count = NULL
for (i in 1:55)
{
count[i] =nmissing(p_bicep[,i])
}

MissingValue<-as.data.frame(count)
row.names(MissingValue)<-aa
names(MissingValue)="Count of Zero Values"

miss<-p_bicep [order(p_bicep$user_name, p_bicep$classe, p_bicep$num_window),] #order the observations to visually align the zero values

h<-HeatStruct(miss[,3:54], high.col = "steelblue", low.col = "white",  
               missing.col = "gold", xlab = "", ylab = "") + 
               theme(axis.ticks = element_blank(), axis.text.x = 
               element_text(angle=90, vjust=0.5, hjust=1, size = 7),axis.text.y = element_blank()) +
               labs(title = "Figure 1: Heatmap of Scaled Values Highlighting in Gold Sequences \nof Zero Values:by variable with Identification of Users") +
              geom_abline(intercept=3892)+ geom_abline(intercept=7004)+ 
              geom_abline(intercept=10540)+ geom_abline(intercept=13610)+ 
              geom_abline(intercept=17012) +
              geom_text(data = NULL, x = 50, y = 1946, label = "Adelmo") +
              geom_text(data = NULL, x = 50, y = 5448, label = "Carlitos") +
              geom_text(data = NULL, x = 50, y = 8772, label = "Charles") + 
              geom_text(data = NULL, x = 50, y = 12075, label = "Eurico") + 
              geom_text(data = NULL, x = 50, y = 15331, label = "Jeremy") +
              geom_text(data = NULL, x = 50, y = 18317, label = "Pedro") 
            
h

#http://stackoverflow.com/questions/14991476/find-the-non-zero-values-and-frequency-of-those-values-in-r
```




As can be seen from the annotated Heatmap above, roll_forearm, pitch_forearm, and yaw_forearm have large number of zero values as do roll_arm, pitch_arm and yaw_arm.  These are in a consistent string of zero values. The "_arm" strings represent zero readings from Jeremy's bicep curl efforts and the "_forearm" strings represent zero readings from Adelmo's efforts.  These are shown as the longer segments of gold.   

Less consistent strings of gold zero values are seen in three user level locations reflect the higher number of zeros from gyros-belt x, y and z. Other zero values measures seem more or less random.

####  Imputation. 
These omissions were corrected by use of the sample function in base R.  The annotated code is shown in the chunk below. 

```{r, eval=FALSE}
#First, zeros have been turned into NAs.   
imputed$roll_arm[which(imputed$roll_arm==0)] <-NA

#Then, this code identifies the roll_arm values that are NA,    

imputed$roll_arm[is.na(imputed$roll_arm)]

#then the sample function samples from the values that were not NAs and replaces the NAs (zeros)     
#with the randomly generated value for each of the NAs in roll_arm.

<-sample(imputed$roll_arm[!is.na(imputed$roll_arm)], sum(is.na(imputed$roll_arm)),TRUE) 

#Then this code assigns those values to the analysis data frame.
bicep$roll_arm<-imputed$roll_arm
# These steps are repeated for the five other variables.    

```



```{r, "zero group", echo=FALSE, results='hide'}
bicep<-read.csv("bicep.csv", stringsAsFactors=FALSE, header=TRUE)

zerogroup<- as.vector(c(46:48,122:124))
zerogroup

feature<-bicep[,zerogroup] # isolates the subject variables 
dim(feature)
summary(feature)
#sources
#http://topepo.github.io/caret/preprocess.html
#http://www.r-bloggers.com/data-imputation-i/
#http://stackoverflow.com/questions/11971876/how-to-fill-na-with-median
#http://thomasleeper.com/Rcourse/Tutorials/NAhandling.html
#http://adv-r.had.co.nz/Functional-programming.html#
```



```{r, "ImputStep1", echo=FALSE, results='hide'}
#setting zeros to NA
summary(feature) # distribution prior to NAs
feature$roll_arm[which(feature$roll_arm==0)] <-NA
feature$pitch_arm[which(feature$pitch_arm==0)] <-NA
feature$yaw_arm[which(feature$yaw_arm==0)] <-NA
feature$roll_forearm[which(feature$roll_forearm==0)] <-NA
feature$pitch_forearm[which(feature$pitch_forearm==0)] <-NA
feature$yaw_forearm[which(feature$yaw_forearm==0)] <-NA
dim(feature)
summary(feature) # distribution after NAs

```

```{r, "Imput step2 sample", echo=FALSE, results='hide'}
#imputation strategy - random imputation- function
#source-http://thomasleeper.com/Rcourse/Tutorials/NAhandling.html
set.seed(1222)
feature$roll_arm[is.na(feature$roll_arm)]<-sample(feature$roll_arm[!is.na(feature$roll_arm)], sum(is.na(feature$roll_arm)),TRUE)
feature$pitch_arm[is.na(feature$pitch_arm)]<-sample(feature$pitch_arm[!is.na(feature$pitch_arm)], sum(is.na(feature$pitch_arm)),TRUE)
feature$yaw_arm[is.na(feature$yaw_arm)]<-sample(feature$yaw_arm[!is.na(feature$yaw_arm)], sum(is.na(feature$yaw_arm)),TRUE)
feature$roll_forearm[is.na(feature$roll_forearm)]<-sample(feature$roll_forearm[!is.na(feature$roll_forearm)], sum(is.na(feature$roll_forearm)),TRUE)
feature$pitch_forearm[is.na(feature$pitch_forearm)]<-sample(feature$pitch_forearm[!is.na(feature$pitch_forearm)], sum(is.na(feature$pitch_forearm)),TRUE)
feature$yaw_forearm[is.na(feature$yaw_forearm)]<-sample(feature$yaw_forearm[!is.na(feature$yaw_forearm)], sum(is.na(feature$yaw_forearm)),TRUE)
summary(feature)
```

```{r, "Imput final step", echo=FALSE, results='hide'}
summary(bicep)[,c(46:48,122:124)] # distribution prior to NAs
#replaces actual data with imputted data

bicep$roll_arm<-feature$roll_arm
bicep$pitch_arm<-feature$pitch_arm
bicep$yaw_arm<-feature$yaw_arm
bicep$roll_forearm<-feature$roll_forearm
bicep$pitch_forearm<-feature$pitch_forearm
bicep$yaw_forearm <-feature$yaw_forearm

summary(bicep)[,c(46:48,122:124)]

```


```{r, echo=FALSE,message=FALSE,warning=FALSE}
fewer<- as.vector(c(8:11, 37:49, 60:68,84:86,102,113:124,140,151:160))# subset to relevant variables
bicep_mod<-bicep[,fewer] #somewhat redundant
bicep_mod$classe<-as.factor(bicep_mod$classe)

```


#### Data Partition    

The data is partitioned into a training and test set.  Seventy-five percent of the observations are randomly assigned to the training set and twenty-five percent are assigned to the test set.  The training set will be used develop the model. The test set will be used to tune and refine the model.
```{r, "training and test set", warning=FALSE, message=FALSE}
library(caret)
set.seed(1666)

inTrain = createDataPartition(bicep_mod$classe, p =0.75, list=FALSE)
training = bicep_mod[ inTrain,]
testing = bicep_mod[-inTrain,]


```

### Modeling Algorithm:

The modeling is done with the "rf" method in the train function of caret.  Rpart was tried but with not enough accuracy.  TrainControl was set a method="repeatedcv" with ten folds and 5 repeats.  Having five repeats provided a better picture of error.  Ten folds allowed some bump in accuracy.
```{r "modFit", cache=TRUE, message=FALSE, warning=FALSE}
modFit<- train(classe ~ .,method="rf", preProcess=c("center", "scale"),trControl=trainControl(method = "repeatedcv", number = 10, repeats=5), data=training, importance=TRUE)
varImp(modFit)
#print(modFit)
modFit$finalModel
rsamp<-modFit$resample
reslt<-modFit$results

#Resources http://topepo.github.io/caret/training.html caret primer

```
Here is the accuracy and the in-sample error rate estimate.
```{r, "accuracy"}
mean(rsamp$Accuracy)
1-mean(rsamp$Accuracy)

```


### Results
 The results show that we can predict the technique of a subject doing a bicep curl with high accuracy.  The model, however is very much a "black box" in that it would be difficult for a person such as a personal trainer to use the readings from the sensors in real time to coach a subject on proper bicep curl technique.    
 
Also, it remains to be seen whether the model can be scaled up beyond these six subjects.
```{r, "prediction", echo=FALSE,message=FALSE,warning=FALSE}
pred<-predict(modFit, newdata=testing)
cm<-confusionMatrix(pred, testing$classe)
print(cm)
```
This is the prediction accuracy of the testing sets against the model results.  `r cm$overall[1]`.  The out-of-sample error is one minus this number or `r 1-cm$overall[1]`.  This error is surprising in that test error is expected to be noticeably less than training error.  Here they are virually equal.  

```{r, "prediction vs test group", echo=FALSE}
testgroup<-read.csv("testgroup.csv", stringsAsFactors=FALSE, header=TRUE)

testgroup<-testgroup[,fewer]

#sources http://nthturn.com/2015/02/22/prediction-using-random-forests-in-r-an-example/
predictions<-predict(modFit, newdata=testgroup)

predict(modFit, newdata=testgroup)

```
The results of the model against the test set are 20 out of twenty.



```{r, eval=FALSE, echo=FALSE}
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(predictions[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


```


```{r, echo=FALSE, results='hide'}
sessionInfo()

```

```{r, echo=FALSE, results='hide'}
endTime<-Sys.time()

endTime
endTime-startTime

```


```{r, echo=FALSE}
stopCluster(cl)
#https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
```

