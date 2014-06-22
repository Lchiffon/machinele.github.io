Machine learning assessment
========================================================
## Summary
- Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively.
- The data for this project come from this source: [http://groupware.les.inf.puc-rio.br/har].
- In this paper, I predict the test setdata from the train data using regression tree method and use 5-fold cross-validation to calculate the accurancy.


## clean the data
### After downing the data, I clean the data as follows:
1. load the data

```r
setwd("C:/Users/Administrator/Desktop/machine learning")
options(stringsAsFactors=F)
train<-read.csv("pml-training.csv")
test<-read.csv("pml-testing.csv")
```
2. Give two datasets as same names:

```r
names(train)->names(test)
```
3. Create a label to show whether it is a train data or test data

```r
rep(0,dim(train)[1])->lab
train1<-data.frame(train,labs=lab)
rep(NA,dim(test)[1])->lab
test1<-data.frame(test[,1:159],classe=lab,labs=test[,160])
```
4. Delete the variables which are not numberic or integer

```r
as.character(test1[,160])->test1[,160]
set<-rbind(test1,train1)
attach(set)
train_data<-data.frame(ID=X,
                  user_name=factor(user_name),num_window=num_window,
                  roll_belt=roll_belt,pitch_belt=pitch_belt,
                  yaw_belt=yaw_belt,
                  total_accel_belt=total_accel_belt,
                  gyros_belt_x=gyros_belt_x,gyros_belt_y=gyros_belt_y,
                  gyros_belt_z=gyros_belt_z,accel_belt_x=accel_belt_x,
                  accel_belt_y=accel_belt_y,accel_belt_z=accel_belt_z,
                  magnet_belt_x=magnet_belt_x,magnet_belt_y=magnet_belt_y,
                  magnet_belt_z=magnet_belt_z,roll_arm=roll_arm,
                  pitch_arm=pitch_arm,yaw_arm=yaw_arm,
                  total_accel_arm=total_accel_arm,
                  gyros_arm_x=gyros_arm_x,gyros_arm_y=gyros_arm_y,
                  gyros_arm_z=gyros_arm_z,accel_arm_x=accel_arm_x,
                  accel_arm_y=accel_arm_y,accel_arm_z=accel_arm_z,
                  magnet_arm_x=magnet_arm_x,magnet_arm_y=magnet_arm_y,
                  magnet_arm_z=magnet_arm_z,roll_dumbbell=roll_dumbbell,
                  pitch_dumbbell=pitch_dumbbell,
                  yaw_dumbbell=yaw_dumbbell,total_accel_dumbbell=total_accel_dumbbell,
                  gyros_dumbbell_x=gyros_dumbbell_x,gyros_dumbbell_y=gyros_dumbbell_y,
                  gyros_dumbbell_z=gyros_dumbbell_z,accel_dumbbell_x=accel_dumbbell_x,
                  accel_dumbbell_y=accel_dumbbell_y,accel_dumbbell_z=accel_dumbbell_z,
                  magnet_dumbbell_x=magnet_dumbbell_x,magnet_dumbbell_y=magnet_dumbbell_y,
                  magnet_dumbbell_z=magnet_dumbbell_z,roll_forearm=roll_forearm,
                  pitch_forearm=pitch_forearm,yaw_forearm=yaw_forearm,
                  total_accel_forearm=total_accel_forearm,
                  gyros_forearm_x=gyros_forearm_x,gyros_forearm_y=gyros_forearm_y,
                  gyros_forearm_z=gyros_forearm_z,accel_forearm_x=accel_forearm_x,
                  accel_forearm_y=accel_forearm_y,accel_forearm_z=accel_forearm_z,
                  magnet_forearm_x=magnet_forearm_x,magnet_forearm_y=magnet_forearm_y,
                  magnet_forearm_z=magnet_forearm_z,
                  classe=factor(classe),labs=labs )
detach(set)
```
### It's not a good method to clean the data,I know there are some good ways to solve it so, please tell me in the feedback.

4. Save the data and clean the Gobal Enviroment

```r
save(train_data, file='train_data.RData')
rm(list=ls())
gc()
```

```
##          used (Mb) gc trigger (Mb) max used (Mb)
## Ncells 259525  7.0     667722 17.9   456716 12.2
## Vcells 317761  2.5    9474426 72.3 11309292 86.3
```

## Build the model
1. Load the data

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
load("train_data.RData")
train<-train_data[train_data$labs==0,]
test<-train_data[!train_data$labs==0,]
```
2. split the data in 5 parts

```r
ids <- train$ID
set.seed(114)
ids <- split(ids, sample(length(ids)) %% 5)
sapply(ids,length)
```

```
##    0    1    2    3    4 
## 3924 3925 3925 3924 3924
```
3. 5-fold cv: using classification tree

```r
Acc<-0
for ( i in 1:5 ){
training<-train[-unlist(ids[i]),c(-1,-57)]
testing<-train[unlist(ids[i]),c(-1,-57)]
modelFit1<-train(classe~.,data=training,method="rpart")
cM<-confusionMatrix(testing$classe,predict(modelFit1,testing))
cM$overall[i]->Acc[i]
}
```

```
## Loading required package: rpart
```
### After calculating the 5-fold cv,overall accurancy is shown.

```r
mean(Acc)
```

```
## [1] 0.4943
```
### It's not really impressive...
## final model
1. fit the model

```r
rm(list=ls())
gc()
```

```
##           used (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 1439109 38.5    2403845  64.2  2403845  64.2
## Vcells 1367080 10.5   20624691 157.4 25780863 196.7
```

```r
load("train_data.RData")
train<-train_data[train_data$labs==0,]
test<-train_data[!train_data$labs==0,]
final_train<-train[,c(-1,-57)]
finalModel<-train(classe~.,data=final_train,method="rpart")
final_test<-test[,c(-1,-57)]
answer<-predict(finalModel,final_test)
confusionMatrix(final_train$classe,predict(finalModel,final_train))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 5080   81  405    0   14
##          B 1581 1286  930    0    0
##          C 1587  108 1727    0    0
##          D 1449  568 1199    0    0
##          E  524  486  966    0 1631
## 
## Overall Statistics
##                                         
##                Accuracy : 0.496         
##                  95% CI : (0.489, 0.503)
##     No Information Rate : 0.521         
##     P-Value [Acc > NIR] : 1             
##                                         
##                   Kappa : 0.341         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.497   0.5085    0.330       NA   0.9915
## Specificity             0.947   0.8531    0.882    0.836   0.8901
## Pos Pred Value          0.910   0.3387    0.505       NA   0.4522
## Neg Pred Value          0.634   0.9215    0.784       NA   0.9991
## Prevalence              0.521   0.1289    0.266    0.000   0.0838
## Detection Rate          0.259   0.0655    0.088    0.000   0.0831
## Detection Prevalence    0.284   0.1935    0.174    0.164   0.1838
## Balanced Accuracy       0.722   0.6808    0.606       NA   0.9408
```
2. predict the test dataset

```r
answer<-as.character(answer)
answer
```

```
##  [1] "C" "A" "C" "A" "A" "C" "C" "A" "A" "A" "C" "C" "C" "A" "C" "A" "A"
## [18] "A" "A" "C"
```
3. Write the txt flies:

```r
  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }
  pml_write_files(answer)
```
