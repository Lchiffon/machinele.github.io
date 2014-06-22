## machine learning
setwd("C:/Users/Administrator/Desktop/machine learning")
options(stringsAsFactors=F)
train<-read.csv("pml-training.csv")
test<-read.csv("pml-testing.csv")
names(train)->names(test)
rep(0,dim(train)[1])->lab
train1<-data.frame(train,labs=lab)
rep(NA,dim(test)[1])->lab
test1<-data.frame(test[,1:159],classe=lab,labs=test[,160])
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
save(train_data, file='train_data.RData')
rm(list=ls())
gc()
load("train_data.RData")
library(caret)
str(train_data)
train<-train_data[train_data$labs==0,]
test<-train_data[!train_data$labs==0,]
ids <- train$ID
set.seed(114)
ids <- split(ids, sample(length(ids)) %% 5)
sapply(ids,length)
### 5-fold cv:everyclassification
Acc<-0
for ( i in 1:5 ){
training<-train[-unlist(ids[i]),c(-1,-57)]
testing<-train[unlist(ids[i]),c(-1,-57)]
modelFit1<-train(classe~.,data=training,method="rpart")
cM<-confusionMatrix(testing$classe,predict(modelFit1,testing))
cM$overall[i]->Acc[i]
}
### overall accurancy
mean(Acc)

## final model
rm(list=ls())
gc()
load("train_data.RData")
train<-train_data[train_data$labs==0,]
test<-train_data[!train_data$labs==0,]
final_train<-train[,c(-1,-57)]
finalModel<-train(classe~.,data=final_train,method="rpart")
final_test<-test[,c(-1,-57)]
answer<-predict(finalModel,final_test)
confusionMatrix(final_train$classe,predict(finalModel,final_train))
answer<-as.character(answer)



  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }
  pml_write_files(answer)



### further final answer is a,c.delete them and remodel
train2<-final_train[!(final_train$classe=="A" | 
                        final_train$classe=="C"), ]

Model2<-train(classe~.,data=train2,method="rpart")
confusionMatrix(train2$classe,predict(Model2,train2))
answer2<-predict(Model2,final_test)
answer2
setwd("C:/Users/Administrator/Desktop/machine learning/sub2")
pml_write_files(answer2)

# tr1<-predict(modelFit1,training)
# pr1<-predict(modelFit1,testing)
# 
# modelFit2<-train(classe~.,data=training,method="elm")
# confusionMatrix(testing$classe,predict(modelFit2,testing))
# tr2<-predict(modelFit2,training)
# pr2<-predict(modelFit2,testing)
# 
# spackingTr<-data.frame(classe=training$classe,a1=tr1,a2=tr2)
# spackingTe<-data.frame(classe=testing$classe,a1=pr1,a2=pr2)
# modelFit3<-train(classe~.,data=spackingTr,method="rpart")
# confusionMatrix(spackingTe$classe,predict(modelFit3,spackingTe))

# ids <- id[!is.na(y)]
# set.seed(114)
# ids <- split(ids, sample(length(ids)) %% 5)
# sapply(ids,length)
# 
# 
# train2<-training[!(training$classe=="A" | training$classe=="B"),]
# test2<-testing[!(predict(modelFit,testing)=="A" | predict(modelFit,testing)=="B"),]
# moedelFit2<-train(classe~.,data=train2,method="rpart")
# confusionMatrix(test2$classe,predict(moedelFit2,test2))
# 

