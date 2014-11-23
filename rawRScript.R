#load library
library(caret)
library(randomForest)

setwd("C:/Users/Nung Lian Sume/Documents/Coursera/Practical Machine Learning/MLProject")

#Get the files.
training <- read.csv("pml-training.csv",header=TRUE,na.strings=c("NA",""))
testing <- read.csv("pml-testing.csv",header=TRUE,na.strings=c("NA",""))

#Check
dim(training)
dim(testing)

#Cleaning & Removing unnecessary columns.
names(training)
str(training)


trainClRM <- c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","num_window")
training <- training[,!names(training) %in% trainClRM]


###Preprocessing the **training** data.
#Check the near-zero variace variable and remove from the training set.

nzv <- nearZeroVar(training,saveMetrics=TRUE)
nzv[nzv$nzv, ][1:10, ]
training <- training[,!nzv$nzv]
dim(training)



#Check missing values
NARemove <- function(data,cutoff){
        NACount <- sapply(training,function (data) sum(is.na(data)))
        NAProportion <- NACount/nrow(training)
        RemovedNA <- NAProportion[NAProportion <= cutoff]
        return (data[,names(RemovedNA)])
}

training <- NARemove(training,0.90)
str(training)
#Check NA and Counting
NACount <- sapply(training,function (training) sum(is.na(training)))
NACount

#Check the target vriable
round(table(training$classe)/nrow(training)*100,2)
qplot(classe,data=training,,fill="classe",geom="bar",ylab="# Cases",xlab="Clases")

#Data Partition
inTrain <- createDataPartition(training$classe,p=0.60,list=FALSE)
training <- training[inTrain,]
validating <- training[-inTrain,]

#Modelling
set.seed(1234)
trControl <- trainControl(method="cv",
                          number=5)
rfFit <- train(classe ~.,data=training,
               method="rf",
               importance=TRUE,
               trControl=trControl)

#Error of Estimate
Since we are using the random forest, the error is estimated internally during the run.
Hence we will look at the OOB as an error of estimate.
rfFit$resample
rfFit$result

Since the result is more than 0.98, we estimate the OBB is less than 1%.
rfFit$finalModel

#Validation
pred <- predict(rfFit,newdata=validating)
confusionMatrix(pred,validating$classe)


#Applying the model to the Model test
#Selecting the same varialbe
testing <- testing[,names(testing) %in% names(training)]
colClass <- sapply(training,function(x) class(x))
for(i in 1:(ncol(training)-1)){
        class(testing[,names(colClass[i])]) <- colClass[[i]]
}

prediction <- predict(rfFit,newdata=testing)
prediction

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(prediction)

###Result
We use the random forest algorithm using 52 variables. Cross-valudation was applied with 5 folds. 
The out-of-sample error is approximately <0.9%. 

