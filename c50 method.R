library('caret')
library('readr')
library('C50')

#read in data
All_CompResp <- read_csv('CompleteResponses.csv')
All_IncompResp <- read_csv('SurveyIncomplete.csv')

#convert y-var to factor to do classification
All_CompResp$brand <- as.factor(All_CompResp$brand)

#rfe: useful
"control <- rfeControl(functions=rfFuncs, method='cv',number=10)
rferesults <-rfe(All_CompResp[,1:6],All_CompResp$brand, sizes=c(1:7), rfeControl=control)
rferesults"

#correlation matrix: not useful
"correlationMatrix <- cor(All_CompResp[,1:6])
correlationMatrix
highlyCorrelated <- findCorrelation(correlationMatrix,cutoff=0.01)
highlyCorrelated"

#variable reduction by importance and rfe analysis
All_CompResp <- All_CompResp[,c(1,2,3,7)]

#create data partition on sample
set.seed(743)
inTraining <- createDataPartition(All_CompResp$brand, p=.75, list=FALSE)
trainset <- All_CompResp[inTraining,]
testset <- All_CompResp[-inTraining,]

#specify resampling technique used in traning model
c50fitControl <- trainControl(method='repeatedcv',number=10,repeats=1)
#train model
c50Fitbrand <- train(brand~.,
                     data=trainset,
                     method='C5.0',
                     trControl=c50fitControl,
                     preProc = c('center','scale'),
                     tuneLength = 2,
                     importance=TRUE)
c50Fitbrand

#importance of variables: useful
"varImp(c50Fitbrand)"

#use model from trainset, predict brand data for testset
c50plot <- predict(c50Fitbrand,testset)
#compare testset predictions to ground truth to find accuracy for testset
confusionMatrix(c50plot,testset$brand)
