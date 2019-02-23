library('caret')
library('readr')

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

#create data partition on All_CompResp
set.seed(743)
inTraining <- createDataPartition(All_CompResp$brand, p=.75, list=FALSE)
trainset <- All_CompResp[inTraining,]
testset <- All_CompResp[-inTraining,]

#specify resampling technique used in traning model
rffitControl <- trainControl(method='repeatedcv',number=10,repeats=1)
#specify tuning parameters to try
rfGrid <- expand.grid(mtry=c(1,3,5,7,9))
#train model
rfFitbrand <- train(brand~., 
                    data=trainset, 
                    method='rf', 
                    trControl=rffitControl,
                    tuneLength=1,
                    importance=TRUE)
rfFitbrand

#importance of variables: not useful
"varImp(rfFitbrand)"

#use model from trainset, predict brand data for testset
rfplot <- predict(rfFitbrand,testset)
#compare testset predictions to ground truth to find accuracy for testset
confusionMatrix(rfplot,testset$brand)
#check performance metrics of rfplot against ground truth
postResample(rfplot,testset$brand)

#use model from trainset, predict brand data for incomplete surveys
rfpredict <- predict(rfFitbrand,All_IncompResp)
rfpredict
summary(rfpredict)


#write data to files for report
Pred_IncompResp <- write_csv( (data.frame(All_IncompResp[,1:6],rfpredict)), 'PredictedResponses.csv')
All_Resp <- write_csv((data.frame(merge(All_CompResp, Pred_IncompResp, all=TRUE))), 'All_Responses.csv')
                             