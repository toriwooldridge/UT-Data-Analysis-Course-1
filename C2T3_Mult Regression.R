library (readr)
library (caret)
library (corrplot)

#read in data, remove variable with missing values
raw_data <- read.csv('existingproductattributes2017.csv')
raw_data$BestSellersRank <- NULL

#create model to apply dummy data
dummy_model <- dummyVars('~.', data = raw_data)
#apply dummy data model onto our raw data, put into data frame
dummify_data <- data.frame (predict (dummy_model, newdata = raw_data))

#find correlation between variables, remove ones with correlation > .9
corrplot (cor (dummify_data))
highlyCorrelated <- findCorrelation (cor (dummify_data), cutoff = .9)
noncorr_data <- data.frame (dummify_data[-(c (16,18))])

#find correlation between Volume & Service/Product Reviews
review_data <- data.frame(dummify_data[15:21],dummify_data[28])
review_corplot <- corrplot(cor (review_data))
review_corplot

#recursive feature engr
control <- rfeControl (functions = rfFuncs, method = 'cv', number = 10)
rferesults <- rfe (noncorr_data[1:25], noncorr_data$Volume, sizes = c(5:25),rfeControl=control)
rferesults

#delete unhelpful variables
noncorr_data$ProfitMargin <- NULL
noncorr_data$ProductNum <- NULL
noncorr_data$ProductDepth <- NULL
noncorr_data$ProductWidth <- NULL
noncorr_data$ProductHeight <- NULL
noncorr_data$ProductType.Accessories <- NULL
noncorr_data$ProductType.Display <- NULL
noncorr_data$ProductType.ExtendedWarranty <- NULL
noncorr_data$ProductType.GameConsole <- NULL
noncorr_data$ProductType.Printer <- NULL
noncorr_data$ProductType.PrinterSupplies <- NULL
noncorr_data$ProductType.Software <- NULL
noncorr_data$ProductType.Tablet <- NULL
noncorr_data$ShippingWeight <- NULL

final_dataset <- noncorr_data

#set training and testing data
set.seed (135)
inTraining <- createDataPartition (final_dataset$Volume, p =.75, list = FALSE)
trainset <- final_dataset [inTraining, ]
testset <- final_dataset [-inTraining, ]

#train linear model
lm_model1 <- lm (Volume~., trainset)
lm_pred1 <- predict (lm_model1, testset)
lm_pred1
postResample (lm_pred1, testset$Volume)
summary(lm_model1)

#train nonparametric models using repeated 10-fold cross validation
fitControl <- trainControl (method = 'repeatedcv', number = 10, repeats = 1)

#train SVM model
set.seed (403)
svm_model2 <- train (Volume~.,
                    data = trainset,
                    method = 'svmLinear2',
                    trControl = fitControl,
                    scale = FALSE)
svm_model2
svm_pred2 <- predict (svm_model2, testset)
postResample (svm_pred2, testset$Volume)
summary(svm_model2)

#train RF model - getting overfit at higher mtry's but good vol
set.seed (403)
rf_tunegrid <- expand.grid (.mtry = c(1:11))
rf_model3 <- train (Volume~.,
                    data = trainset,
                    method = 'rf',
                    tuneGrid = rf_tunegrid,
                    trControl = fitControl,
                    importance = TRUE)
rf_model3
rf_pred3 <- predict (rf_model3, testset)
rf_pred3
postResample (rf_pred3, testset$Volume)

#train GBT model - no overfit but neg volumes
set.seed (403)
"gbt_tunegrid <- expand.grid(n.trees = (0:25)*100,
                            interaction.depth = c(1, 3, 6),
                            shrinkage = seq(.0005, .05,.001),
                            n.minobsinnode=10)"
gbt_tunegrid <- expand.grid(n.trees = 100,
                            interaction.depth = 1,
                            shrinkage = .0175,
                            n.minobsinnode=10)
gbt_model4 <- train (Volume~.,
                   data = trainset,
                   tuneGrid = gbt_tunegrid,
                   method = 'gbm',
                   trControl = fitControl)
gbt_pred4 <- predict (gbt_model4, testset)
gbt_pred4
postResample (gbt_pred4, testset$Volume)

#apply best model (GBT) to new product data
raw_new_data <- read_csv ('newproductattributes2017.csv')

dummify_new_model <- dummyVars ('~.', data = raw_new_data)
dummify_new_data <- data.frame (predict (dummify_new_model, newdata = raw_new_data))
dummify_new_data$BestSellersRank <- NULL
dummify_new_data$ProfitMargin <- NULL
dummify_new_data$ProductNum <- NULL
dummify_new_data$ProductDepth <- NULL
dummify_new_data$ProductWidth <- NULL
dummify_new_data$ProductHeight <- NULL
dummify_new_data$ProductTypeAccessories <- NULL
dummify_new_data$ProductTypeDisplay <- NULL
dummify_new_data$ProductTypeExtendedWarranty <- NULL
dummify_new_data$ProductTypeGameConsole <- NULL
dummify_new_data$ProductTypePrinter <- NULL
dummify_new_data$ProductTypePrinterSupplies <- NULL
dummify_new_data$ProductTypeSoftware <- NULL
dummify_new_data$ProductTypeTablet <- NULL
dummify_new_data$ShippingWeight <- NULL

names(dummify_new_data)[1] <- "ProductType.Laptop"
names(dummify_new_data)[2] <- "ProductType.Netbook"
names(dummify_new_data)[3] <- "ProductType.PC"
names(dummify_new_data)[4] <- "ProductType.Smartphone"

finalPred <- predict (gbt_model4, dummify_new_data)
raw_new_data$predictions <- finalPred
write.csv(raw_new_data, file = "finalPred.csv", row.names = TRUE)
