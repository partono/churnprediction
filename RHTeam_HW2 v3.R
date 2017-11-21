# pull data from postgres DB into R

library(RODBC)
#odbcConnect uses the RODBC package
channel = odbcConnect(dsn="PostgreSQL35W", uid="postgres", pwd="Kumeli-78")
odbcSetAutoCommit(channel,autoCommit = TRUE) #set autocommit ON
tr <- sqlQuery(channel,"select * from tr")
te <- sqlQuery(channel,"select * from te")
te_clean <- sqlQuery(channel,"select * from te_clean")
close(channel)


#Q2
head(te,n=3)
head(te_clean,n=3)
# excluding missing values 
lapply(te, mean, na.rm=TRUE)
sapply(te_clean, mean, na.rm=TRUE)
summary(te)
summary(te_clean)

#Q3 Imputation using Mice
# choose first 10,000 records in the te table
install.packages("mice")
install.packages("missForest")

te.mis <- te[1:10000,]
md.pattern(te.mis)

install.packages("VIM")
library(VIM)
mice_plot <- aggr(te.mis, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(te.mis), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

imputed_Data <- mice(te.mis, m=5, maxit = 50, method = 'cart', seed = 500)
summary(imputed_Data)


# Imputation using Amelia
library(Amelia)
amelia_fit <- amelia(te.mis, idvars = c("msno", "is_churn", "planother", "pricother"), +
                       ords= c("gender", "useforpredictions"), m=5, parallel = "multicore", +
                       noms = "daysrange")

?amelia

te$useforpredictions <- as.factor(te$useforpredictions)

remove(tr)
remove(trsf)

#Q6
tr1 <- within(tr, rm("msno", "registration_init_time", "methother", "planother", "pricother", "sub_churn"))

#Q7
tr2 <-subset(tr1, useforpredictions='keep')
tr2$useforpredictions <- NULL
head(tr2, n=3)

#Q8
tr3 <- na.omit(tr2)
head(tr3, n=3)

#Q9
# Make sure the following features are set up as factors: gender, is_churn, city, registered_via. 
class(tr3$gender)
class(tr3$is_churn)
class(tr3$city)
class(tr3$registered_via)

tr3$is_churn <- as.factor(tr3$is_churn)
tr3$city <- as.factor(tr3$city)
tr3$registered_via <- as.factor(tr3$registered_via)


#Q10
str(tr3)
head(tr3, n=3)
library(caret)

#dummy variables for categorical features
dmy <- dummyVars(" ~ city + gender + registered_via", data = tr3)
trsf <- data.frame(predict(dmy, newdata = tr3))
print(trsf)
tr4 <- data.frame(tr3, trsf)
head(tr4, n=3)

#remove categorical variables
tr5 <- within(tr4, rm("city", "gender", "registered_via"))
head(tr5, n=3)

#Q11
x = nearZeroVar(tr5, saveMetrics = TRUE)
x <- nearZeroVar(tr5)
tr6 <- tr5[, -x]
head(tr6, n=3)

#Q12
findCorrelation(cor(tr6), cutoff=0.9, verbose=FALSE, exact = ncol(tr6)<100)
tr7 <- within(tr6, rm("meth41", "plan0", "gender.male.............................................."))
colnames(tr7)[33] <- "gender.female"
str(tr7)
head(tr7, n=3)

#Q13
findLinearCombos((tr7))


#Q14
#city4, city5, city6, city13, city15, city22, genderfemale, registered_via3, registered_via4, 
# registered_via7, registered_via9, latest_renew, meth36, meth37, meth38, meth40, plan7, plan31, 
# and plan0. 
tr7$city.4 <- as.factor(tr7$city.4)
tr7$city.5 <- as.factor(tr7$city.5)
tr7$city.6 <- as.factor(tr7$city.6)
tr7$city.13 <- as.factor(tr7$city.13)
tr7$city.15 <- as.factor(tr7$city.15)
tr7$city.22 <- as.factor(tr7$city.22)
tr7$gender.female <- as.factor(tr7$gender.female)
tr7$registered_via.3 <- as.factor(tr7$registered_via.3)
tr7$registered_via.4 <- as.factor(tr7$registered_via.4)
tr7$registered_via.7 <- as.factor(tr7$registered_via.7)
tr7$registered_via.9 <- as.factor(tr7$registered_via.9)
tr7$latest_renew <- as.factor(tr7$latest_renew)
tr7$meth36 <- as.factor(tr7$meth36)
tr7$meth37 <- as.factor(tr7$meth37)
tr7$meth38 <- as.factor(tr7$meth38)
tr7$meth40 <- as.factor(tr7$meth40)
tr7$plan7 <- as.factor(tr7$plan7)
tr7$plan31 <- as.factor(tr7$plan31)
tr7$plan0 <- as.factor(tr7$plan0)
tr7$is_churn <- as.factor(tr7$is_churn)

str(tr7)
head(tr7, n=3)

#Q15
# preprocess values by centering and scaling (typical standardization)
preProcValues <- preProcess(tr7, method=c("center", "scale"))

# predict() actually transform the variables
trainTransformed <- predict(preProcValues, tr7)
str(tr7)
head(trainTransformed)



#Q16
# add back Is_churn variable to trainTransformed table
trainTransformed <- cbind(trainTransformed, tr3$is_churn)
names(trainTransformed)[38]<-paste("is_churn")
head(trainTransformed, n=3)

# make names and relevel is_churn
library(caret)
set.seed(1234)
make.names(trainTransformed$is_churn, unique = FALSE, allow_ = TRUE)
relevel(trainTransformed$is_churn, "0")
levels(trainTransformed$is_churn) <- c("X0", "X1")


#Q17
# Use the createDataPartition() function to create an 80/20 train and test datasets.
set.seed(1234) # set seed to replicate results
inTrain <- createDataPartition(
  y = trainTransformed$is_churn, ## the outcome data are needed
  p = .8, ## The percentage of data in the training set
  list = FALSE)

## The output is a set of integers for the rows of trainTransformed that belong in the 
# training set.
str(inTrain)

# Partition the data into training and test sets
training <- trainTransformed[ inTrain,]
testing <- trainTransformed[-inTrain,]

# number of training records
nrow(training)
head(training, n=3)

# number of testing records
nrow(testing)


#Q18
# Plot the distribution of your target variable in the train and test set.
library(ggplot2)
ggplot(data=training) + geom_histogram(aes(x = training$is_churn))
ggplot(data=training) + geom_density(aes(x = mpg), fill="blue")

par(mfrow=c(1,2), bg="lightgrey")
barplot(table(training$is_churn), main="Train Set")
barplot(table(testing$is_churn), main="Test Set")



#Q19
# Use the trainControl() function to specify 5-fold cross-validation, 
# as well as anything else that is important to indicate you are doing classification-type modeling.
#tree <- train(Class ~ .,
             # data = training,
            #  method = "rpart",
            #  preProc = c("center", "scale"))

ctrl <- trainControl(method = "CV",
                     number = 5,
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
ctrl


#Q20 Use the train() function to build any model you want

#remove id in training
training <- within(training, rm("id"))

str(training)

logFit <- train(is_churn ~ .,
                data = training,
                trControl = ctrl,
                method = "glm",
                metric = "ROC")
logFit

levels(training$is_churn) <- c("X0", "X1")
levels(testing$is_churn) <- c("X0", "X1")

nrow(training$is_churn)

str(training)


#Q21
# Use the predict() function to generate predicted probabilities and predicted classes 
# for both the train and test sets

# predicted classes
logClasses <- predict(logFit, newdata=training)
str(logClasses)
head(training)

# predicted probabilities
logProbs <- predict(logFit, newdata=training, type="prob")
head(logProbs)

# predicted classes
logClasses1 <- predict(logFit, newdata=testing)
str(logClasses1)
head(testing)

# predicted probabilities
logProbs1 <- predict(logFit, newdata=testing, type="prob")
head(logProbs1)

#Q22 
# Use the confusionMatrix() function to evaluate your model. 
# Compare the train and test statistics and describe if you have overfit your model.

# confusion matrix for training set
confusionMatrix(data=logClasses, training$is_churn)

# confusion matrix for testing set
confusionMatrix(data=logClasses1, testing$is_churn)



#Q23
# Use the roc() function to create an ROC curve on the test set predictions, 
# as well as the auc() function to capture the area-under-the-curve statistc.
install.packages("pROC")
library(pROC)
require(pROC)

# calculate AUC
head(testing)
roc(response=testing$is_churn, predictor=logProbs1$X1)
roc(response=testing$is_churn, predictor=logProbs1$X0)

# incorporate logProb1 column into results table
results = data.frame(testing, logClasses1, logProbs1)
head(results, n=3)
head(testing)

auc(testing$is_churn, logProbs1$X0)

# plot ROC
par(mfrow=c(1,1), bg="lightgrey")
plot.roc(testing$is_churn, logProbs1$X1, main="ROC Curves", percent=T, identity=T
         ,auc.polygon=TRUE, col="#1c61b6", print.auc=F)
plot.roc(testing$is_churn, logProbs1$X1, col="red", percent=T, add=T) 





#Q24
# Use the mgcv R package and saveRDS() function to save your predictive model for use later
install.packages("mgcv")
saveRDS(logFit, "HW2.rds")


#Q25
# Pull the te_clean dataset from your database into R. 
# Coerce the factor variables in this dataset exactly like you did in Q14. 
library(RODBC)
#odbcConnect uses the RODBC package
channel = odbcConnect(dsn="PostgreSQL35W", uid="postgres", pwd="Kumeli-78")
odbcSetAutoCommit(channel,autoCommit = TRUE) #set autocommit ON
te_clean <- sqlQuery(channel, "select * from te_clean")
close(channel)

remove(inTrain, mtcars, tr, tr1, tr2, tr3, tr4, tr5, tr6)

str(te_clean)
te_clean$city.4 <- as.factor(te_clean$city.4)
te_clean$city.5 <- as.factor(te_clean$city.5)
te_clean$city.6 <- as.factor(te_clean$city.6)
te_clean$city.13 <- as.factor(te_clean$city.13)
te_clean$city.15 <- as.factor(te_clean$city.15)
te_clean$city.22 <- as.factor(te_clean$city.22)
te_clean$gender.female <- as.factor(te_clean$gender.female)
te_clean$registered_via.3 <- as.factor(te_clean$registered_via.3)
te_clean$registered_via.4 <- as.factor(te_clean$registered_via.4)
te_clean$registered_via.7 <- as.factor(te_clean$registered_via.7)
te_clean$registered_via.9 <- as.factor(te_clean$registered_via.9)
te_clean$latest_renew <- as.factor(te_clean$latest_renew)
te_clean$meth36 <- as.factor(te_clean$meth36)
te_clean$meth37 <- as.factor(te_clean$meth37)
te_clean$meth38 <- as.factor(te_clean$meth38)
te_clean$meth40 <- as.factor(te_clean$meth40)
te_clean$plan7 <- as.factor(te_clean$plan7)
te_clean$plan31 <- as.factor(te_clean$plan31)
te_clean$plan0 <- as.factor(te_clean$plan0)
te_clean$num100 <- as.numeric(te_clean$num100)
te_clean$is_churn <- as.factor(te_clean$is_churn)

str(te_clean)
head(te_clean)


#Q25
# Use the preprocess() function to center and scale your te_clean dataset 
# just like you did to your tr dataset in Q15. 
# Then use the predict() function to actually transform your variables.

remove(results, testing, tr7, trsf)

# preprocess values by centering and scaling (typical standardization)
library(caret)
te_clean_preProcValues <- preProcess(te_clean, method=c("center", "scale"))

# predict() actually transform the variables
te_clean_Transformed <- predict(te_clean_preProcValues, te_clean)
#te_clean_Transformed1 <- within(te_clean_Transformed, rm("msno"))
str(te_clean_Transformed)
head(te_clean_Transformed)

#Q26
# Use the predict() function to score your predictions on the te_clean dataset. 
# This might take a few minutes given the type of model you used in Q20. 
# For example, k-nearest neighbors (method="knn") will take a long time 
# because of the distance calculations that it will have to make.

# predicted classes

te_clean_logClasses <- predict(logFit, newdata=te_clean_Transformed)

str(te_clean_logClasses)
head(te_clean_logClasses)

# predicted probabilities
te_clean_logProbs <- predict(logFit, newdata=te_clean_Transformed, type="prob")
head(te_clean_logProbs)


#Q27
# Use the write.table() function to write out the msno and predictions from Q27


# samplesubmission <- read.csv(file="sample_submission_v2.csv", header=T, sep = ",")
setwd("C:/Users/Prasidhi Artono/Desktop/Data Mining/Data Mining HW1/Kaggle Data")

head(te_clean_logProbs$X1)
head(te_clean$msno)
te_clean$msno <- as.data.frame(te_clean$msno)
class(te_clean$msno)
class(te_clean_logProbs)


kagglesubmissions <- cbind(te_clean$msno, te_clean_logProbs$X1)
head(kagglesubmissions)
colnames(kagglesubmissions)[1] <- "msno"
colnames(kagglesubmissions)[2] <- "is_churn"

kagglesubmission_hw2 <- write.table(kagglesubmissions, 
                                 file = "sample_submission_v2.csv", 
                                 row.names=FALSE, col.names=TRUE, sep = ",")


