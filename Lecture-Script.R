# Coursera Data Science Specialization Course: Reproducible Resarch
# Lecture Scripts
############################################################################################
# Set up environment and import data
library(kernlab)
library(caret)
data(spam)
set.seed(3435)
str(spam)

# Split data into training and test sets
inTrain <- createDataPartition(y = spam$type, p = 0.5, list = FALSE)
trainSpam <- spam[inTrain,]
testSpam  <- spam[-inTrain,]

# Exploratory data analysis: training data
names(trainSpam)
head(trainSpam)
table(trainSpam$type)
boxplot(capitalAve ~ type, data = trainSpam) # highly skewed
boxplot(log10(capitalAve + 1) ~ type, data = trainSpam) # plus one handles NA's
pairs(log10(trainSpam[,1:4] + 1))
hcluster <- hclust(dist(t(trainSpam[,1:57]))); plot(hcluster)
hclusterUpdated <- hclust(dist(t(log10(trainSpam[,1:55] + 1)))) ; plot(hclusterUpdated)

# Statistical prediction / modeling
trainSpam$numType <- as.numeric(trainSpam$type) - 1
testSpam$numType <- as.numeric(testSpam$type) - 1
costFunction <- function(x, y) sum(x != (y > 0.5))
cvError <- rep(NA, 55)
library(boot)
for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family = binomial, data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

## Which predictor has minimum cross-validated error
names(trainSpam[which.min(cvError)])

## Create a simple, one-predictor model
predictionModel <- train(type ~ charDollar, method = "glm", data = trainSpam)
predictionTest  <- predict(predictionModel, testSpam)
confusionMatrix(predictionTest, testSpam$type)

## Create a random forest model
library(ranger)
modelRanger <- train(type~., data = trainSpam, method = "ranger",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
modelRanger
predictTestRanger <- predict(modelRanger, testSpam)
confusionMatrix(predictTestRanger, testSpam$type)

########################################################################################
# Week 3
## Good practices:
# Make everything machine reproducible, documented
sessionInfo()  # Call to document environment
set.seed(3435) # Set with integer

