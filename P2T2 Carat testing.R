pacman:: p_load(readr, caret, mlbench, e1071) #function to check if these packsges are installed, install them if not, and then call on them in this library


data(Sonar) #calling on the Sonar dataset 
set.seed(107) #setting the seed as to be able to use the same random numbers in the future
## cleaning of my environment

inTrain <- createDataPartition(
  y = Sonar$Class,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

##Asign training samples and check they are the correct size
training <- Sonar[ inTrain,] 
testing  <- Sonar[-inTrain,]
nrow(training)
nrow(testing)

##Setting certain parameters for the train function
ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

##training the model and tuning the parameters
plsFit <- train(
  Class ~ .,
  data = training,
  method = 'pls',
  ## Center and scale the predictors for the training set and all future samples.
  preProc = c('center', 'scale'),
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)

#Computing class of testing set using plsFit model
plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

#Predicting probablities of each class for testing model using plsFit model
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

#Computing confusion matrix for plsFit
confusionMatrix(data = plsClasses, testing$Class)

#Here we are going to train another model
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(107)
rdaFit <- train(
  Class ~ .,
  data = training,
  method = "rda",
  tuneGrid = rdaGrid,
  trControl = ctrl,
  metric = "ROC"
)

#Here we are using the RDA model to make predictions ont testing data
rdaClasses <- predict(rdaFit, newdata = testing)
head(rdaClasses)

#Computing confusion matrix using actual results of the testing data
confusionMatrix(rdaClasses, testing$Class)

#Comparing results and metrics of both model
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

#Plotting the results using Bland-Altman type plot
xyplot(resamps, what = "BlandAltman")

#Comparing both models again
diffs <- diff(resamps)
summary(diffs)

# create a simple Function
#Kraken testing