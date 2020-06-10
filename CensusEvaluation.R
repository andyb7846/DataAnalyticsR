

#Code has been sectioned and indexed for ease of navigation.

#==================PACKAGE INSTALLATION + DATA LOADING================================================

library(mlbench)
library(caret)
library(e1071)
library(devtools)
library(ggbiplot)
library(githubinstall)
library(klaR)
library(MASS)
library(ggplot2)
library(skimr)
library(mlbench)
library(caret)
library(psych)
library(class)
library(gmodels)
library(lattice)
library(ElemStatLearn)
library(rpart)
library(tidyverse)
library(plyr)
library(dplyr)
library(kernlab)
library(randomForest)
library(ROCR)

install_github("vqv/ggbiplot") #HIT 0 WHEN PROMPTED

adult <-
  read.table(
    'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data',
    sep = ',',
    fill = F,
    strip.white = T
  )
raw_data <- adult
#Creating a sample of 1000 to ensure fast loading times.
set.seed(567)
adultsample <- sample(1:nrow(adult), 1000)
adult <- adult[adultsample,]

#==========DATA CLEANSING, PREP + EXPLORATORY ANALYSIS==================================


colnames(adult) <- c(
  'age',
  'workclass',
  'fnlwgt',
  'education',
  'eduYears',
  'marital_status',
  'occupation',
  'relationship',
  'race',
  'sex',
  'capgain',
  'caploss',
  'weekhours',
  'origin',
  'income'
)
m <- adult

adult$education <- NULL
adult$fnlwgt <- NULL
adult$relationship <- NULL

#Removing NA Variables
adult <-
  adult[!(adult$workclass == "?" |
            adult$occupation == "?" |
            adult$origin == "?"), ] #Removing Empty Values

#Converting Adult Strings to Numbers for Feature Engineering
b <- m
b$workclass <- as.numeric(m$workclass)
b$education <- as.numeric(m$education)
b$marital_status <- as.numeric(m$marital_status)
b$occupation <- as.numeric(m$occupation)
b$relationship <- as.numeric(m$relationship)
b$race <- as.numeric(m$race)
b$sex <- as.numeric(m$sex)
b$origin <- as.numeric(m$origin)
b$income <- as.numeric(m$income)

#============================EXPLORATORY VISUALISATION====================================


#AGE/INCOME Histogram
ggplot(adult) + aes(x = as.numeric(age),
                    group = income,
                    fill = income) +
  geom_histogram(binwidth = 1, color = 'black')

#AGE/GENDER Histogram
ggplot(adult) + aes(x = as.numeric(age),
                    group = sex,
                    fill = sex) +
  geom_histogram(binwidth = 1, color = 'black')


#=================================EXPLORATORY ANALYSIS========================================
#Tying up existing variables.

summary(adult$workclass)

levels(adult$workclass)[1] <- 'Unknown'
#craeting government jobs
adult$workclass <-
  gsub('^Federal-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^Local-gov', 'Government', adult$workclass)
adult$workclass <-
  gsub('^State-gov', 'Government', adult$workclass)

#creating self employed jobs
adult$workclass <-
  gsub('^Self-emp-inc', 'Self-Employed', adult$workclass)
adult$workclass <-
  gsub('^Self-emp-not-inc', 'Self-Employed', adult$workclass)

#dealing with other variables
adult$workclass <- gsub('^Never-worked', 'Other', adult$workclass)
adult$workclass <- gsub('^Without-pay', 'Other', adult$workclass)
adult$workclass <- gsub('^Other', 'Other/Unknown', adult$workclass)
adult$workclass <-
  gsub('^Unknown', 'Other/Unknown', adult$workclass)

adult$workclass <- as.factor(adult$workclass)

summary(adult$workclass)

#CALCULATING INCOME BY EDUCATIONAL YEARS

dataframe1 <- data.frame(table(adult$income, adult$eduYears))
names(dataframe1) <- c('income', 'eduYears', 'group')
dataframe1

#Calculate percentages
dataframe1 <-
  ddply(dataframe1, .(eduYears), transform, percent = group / sum(group) * 100)

#Format labels and calculate positions
dataframe1 <-
  ddply(dataframe1, .(eduYears), transform, pos = (cumsum(group) - 0.5 * group))
dataframe1$label <- paste0(sprintf("%.0f", dataframe1$percent), "%")

#Remove group percentages to stop text overlap
dataframe1$label[which(dataframe1$percent < 5)] <- NA

#Bar plot eduYear vs Income
ggplot(dataframe1, aes(x = eduYears, y = group, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) +
  ggtitle('Income Level with Years of Education')

#CALCULATING INCOME BY OCCUPATION

summary(adult$occupation)

levels(adult$occupation)[1] <- 'Unknown'
adult$occupation <-
  gsub('Adm-clerical', 'White-Collar', adult$occupation)
adult$occupation <-
  gsub('Craft-repair', 'Blue-Collar', adult$occupation)
adult$occupation <-
  gsub('Exec-managerial', 'White-Collar', adult$occupation)
adult$occupation <-
  gsub('Farming-fishing', 'Blue-Collar', adult$occupation)
adult$occupation <-
  gsub('Handlers-cleaners', 'Blue-Collar', adult$occupation)
adult$occupation <-
  gsub('Machine-op-inspct', 'Blue-Collar', adult$occupation)
adult$occupation <-
  gsub('Other-service', 'Service', adult$occupation)
adult$occupation <-
  gsub('Priv-house-serv', 'Service', adult$occupation)
adult$occupation <-
  gsub('Prof-specialty', 'Professional', adult$occupation)
adult$occupation <-
  gsub('Protective-serv', 'Service', adult$occupation)
adult$occupation <-
  gsub('Tech-support', 'Service', adult$occupation)
adult$occupation <-
  gsub('Transport-moving', 'Blue-Collar', adult$occupation)
adult$occupation <-
  gsub('Unknown', 'Other/Unknown', adult$occupation)
adult$occupation <-
  gsub('Armed-Forces', 'Other/Unknown', adult$occupation)
adult$occupation <- as.factor(adult$occupation)
summary(adult$occupation)



#Create Dataframes
dataframe2 <- data.frame(table(adult$income, adult$occupation))
names(dataframe2) <- c('income', 'occupation', 'group')
dataframe2

#Calculate percentages
dataframe2 <-
  ddply(dataframe2, .(occupation), transform, percent = group / sum(group) * 100)

#Format Labels
dataframe2 <-
  ddply(dataframe2, .(occupation), transform, pos = (cumsum(group) - 0.5 * group))
dataframe2$label <- paste0(sprintf("%.0f", dataframe2$percent), "%")

#Visualisation
ggplot(dataframe2, aes(x = occupation, y = group, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) +
  ggtitle('Income Level with Different Occupations')

#Capital Gain Histogram
ggplot(adult) + aes(x = as.numeric(capgain),
                    group = income,
                    fill = income) +
  geom_histogram(bins = 10, color = 'black') + ggtitle('Histogram of Capital Gain')

#Capital Loss Histogram
ggplot(adult) + aes(x = as.numeric(caploss),
                    group = income,
                    fill = income) +
  geom_histogram(bins = 10, color = 'black') + ggtitle('Histogram of Capital Loss')

#% of Observations without any Capital Gain or Loss
sum(adult$capgain == 0) / length(adult$capgain)
sum(adult$caploss == 0) / length(adult$caploss)

adult$capgain <- NULL
adult$caploss <- NULL
adult$origin <- NULL

#CALCULATING INCOME BY RACE

dataframe3 <- data.frame(table(adult$income, adult$race))
names(dataframe3) <- c('income', 'race', 'group')
dataframe3

#Calculate percentages
dataframe3 <-
  ddply(dataframe3, .(race), transform, percent = group / sum(group) * 100)

#Format Labels
dataframe3 <-
  ddply(dataframe3, .(race), transform, pos = (cumsum(group) - 0.5 * group))
dataframe3$label <- paste0(sprintf("%.0f", dataframe3$percent), "%")

#Get rid of low categories
dataframe3$label[dataframe3$race == 'Other'] <- NA
dataframe3$label[dataframe3$race == 'Amer-Indian-Eskimo'] <- NA

#Visualisation
ggplot(dataframe3, aes(x = race, y = group, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) +
  ggtitle('Income Level by Race')

##=================================LOGISTIC REGRESSION==========================================

setsize <- round(.8 * dim(adult)[1])
training_set <- adult[1:setsize, ]
testing_set <- adult[-(1:setsize), ]

#Queriying the logistic regression model.
m1 <-
  glm(income ~ ., data = training_set, family = binomial('logit'))
summary(m1)

confint(m1)

m_full <- m1  # full model is the model just fitted
m_null <-
  glm(income ~ 1, data = training_set, family = binomial('logit'))

summary(m_null)

# backward selection
step(
  m_full,
  trace = F,
  scope = list(lower = formula(m_null), upper = formula(m_full)),
  direction = 'backward'
)

# forward selection
step(
  m_null,
  trace = F,
  scope = list(lower = formula(m_null), upper = formula(m_full)),
  direction = 'forward'
)

# create a data frame to store information regarding deviance residuals
index <- 1:dim(training_set)[1]
residualdeviation <- residuals(m1)
income <- training_set$income
residualdataframe <- data.frame(index, residualdeviation, income)


ggplot(residualdataframe,
       aes(x = index, y = residualdeviation, color = income)) +
  geom_point() +
  geom_hline(yintercept = 3,
             linetype = 'dashed',
             color = 'blue') +
  geom_hline(yintercept = -3,
             linetype = 'dashed',
             color = 'blue')
ggtitle('Plot of Deviance Residuals')

prob <- predict(m1, testing_set, type = 'response')
pred <- rep('<=50K', length(prob))
pred[prob >= .5] <- '>50K'

#CONFUSION MATRIX
tb <- table(pred, testing_set$income)
# outputs raw figures
tb
confusionMatrix(tb)

#========================================MODEL FITTING====================================================

library(nnet)
neuralnetworks1 <-
  nnet(income ~ .,
       data = training_set,
       size = 40,
       maxit = 500)

neuralnetworks1.pred <-
  predict(neuralnetworks1, newdata = testing_set, type = 'raw')

pred1 <- rep('<=50K', length(neuralnetworks1.pred))
pred1[neuralnetworks1.pred >= .5] <- '>50K'
#confusion matrix
predictiontable1 <- table(pred1, testing_set$income)
predictiontable1
#more data
confusionMatrix(predictiontable1)

#=================================================CART==============================================================

library(rpart)
CART2 <-
  rpart(income ~ .,
        data = training_set,
        method = 'class',
        cp = 1e-3)
CART2.pred.prob <-
  predict(CART2, newdata = testing_set, type = 'prob')
CART2.pred <- predict(CART2, newdata = testing_set, type = 'class')
#CONFUSION MATRIX
predictiontable2 <- table(CART2.pred, testing_set$income)
confusionMatrix(predictiontable2)

#========================================RANDOM FOREST=====================================================

library(randomForest)
randomforest3 <-
  randomForest(income ~ ., data = training_set, ntree = 1000)
randomforest3.pred.prob <-
  predict(randomforest3, newdata = testing_set, type = 'prob')
randomforest3.pred <-
  predict(randomforest3, newdata = testing_set, type = 'class')
#CONFUSION MATRIX
predictiontable3 <- table(randomforest3.pred, testing_set$income)
confusionMatrix(predictiontable3)

#===============================SUPPORT VECTOR MACHINE==========================================

library(kernlab)
supportvectormachine4 <- ksvm(income ~ ., data = training_set)
supportvectormachine4.pred.prob <-
  predict(supportvectormachine4, newdata = testing_set, type = 'decision')
supportvectormachine4.pred <-
  predict(supportvectormachine4, newdata = testing_set, type = 'response')
#CONFUSION MATRIX
predictiontable4 <- table(supportvectormachine4.pred, testing_set$income)
confusionMatrix(predictiontable4)

#==================================================KNN=============================================================
#Bugged. Currently Not working, additional KNN method below.
library(class)
library(gmodels)
knn5 <-
  knn(income ~ . ,
      training_set,
      testint_set,
      norm = TRUE,
      k = 3) #Doesn't work, but I feel as if I was very close.
knn5.pred.prob <-
  predict(knn5, newdata = testing_set, type = 'prob')
knn5.pred <- predict(knn5, newdata = testing_set, type = 'response')
#CONFUSION MATRIX
predictiontable5 <- table(knn5.pred, testing_set$income)
confusionMatrix(predictiontable5)

#============================================ROC CURVE=======================================

#create a prediction object
pr <- prediction(prob, testing_set$income)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

#create a data frame for TP and FP rates
dd <- data.frame(FP = prf@x.values[[1]], TP = prf@y.values[[1]])

#=================================PLOT NEURAL NETWORKS=============================
prediction1 <- prediction(neuralnetworks1.pred, testing_set$income)
performance1 <-
  performance(prediction1, measure = "tpr", x.measure = "fpr")
perfdata1 <-
  data.frame(FP = performance1@x.values[[1]], TP = performance1@y.values[[1]])

#============================================PLOT CART=============================
prediction2 <- prediction(CART2.pred.prob[, 2], testing_set$income)
performance2 <-
  performance(prediction2, measure = "tpr", x.measure = "fpr")
perfdata2 <-
  data.frame(FP = performance2@x.values[[1]], TP = performance2@y.values[[1]])

#===================================PLOT RANDOM FOREST=============================
prediction3 <-
  prediction(randomforest3.pred.prob[, 2], testing_set$income)
performance3 <-
  performance(prediction3, measure = "tpr", x.measure = "fpr")
perfdata3 <-
  data.frame(FP = performance3@x.values[[1]], TP = performance3@y.values[[1]])

#==========================PLOT SUPPORT VECTOR MACHINE=========================
prediction4 <-
  prediction(supportvectormachine4.pred.prob, testing_set$income)
performance4 <-
  performance(prediction4, measure = "tpr", x.measure = "fpr")
perfdata4 <-
  data.frame(FP = performance4@x.values[[1]], TP = performance4@y.values[[1]])

#=============================================PLOT KNN======================================
prediction5 <- prediction(knn5.pred.prob, testing_set$income)
performance5 <-
  performance(prediction5, measure = "tpr", x.measure = "fpr")
perfdata5 <-
  data.frame(FP = performance5@x.values[[1]], TP = performance5@y.values[[1]])

#==================================PLOT NAIVE-BAYESIAN=================================
prediction6 <- prediction(nb6.pred.prob, testing_set$income)
performance6 <-
  performance(prediction6, measure = "tpr", x.measure = "fpr")
perfdata6 <-
  data.frame(FP = performance6@x.values[[1]], TP = performance6@y.values[[1]])

#============================================ROC CURVE=============================================

g <- ggplot() +
  geom_line(data = dd, aes(x = FP, y = TP, color = 'Logistic Regression')) +
  geom_line(data = perfdata1, aes(x = FP, y = TP, color = 'Neural Networks')) +
  geom_line(data = perfdata2, aes(x = FP, y = TP, color = 'CART')) +
  geom_line(data = perfdata3, aes(x = FP, y = TP, color = 'Random Forest')) +
  geom_line(data = perfdata4, aes(x = FP, y = TP, color = 'Support Vector Machine')) +
  geom_line(data = perfdata4, aes(x = FP, y = TP, color = 'KNN')) +
  geom_line(data = perfdata4, aes(x = FP, y = TP, color = 'Naive Bayesian')) +
  geom_segment(aes(
    x = 0,
    xend = 1,
    y = 0,
    yend = 1
  )) +
  ggtitle('ROC Curve') +
  labs(x = 'False Positive Rate', y = 'True Positive Rate')


g +  scale_colour_manual(
  name = 'Classifier',
  values = c(
    'Logistic Regression' = '#E69F00',
    'Neural Networks' =
      '#56B4E9',
    'CART' = '#009E73',
    'Random Forest' =
      '#D55E00',
    'Support Vector Machine' = '#0072B2',
    'KNN' = '#4dffa6',
    'Naive Bayesian' = '#cc00ff'
  )
)


#=====================================AREA UNDER CURVE=====================================

auc <- rbind(
  performance(prediction0, measure = 'auc')@y.values[[1]],
  performance(prediction1, measure = 'auc')@y.values[[1]],
  performance(prediction2, measure = 'auc')@y.values[[1]],
  performance(prediction3, measure = 'auc')@y.values[[1]],
  performance(prediction4, measure = 'auc')@y.values[[1]]
)
rownames(auc) <-
  (
    c(
      'Logistic Regression',
      'Neural Networks',
      'CART',
      'Random Forest',
      'Support Vector Machine'
    )
  )
colnames(auc) <- 'Area Under ROC Curve'
round(auc, 4)



#View(adult)


#==================================FEATURE ENGINEERING===========================================
library(mlbench)
library(caret)
library(skimr)
library(randomForest)

bFE <- b
bFE$income <- as.numeric(m$income)
#1. Remove the Catagorical Variables
bFE$relationship <- NULL
bFE$marital_status <- NULL
bFE$race <- NULL
bFE$workclass <- NULL
bFE$fnlwgt <- NULL
bFE$occupation <- NULL
bFE$education <- NULL
bFE$origin <- NULL
#View(b)

#2. Pass the adult matrix to the prcomp function. Done below.
#3. Set two arguments, center and scale, to be TRUE. Assign your output to adult.pca.
bFE.pca <- prcomp(bFE, scale = TRUE, center = TRUE)

#4.	View your PCA object with summary().
summary(bFE.pca)

#5.	Call str(adult.pca)
str(bFE.pca)

#6. Plot the principle components.

#View(b)

#Number of prevalent score/Total number of rows * 100 = Percentage of histogram.
#7. Call biplot on data
ggbiplot(bFE.pca)

#8. Args Provided
ggbiplot(bFE.pca, labels = rownames(t))
ggbiplot(bFE.pca, var.scale = 0.1)


#9. Calculate Correlation Matrix
correlationMatrix <- cor(bFE)

#10.	summarize the correlation matrix
print(correlationMatrix)

#11.	find attributes that are highly corrected (e.g. >0.5)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)

#12.	print indexes of highly correlated attributes
print(highlyCorrelated)

#============================IMPORTANT FEATURE FINDING===================
#13.	prepare training scheme
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3)

#14. train the model using lvq
model <-
  train(
    income ~ .,
    data = bFE,
    method = "lvq",
    preProcess = "scale",
    trControl = control
  )

#15.	estimate variable importance
importance <- varImp(model, scale = FALSE)

#16.	summarize importance - Warning! This Big Dataset can cause R to crash!
print(importance)

#17.	plot importance - This can ALSO cause a crash...
plot(importance)

#=======================================NAIVE-BAYESIAN=============================================
library(klaR)
library(caret)
library(ElemStatLearn)
library(e1071)

#Naive Bayesian model
training_setx <- training_set[, -15]
training_sety <- as.factor(training_set$income)
testing_setx <- testing_set[, -15]
testing_sety <- as.factor(testing_set$income)

model <-
  train(training_setx,
        training_sety,
        'nb',
        trControl = trainControl(method = 'cv', number = 10)) #tells accuracy.

#Show the Model
model
predict(model$finalModel, testing_setx)

Predictions <-
  table(predict(model$finalModel, testing_setx)$income, testing_sety)
confusionMatrix(model)
?  ? confusionMatrix
#================================================KNN3
library(ISLR)
library(caret)

prop.table(table(training_set$income)) * 100
prop.table(table(testing_set$income)) * 100
prop.table(table(b$income)) * 100
trainX <- training_set[, names(training_set) != "Income"]
preprocessing <-
  preProcess(x = trainX, method = c("center", "scale"))
preprocessing

set.seed(400)
ctrl <-
  trainControl(method = "repeatedcv", repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
curveFit <-
  train(
    income ~ .,
    data = training_set,
    method = "knn",
    trControl = ctrl,
    preProcess = c("center", "scale"),
    tuneLength = 20
  )

#Output of kNN fit
curveFit

plot(curveFit)

knnPredict <- predict(curveFit, newdata = testing_set)
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing_set$income)

#============================================F-MEASURE=====================================================

#Non functioning
#F.measure.single(testing_set, testing_set_labels)
#F.measure.single.over.classes(income, prediction, g, root = "10")
#compute.mean.F.measure.single.over.classes(testing_sety)

#==============================K-FOLD CROSS NAVIGATION=======================================
library(tidyverse)
library(caret)
sample_n(b, 3)
View(b)
#Data Split into training and tested
set.seed(123)
training.samples <- b$education %>%
  createDataPartition(p = 0.8, list = FALSE)

training_set  <- b[training.samples,]
testing_set <- b[-training.samples,]

#Build model
model <- lm(education ~ ., data = training_set)
#Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(testing_set)
data.frame(
  R2 = R2(predictions, testing_set$education),
  RMSE = RMSE(predictions, testing_set$education),
  MAE = MAE(predictions, testing_set$education)
)



#===============================================EXTRAS========================================

#Removing NA variables - caused problems on coercion.
#b$workclass <- gsub("1",NA,b$workclass, fixed = TRUE)
#b$occupation <- gsub("1",NA,b$occupation, fixed = TRUE)
#b$origin <- gsub("1",NA,b$origin, fixed = TRUE)
#b<-na.omit(b)

#Random Sampling - No longer needed.
#set.seed(13)
#t <- sample(1:nrow(b), 100)
#t<- b[t, ]

#Describing Data
#m<-adult
#p <- ggplot(m, aes(colour = factor(relationship), marital_status)) # ENTITY <- ggplot(DATASET, aes(DATA FIELD,DATA FIELD))
#p + geom_histogram(binwidth = 10)
