rm(list = ls())
energy.df<- read.csv("C:/Users/sahit/OneDrive/Documents/BDA/Project/EnergyData.csv")
# Use gsub to remove stars from Column1
#df$Address <- gsub("\\*", "", tel.df$Address)

# View the updated data frame
summary(energy.df)
head(energy.df)
class(energy.df)
names(energy.df)
str(energy.df)

# Renaming column names for easier interpretation
##colnames(crash_df) <- c('Report Number','ACRS Report Type','Crash Date','Crash Time','Route Type',
##                  'Collision Type','Weather','Surface Condition','Light','Traffic Control',
##                'Driver Substance Abuse','Injury Severity','Drivers License State',
##               'Vehicle Body Type','Speed Limit')

colnames(energy.df) <- c('Relative_Compactness',
                      'Surface_Area',
                      'Wall_Area',
                      'Roof_Area',
                      'Overall_Height',
                      'Orientation',
                      'Glazing_Area',
                      'Glazing_Area_Distribution',
                      'Heating_Load',
                      'Cooling_Load')
?colSums
colSums(is.na(energy.df) | energy.df == "")

df %>% 
  pivot_longer(c(Carrier..1, Carrier..2,Carrier..3,Carrier..4,Carrier..5,Carrier..6,
                 Carrier..7,Carrier..8,Carrier..9), 
               names_to = "Carrier", values_to = "c_name")

boxplot(energy.df)

energy.df[,1:8] <- scale(energy.df[,1:8])

boxplot(energy.df)


options(digits = 3)
format(colMeans(energy.df[,1:8]), scientific = FALSE)

# partition data

options(warn=-1)

#Seed value will help generating same set of randomisation in every run
set.seed(22, sample.kind = "Rounding")
train.index <- sample(c(1:dim(energy.df)[1]), dim(energy.df)[1]*0.65)  
train.df <- energy.df[train.index,]
valid.df <- energy.df[-train.index,]

mean(train.df$Heating_Load)
mean(valid.df$Heating_Load)


#-------------Linear Regression on Heating load------------------

set.seed(2, sample.kind = "Rounding")
energy.lm <- lm(Heating_Load ~ ., data = train.df)
options(scipen = 999)
summary(energy.lm)   # Get model summary
# Plot barchart of coefficients
library(lattice)
barchart(energy.lm$coefficients)

# Make predictions on a new set. 
library(forecast)
energy.lm.pred.hl <- predict(energy.lm, valid.df)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(energy.lm.pred.hl, valid.df$Heating_Load)


# Visually check residuals
energy.lm.pred <- predict(energy.lm, valid.df)
all.residuals <- valid.df$Heating_Load - energy.lm.pred
# The majority of residual values fall into [-1406, 1406]
length(all.residuals)
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

#Now we will apply linear regression to get a prediction for the cooling load

set.seed(11, sample.kind = "Rounding")

energy.lm <- lm(Cooling_Load ~ ., data = train.df)
options(scipen = 999)
summary(energy.lm)   # Get model summary

# Plot barchart of coefficients
library(lattice)
barchart(energy.lm$coefficients)

# Make predictions on a new set. 
library(forecast)
energy.lm.pred.cl <- predict(energy.lm, valid.df)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(energy.lm.pred.cl, valid.df$Cooling_Load)


# Visually check residuals
energy.lm.pred <- predict(energy.lm, valid.df)
all.residuals <- valid.df$Cooling_Load - energy.lm.pred
# The majority of residual values fall into [-1406, 1406]
length(all.residuals)
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")


#-------------------Random Forest-------------------------
library(randomForest)
rf <- randomForest(Heating_Load ~ ., data = train.df, ntree = 400, 
                   mtry = 4, nodesize = 5, importance = TRUE)
summary(rf)
varImpPlot(rf, type = 1)

# Make predictions on a new set. 
library(forecast)
energy.rf.pred.hl <- predict(rf, valid.df)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(energy.rf.pred.hl, valid.df$Heating_Load)
#RF for cooling Load
library(randomForest)
rf <- randomForest(Cooling_Load ~ ., data = train.df, ntree = 400, 
                   mtry = 4, nodesize = 5, importance = TRUE)
summary(rf)
varImpPlot(rf, type = 1)

# Make predictions on a new set. 
library(forecast)
energy.rf.pred.cl <- predict(rf, valid.df)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(energy.rf.pred.cl, valid.df$Cooling_Load)


#----------------------KNN--------------------------------------
#Build KNN with Train function for Heating Load
# Find optimal K
library(caret)
set.seed(105)
grid1 <- expand.grid(.k = seq(2, 20, by = 1))
control <- trainControl(method = "cv")
knn.train <- train(Heating_Load ~ ., data = train.df,
                   method = "knn",
                   trControl = control,
                   tuneGrid = grid1)
knn.train
plot(knn.train)

knn.pred.hl <- predict(knn.train, newdata = valid.df)

knn.reg=RMSE(knn.pred.hl, valid.df$Heating_Load)
knn.reg

#Build KNN with Train function for Cooling Load
set.seed(207)
grid1 <- expand.grid(.k = seq(2, 20, by = 1))
control <- trainControl(method = "cv")
knn.train <- train(Cooling_Load ~ ., data = train.df,
                   method = "knn",
                   trControl = control,
                   tuneGrid = grid1)
knn.train
plot(knn.train)

knn.pred.cl <- predict(knn.train, newdata = valid.df)
knn.pred.cl
knn.reg=RMSE(knn.pred.cl, valid.df$Cooling_Load)
knn.reg.cl


#------------create ensemble model for linear,RandomForest and KNN--------------
#heatingload
heating_predictions <- data.frame("lm" = energy.lm.pred.hl,
                            "RF" = energy.rf.pred.hl,
                            "KNN" = knn.pred.hl)
ensemble_predictions_hl <- rowMeans(heating_predictions)    
heating_predictions$ensemble <- ensemble_predictions_hl
ensemble_rmse_hl <- RMSE(ensemble_predictions_hl,valid.df$Heating_Load)
ensemble_rmse_hl

#ensemble for coolingload

cooling_predictions <- data.frame("lm" = energy.lm.pred.cl,
                            "RF" = energy.rf.pred.cl,
                            "KNN" = knn.pred.cl)
ensemble_predictions_cl <- rowMeans(cooling_predictions)                            

cooling_predictions$ensemble <- ensemble_predictions_cl

ensemble_rmse_cl <- RMSE(ensemble_predictions_cl,valid.df$Cooling_Load)
ensemble_rmse_cl


#-----------------------Neural Network-------------------------------

# install.packages ("neuralnet")
# install.packages ("nnet")
#install.packages("matrixStats")
library(neuralnet)
library(nnet)
library(caret)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(elasticnet)
library(knitr)
library(matrixStats)

rm(list = ls())

energy.df <- read.csv("EnergyData.csv")
colnames(energy.df) <- c('Relative_Compactness',
                         'Surface_Area',
                         'Wall_Area',
                         'Roof_Area',
                         'Overall_Height',
                         'Orientation',
                         'Glazing_Area',
                         'Glazing_Area_Distribution',
                         'Heating_Load',
                         'Cooling_Load')

# Random sampling
samplesize = 0.60 * nrow(energy.df)
set.seed(80)
index = sample( seq_len ( nrow ( energy.df ) ), size = samplesize )

# Create training and test set
datatrain = energy.df[ index, ]
datatest = energy.df[ -index, ]

# Scale data for neural network
max = apply(energy.df , 2 , max)
min = apply(energy.df, 2 , min)
scaled = as.data.frame(scale(energy.df, center = min, scale = max - min))

# install library
install.packages("neuralnet ")

# load library
library(neuralnet)

# creating training and test set
trainset = scaled[index , ]
testset = scaled[-index , ]

#Fit Neural Network (Hidden Nodes/Layers)
library(neuralnet)
nn_hd <- neuralnet(Heating_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Area_Distribution, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
plot(nn_hd)


#Test the resulting output/Prediction using neural network

temp_test <- subset(testset, select = c("Relative_Compactness", "Surface_Area", "Wall_Area", "Roof_Area", "Overall_Height", "Orientation", "Glazing_Area", "Glazing_Area_Distribution"))
nn_hd.results <- neuralnet::compute(nn_hd, temp_test)
nn_hd.results = (nn_hd.results$net.result * (max(energy.df$Heating_Load) - min(energy.df$Heating_Load))) + min(energy.df$Heating_Load)
plot(datatest$Heating_Load, nn_hd.results, col='blue', pch=16, ylab = "predicted heating load NN", xlab = "real heating load")
abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$Heating_Load - nn_hd.results)^2) / nrow(datatest)) ^ 0.5
RMSE.NN

#Accuracy for Heating Load
comparison=data.frame(nn_hd.results,datatest$Heating_Load)
deviation=((datatest$Heating_Load-nn_hd.results)/datatest$Heating_Load)
comparison=data.frame(nn_hd.results,datatest$Heating_Load,deviation)
accuracy=1-abs(mean(deviation))
accuracy

#Cooling Load

#Fit Neural Network
library(neuralnet)
nn_cd <- neuralnet(Cooling_Load ~ Relative_Compactness + Surface_Area + Wall_Area + Roof_Area + Overall_Height + Orientation + Glazing_Area + Glazing_Area_Distribution, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
plot(nn_cd)


#Test the resulting output/Prediction using neural network

temp_test_cd <- subset(testset, select = c("Relative_Compactness", "Surface_Area", "Wall_Area", "Roof_Area", "Overall_Height", "Orientation", "Glazing_Area", "Glazing_Area_Distribution"))
nn_cd.results <- neuralnet::compute(nn_hd, temp_test_cd)
nn_cd.results = (nn_cd.results$net.result * (max(energy.df$Cooling_Load) - min(energy.df$Cooling_Load))) + min(energy.df$Cooling_Load)
plot(datatest$Cooling_Load, nn_cd.results, col='blue', pch=16, ylab = "predicted cooling load NN", xlab = "real cooling load")
abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$Cooling_Load - nn_cd.results)^2) / nrow(datatest)) ^ 0.5
RMSE.NN

#Accuracy for Cooling Load
comparison=data.frame(nn_cd.results,datatest$Cooling_Load)
deviation=((datatest$Cooling_Load-nn_cd.results)/datatest$Cooling_Load)
comparison=data.frame(nn_cd.results,datatest$Cooling_Load,deviation)
accuracy=1-abs(mean(deviation))
accuracy

# install.packages("NeuralNetTools")
library(NeuralNetTools)
# Plot neural net
par(mfcol=c(1,1))
plotnet(nn_hd)
plotnet(nn_cd)
# get the neural weights
neuralweights(nn_hd)
neuralweights(nn_cd)
# Plot the importance for Heating Load
olden(nn_hd)
# Plot the importance for Cooling Load
olden(nn_cd)


#----------Parameter Tuning------------------------
fitControl <- trainControl(
  method = "cv",
  number = 6,
  savePredictions = 'final',
  classProbs = T)

predictors<-c("Relative_Compactness", "Surface_Area", "Wall_Area", "Roof_Area", "Overall_Height", "Orientation", "Glazing_Area", "Glazing_Area_Distribution")
outcomeName<-c("Heating_Load")
outcomeName_1<-c("Cooling_Load")

#Heating Load
nntrain<-trainset[,predictors]
nntrain$Heating_Load<-factor(trainset[,outcomeName])
nnvalid<-testset

levels(nntrain$Heating_Load) <- make.names(levels(factor(nntrain$Heating_Load)))

model_nn<-train(nntrain[,predictors],nntrain[,outcomeName],method='nnet',
                trControl=fitControl,tuneLength=3)

nnvalid.class<-predict(object = model_nn,testset[,predictors])
nnvalid.class<-ifelse(nnvalid.class=="X1",1,0)
RMSE.NN = (sum((testset$Heating_Load - nnvalid.class)^2) / nrow(testset)) ^ 0.5
RMSE.NN

#Cooling Load
nntrain<-trainset[,predictors]
nntrain$Cooling_Load<-factor(trainset[,outcomeName_1])
nnvalid<-testset

levels(nntrain$Cooling_Load) <- make.names(levels(factor(nntrain$Cooling_Load)))

model_nn<-train(nntrain[,predictors],nntrain[,outcomeName_1],method='nnet',
                trControl=fitControl,tuneLength=3)

nnvalid.class<-predict(object = model_nn,testset[,predictors])
nnvalid.class<-ifelse(nnvalid.class=="X1",1,0)
RMSE.NN = (sum((testset$Cooling_Load - nnvalid.class)^2) / nrow(testset)) ^ 0.5
RMSE.NN





