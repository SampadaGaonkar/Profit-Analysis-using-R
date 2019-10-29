# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

# Building the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)

#Automatic Backward Elimination
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > SL){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)

#Creating new datasets
training_set1 = data.frame(R=training_set$R.D.Spend,P=training_set$Profit)
test_set1 = data.frame(R=test_set$R.D.Spend,P=test_set$Profit)
regressor_train1 = lm(formula = P ~ R,
               data = training_set1)
regressor_test1 = lm(formula = P ~ R,
                      data = training_set1)


# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set1$R, y = training_set1$P),
             colour = 'red') +
  geom_line(aes(x = training_set1$R, y = predict(regressor_train1, newdata = training_set1)),
            colour = 'blue') +
  ggtitle('R&D and Profit(Training set)') +
  xlab('R&D') +
  ylab('Profit')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set1$R, y = test_set1$P),
             colour = 'red') +
  geom_line(aes(x = training_set$R, y = predict(regressor_test1, newdata = training_set1)),
            colour = 'blue') +
  ggtitle('R&D and Profit(Test set)') +
  xlab('R&D') +
  ylab('Profit')

library(foreach)
library(doParallel)

#K means clustering
dataset1 = data.frame(dataset$R.D.Spend,dataset$Profit)
# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()

  foreach(i= 1:10, .combine=c) %do%{
    wcss[i] = sum(kmeans(dataset1, i)$withinss)}
#system.time(
#foreach(i = 1:40, .combine = c) %dopar% {
 # wcss[i] = sum(kmeans(dataset1,i)$withinss)})
registerDoParallel()

#---------
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')
# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset1, centers = 5)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of companies'),
         xlab = 'R&D',
         ylab = 'Profit ')

