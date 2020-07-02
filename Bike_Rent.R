setwd("C:/Users/USER/Downloads")
# Importing Dataset
data = read.csv('day.csv')
# Check Missing Values
sum(is.na(data))
# Output is 0 hence no missing values

# Converting categorical data as category
data$dteday= as.Date(data$dteday)
data$yr=as.factor(data$yr)
data$mnth=as.factor(data$mnth)
data$season = as.factor(data$season)
data$holiday= as.factor(data$holiday)
data$weekday= as.factor(data$weekday)
data$workingday= as.factor(data$workingday)
data$weathersit= as.factor(data$weathersit)

# Corelation
library(GGally)
library(ggplot2)
cor(data[10:16])
ggcorr(data[10:16],label=TRUE)
data = subset(data, select = -c(registered,casual,atemp) )

# Treating Outliers

boxplot(data$hum,main="Humidity")
Q=quantile(data$hum,c(.25,0.75))
IQR=IQR(data$hum)
m=mean(data$hum)
up=1.5*IQR+Q[2]
lo=Q[1]-1.5*IQR
data$hum[data$hum<lo]=m

boxplot(data$temp,main="Temperature")
 # Contains no outliers

boxplot(data$windspeed,main="Windspeed")
Q=quantile(data$windspeed,c(.25,0.75))
IQR=IQR(data$windspeed)
up=1.5*IQR+Q[2]
lo=1.5*IQR+Q[1]
m=mean(data$windspeed)
data$windspeed[data$windspeed>up]=m

boxplot(data$cnt,main="Count of Bike Rent")
 # contains no Outliers

# Visualizing data using Plots
ggplot(data,aes(x=yr,y=cnt))+geom_col()
ggplot(data,aes(x=yr,y=cnt,fill=season))+geom_col()
ggplot(data,aes(x=mnth,y=cnt))+geom_col()
ggplot(data,aes(x=mnth,y=cnt,fill=season))+geom_col()
ggplot(data,aes(x=season,y=cnt))+geom_col()
ggplot(data,aes(x=holiday,y=cnt))+geom_col()
ggplot(data,aes(x=workingday,y=cnt))+geom_col()
ggplot(data,aes(x=weathersit,y=cnt))+geom_col()

data = subset(data, select = -c(dteday,instant) )

# Encoding Categorical variables using OneHotEncoder
library(mltools)
library(data.table)
data = one_hot(as.data.table(data))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(data$cnt, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = cnt ~ .,data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Summarizing the Multiple Regression Model
summary(regressor)
library(caret)
RMSE(y_pred,test_set$cnt)
MAE(y_pred,test_set$cnt)
R2(y_pred,test_set$cnt)
mse(y_pred,test_set$cnt)

# Fitting Random Forest Regression to the dataset
library(randomForest)
set.seed(1234)
reg = randomForest(formula = cnt ~ .,data = training_set,ntree = 500)

# Predicting result with Random Forest Regression
y_pred_random = predict(reg, newdata = test_set)

# Summarizing the Random Forest Regression Model
summary(reg)
library(caret)
RMSE(y_pred_random,test_set$cnt)
MAE(y_pred_random,test_set$cnt)
R2(y_pred_random,test_set$cnt)
mse(y_pred_random,test_set$cnt)

