## Used Car_Price Predicition ##

train = read.csv('Data_Train.csv')
train = train[, -1]

attach(train)

str(train)    ## Structure if the dataset

train$Power = as.numeric(as.character(Power)) ## Convert Power var into Numeric var

colSums(is.na(train))   ## Check for NAs


#library(rpivotTable)
#rpivotTable(train)

## Outliers Study

boxplot(Kilometers_Driven)
boxplot(Power)
boxplot(Mileage)
boxplot(Engine)

## Remove Zero Seat & extream outlier from Kilometers_Driven

train = train[-c(4000,2329), ] 

attach(train)

Predict_New_Test = train

## Ommit NAs

train_new = na.omit(train)

colSums(is.na(train_new))

summary(train_new)
str(train_new)

attach(train_new)



## Check Target Variable Significant Level 

chisq.test(New_Price, Name)
chisq.test(New_Price, Location)
chisq.test(New_Price, Fuel_Type)
chisq.test(New_Price, Transmission)
chisq.test(New_Price, Owner_Type)

## NORMALIZE DATASET - ONLY VARS - Power,  Engine, Kilometer_Driven

library(MASS)
library(forecast)

hist(Power)  ## Power

BoxCox.lambda(Power)
Power = (1/Power)
Power = sqrt(Power)


hist(Engine)  ## Engine

BoxCox.lambda(Engine)
Engine = (1/Engine)
Engine = sqrt(Engine)

hist(Kilometers_Driven)  # Kilometer_Driven

BoxCox.lambda(Kilometers_Driven)
Kilometers_Driven = log(Kilometers_Driven)
Kilometers_Driven = (Kilometers_Driven^2)

attach(train_new)

## Due to High Correlation between independent Vraiable - Multicolinarity - 
## Use CART and NOt Linear Regression 


library(rpart)
library(rpart.plot)
library(rattle)


## *********************  PREDICT NEW PRICE  ********************** ##

ctrl_new_price = rpart.control(minsplit = 45, minbucket = 4, cp = 0, xval = 10)

model_new_price = rpart(formula = New_Price ~ ., data = train_new, method = 'anova', 
               control = ctrl_new_price)


printcp(model_new_price)

prun_new_price = prune(model_new_price, cp = 0.000103, 'cp')
fancyRpartPlot(prun_new_price)

par(mfrow = c(1,2))
rsq.rpart(prun_new_price)   


## Create Dataset Predict_New_Test for New_Price Predict Empty Dataset 

## 1st Convert NA - train$New_Price into 0 
## 2nd Creat dataset accordingly train$New_Price ==0
## 3rd Load subset - Predict_New_Price



library(imputeTS)

Predict_New_Test$New_Price = na.replace(Predict_New_Test$New_Price,0)

Predict_New_Test = Predict_New_Test[which(Predict_New_Test$New_Price == 0),]

colSums(is.na(Predict_New_Test))

Predict_New_Test = na.omit(Predict_New_Test)

## PREDICT NEW PRICE ##

Predict_New_Test$New_Price = predict(prun_new_price, Predict_New_Test, 
                                     method = 'anova',  interval = 'confident')

## Final Dataset 

train_final = rbind(train_new, Predict_New_Test)

write.csv(train_final, 'train_final.csv')



### **************  FINAL MODEL USED CAR - STUDY  ************** ###

train_used = read.csv('train_final.csv')

train_used = train_used[,-c(1)]

dim(train_used)
str(train_used)

attach(train_used)

# Check Missing Values

colSums(is.na(train_used))

# Check Outliers

boxplot(Kilometers_Driven)
boxplot(Mileage)
boxplot(Engine)
boxplot(Power)
boxplot(Seats)
boxplot(New_Price)

## Remove Outlier from Kilometers_Driven

train_used = train_used[-2769, ]


## Normilize traun_used : dataset for model building

library(MASS)
library(forecast)

hist(Kilometers_Driven)     # kilometer_Driven

BoxCox.lambda(Kilometers_Driven)
Kilometers_Driven = log(Kilometers_Driven)
Kilometers_Driven = (Kilometers_Driven^2)

hist(Power)     # Power

BoxCox.lambda(Power)
Power = (1/(sqrt(Power)))
Power = sqrt(Power)

hist(Engine)     # Engine

BoxCox.lambda(Engine)
Engine = (1/Engine)
Engine = log(Engine)
Engine = (Engine^2)
Engine = (1/Engine)

attach(train_used)

## WHY IT WAS IMP TO PREDICY NEW_PRICE FIRST - CHK CORRELATION 

cor(New_Price, Price)
# [1] 0.7228803


## BUILD CART MODEL ##


library(rpart)
library(rpart.plot)
library(rattle)

m.ctrl.used = rpart.control(minsplit = 170, minbucket = 10, cp = 0, xval = 10)

model.used = rpart(formula = Price ~ ., data = train_used, method = 'anova', 
               control = m.ctrl.used)

#fancyRpartPlot(model.used)

printcp(model.used)

model.used.prun = prune(model.used, cp = 0.000149, 'cp')
fancyRpartPlot(model.used.prun)

par(mfrow = c(1,2))
rsq.rpart(model.used.prun)

## Evaluate Result on Same Data

actual = train_used[, 12]
predict = train_used[, -12]

predict$Price = predict(model.used.prun, predict, 
                    method = 'anova', interval = 'confidence')

predict = predict[,12]

backtest = data.frame(actual, predict)


## Load Test File - To predict the Used Car Price

test = read.csv('Data_Test.csv')

test = test[,-c(1)]

test$Price = predict(model.used.prun, test, method = 'anova', interval = 'confidence')

used_care_1 = test$Price

write.csv(used_care_1, 'Final_Hackthon.xlsx')

~~~~~~~~~~~~~~~~~~~~~~
  
library(imputeTS)

test_new_price = test

test_new_price$New_Price = na.replace(test_new_price$New_Price,0)

test_new_price = test_new_price[which(test_new_price$New_Price == 0),]

colSums(is.na(test_new_price))

# Predict_New_Test = na.omit(Predict_New_Test)

## PREDICT NEW PRICE ##

test_new_price$New_Price = predict(prun_new_price, test_new_price, 
                                     method = 'anova',  interval = 'confident')

## Final Dataset 

test1 = na.omit(test)

test_new = rbind(test1, test_new_price)

## Predict 2

test_new$Price = predict(model.used.prun, test_new, method = 'anova', interval = 'confidence')

used_care_2 = test_new$Price

write.csv(used_care_2, 'Final_Submission_2.xlsx')



