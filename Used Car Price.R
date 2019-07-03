## Used Car_Price Predicition ##

train = read.csv('Data_Train.csv')

attach(train)

str(train)    ## Structure if the dataset

train$Power = as.numeric(as.character(Power)) ## Convert Power var into Numeric var

colSums(is.na(train))   ## Check for NAs


#library(rpivotTable)
#rpivotTable(train)

train = train[-4000,]   #3 Remove Zero Seat obersavation - Makes No sence

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
Power = 1/ Power
Power = sqrt(Power)


hist(Engine)  ## Engine

BoxCox.lambda(Engine)
Engine = 1 / Engine
Engine = sqrt(Engine)

hist(Kilometers_Driven)  # Kilometer_Driven

BoxCox.lambda(Kilometers_Driven)
Kilometers_Driven = log(Kilometers_Driven)
Kilometers_Driven = Kilometers_Driven^2

attach(train_new)

library(ggplot2)
library(ggcorrplot)

corp = train_new[, -c(1,2,3,5,6,7)]

ggcorrplot(cor(corp), method = 'circle', type = 'lower')

## Due to High Correlation between independent Vraiable - Multicolinarity 
## Use CART


library(rpart)
library(rpart.plot)
library(rattle)


m.ctrl = rpart.control(minsplit = 90, minbucket = 10, cp = 0, xval = 10)

model1 = rpart(formula = New_Price ~ ., data = train_new, method = 'anova', 
               control = m.ctrl)

fancyRpartPlot(model1)

printcp(model1)

model1.prun = prune(model1, cp = 0.000429, 'cp')
fancyRpartPlot(model1.prun)

par(mfrow = c(1,2))
rsq.rpart(model1.prun)


## Create Dataset Predict_New_Test for New_Price Predict Empty Dataset 

## 1st Convert NA - train$New_Price into 0 
## 2nd Creat dataset accordingly train$New_Price ==0
## 3rd Load subset - Predict_New_Price



library(imputeTS)

Predict_New_Test$New_Price = na.replace(Predict_New_Test$New_Price,0)

Predict_New_Test = Predict_New_Test[which(Predict_New_Test$New_Price == 0),]

colSums(is.na(Predict_New_Test))

Predict_New_Test = na.omit(Predict_New_Test)

Predict_New_Test$New_Price = predict(model1.prun, Predict_New_Test, method = 'anova',  interval = 'confident')

## Final Dataset 

train_final = rbind(train_new, Predict_New_Test)

write.csv(train_final, 'train_final.csv')









