## Used Car_Price Predicition ##

train = read.csv('Data_Train.csv')

attach(train)

str(train)    ## Structure if the dataset

train$Power = as.numeric(as.character(Power))  ## Convert Power var into Numeric var

colSums(is.na(train))   ## Check for NAs

library(rpivotTable)
rpivotTable(train)

## NOTES : 
## 1. Year -  Merge 98,99,00
## 2. Seats -  Remove 0
## 3. Predict New_Proce based on Proice..ANd then use model on test data
## 4. Can we Pridict 500 then all together 100 then 1500 .. like for test NEw_Price pred.


train = na.omit(train)
