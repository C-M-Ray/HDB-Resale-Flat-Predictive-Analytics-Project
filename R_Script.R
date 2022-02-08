library(data.table)
library(dplyr)
library(caTools)
library(earth)
library(randomForest)
library(car)

setwd("C:/Users/Min Ray/Desktop")

#Data Preparation 

data1 = fread('resale-flat-prices-201701-202103.csv', stringsAsFactors = T)
summary(data1)
sapply(data1, class)

data1$remaining_lease_yrs = as.integer(substr(data1$remaining_lease,0,2))
class(data1$remaining_lease_yrs)

data1[,c("lease_commence_date", "remaining_lease"):=NULL]
summary(data1)

data1$block_street = as.factor(paste(data1$block, sep = " ", data1$street_name))
class(data1$block_street)

data1[,c("block", "street_name"):=NULL]
summary(data1)

#Data Exploration

head(data1 %>% count(month)%>% arrange(n),1) #month with the lowest transaction volume

head(data1 %>% count(month)%>% arrange(desc(n)),1) #month with the highest transaction volume

head(data1 %>% count(town)%>% arrange(n),1) #town with the lowest transaction volume

head(data1 %>% count(town)%>% arrange(desc(n)),1) #town with the highest transaction volume

data1 %>% slice_max(resale_price, n = 5) %>% 
  select(resale_price,flat_type,block_street,town,floor_area_sqm, storey_range) #top 5 resale prices

data1 %>% slice_min(resale_price, n = 5) %>% 
  select(resale_price,flat_type,block_street,town,floor_area_sqm, storey_range) #bottom 5 resale prices

#Data Visualisation (More in Excel)

boxplot(data1$resale_price, ylab = "Resale Price ($)", main = "Resale Price of Flats")

boxplot(data1$remaining_lease_yrs, ylab = "Remaining Lease (Years)", main = "Remaining Lease Years of Resale Flats")

#Modifying the data

data2 = data1 #Making a copy of the data

#Removed flat_type "1 ROOM", "MULTI-GENERATION", "2-room", "Premium Maisonette" and "ImprovedMaisonette" cases from data2 as their occurrence is low

data2 = data2[flat_type!="1 ROOM"]
data2 = data2[flat_type!="MULTI-GENERATION"]
data2 = data2[flat_model!="2-room"]
data2 = data2[flat_model!="Premium Maisonette"]
data2 = data2[flat_model!="Improved-Maisonette"]

data2 = droplevels(data2)
summary(data2$flat_type)
levels(data2$flat_type)

#Removed block_street from data2 as there are too many unique categorical variables 

data2[,"block_street":=NULL]
summary(data2)

#Create new categorical level encompassing floors 40 to 51 due to low occurrence of these floors individually.

sum(sum(data2$storey == "40 TO 42"), sum(data2$storey == "43 TO 45"), 
    sum(data2$storey == "46 TO 48"), sum(data2$storey == "49 TO 51"))

data2$storey = data2$storey_range

data2$storey = (factor(data2$storey,levels=c(levels(data2$storey),"40 TO 51")))

data2$storey[(data2$storey) == "40 TO 42"] = "40 TO 51"
data2$storey[(data2$storey) == "43 TO 45"] = "40 TO 51"
data2$storey[(data2$storey) == "46 TO 48"] = "40 TO 51"
data2$storey[(data2$storey) == "49 TO 51"] = "40 TO 51"

data2 = droplevels(data2)

levels(data2$storey)
summary(data2$storey)
class(data2$storey)

data2[,"storey_range":=NULL]
summary(data2)

#Train-test split

set.seed(2021)

train = sample.split(Y=data2$resale_price, SplitRatio = 0.7)
trainset = subset(data2, train==T)
testset = subset(data2, train==F)

results = data.frame(matrix(ncol = 3, nrow = 0, dimnames= list(NULL, c("Model","Trainset RMSE","Testset RMSE"))))

#Linear regression (all predictor variables)

set.seed(2021)
m.lr = lm(resale_price ~ ., data=trainset)
summary(m.lr)

yhat.lr = predict(m.lr, newdata = testset)
RMSE.lr.test = round(sqrt(mean((yhat.lr - testset$resale_price)^2)))
RMSE.lr.test 

yhat.lr = predict(m.lr, newdata = trainset)
RMSE.lr.train = round(sqrt(mean((yhat.lr - trainset$resale_price)^2)))
RMSE.lr.train 

results[nrow(results) + 1, ] = c("Linear Regression", RMSE.lr.train, RMSE.lr.test)

vif(m.lr)

#MARS degree 2

set.seed(2021)
m.mars = earth(resale_price ~ .,degree=2,data=trainset)
summary(m.mars)

yhat.mars = predict(m.mars, newdata = testset)
RMSE.mars.test = round(sqrt(mean((yhat.mars - testset$resale_price)^2)))
RMSE.mars.test 

yhat.mars = predict(m.mars, newdata = trainset)
RMSE.mars.train = round(sqrt(mean((yhat.mars - trainset$resale_price)^2)))
RMSE.mars.train

results[nrow(results) + 1, ] = c("MARS degree 2", RMSE.mars.train, RMSE.mars.test)

var.impt.mars = evimp(m.mars)
print(var.impt.mars) #floor area is most important

#Random Forest

set.seed(2021)
m.rf = randomForest(resale_price ~., data=trainset, importance= T)
summary(m.rf)
plot(m.rf)

yhat.rf = predict(m.rf, newdata = testset)
RMSE.rf.test <- round(sqrt(mean((yhat.rf - testset$resale_price)^2)))
RMSE.rf.test

yhat.rf = predict(m.rf, newdata = trainset)
RMSE.rf.train <- round(sqrt(mean((yhat.rf - trainset$resale_price)^2)))
RMSE.rf.train

results[nrow(results) + 1, ] = c("Random Forest", RMSE.rf.train, RMSE.rf.test)

varImp_RF = importance(m.rf)
varImpPlot(m.rf, type = 1)

results 

m.rf

OOB_MSE = 1569905160 
OOB_RMSE = sqrt(1569905160)
OOB_RMSE