# Libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(corrplot)
library(DescTools)
library(lmtest)
library(faraway)
library(MASS)
library(glmnet)
library(plotmo) 
library(gbm)
library(caret)

# Data Cleaning
house = read_excel("Real estate valuation data set.xlsx")
house$`X8 year` = str_remove(house$`X1 transaction date`, "\\.\\d+")
house$`X7 month` = round(
  as.numeric(str_extract(house$`X1 transaction date`, "\\.\\d+"))*12
)
house$`X7 month`[is.na(house$`X7 month`)] = 12
house$`X8 year` = ifelse(house$`X1 transaction date` == 2013, 2012, house$`X8 year`)
house$`X1.transaction date` = paste0(house$`X7 month`,"-",house$`X8 year`)

# Preliminary Analysis
## Transaction Date Boxplot
date = ggplot(house, aes(x=factor(house$`X7 month`), y=house$`Y house price of unit area`, group = house$`X7 month`)) + 
  geom_boxplot(aes(fill=factor(house$`X7 month`))) + 
  facet_grid( .~ house$`X8 year`) +
  stat_summary(fun.y=mean, geom="point", color = "yellow") +
  labs(x ='Transaction Date', y = 'Price per Unit Area', fill='Months')  + 
  theme(legend.position="right")
print(date)

## Scheffe Test
ScheffeTest((aov(house$`Y house price of unit area` ~ factor(house$`X7 month`), data = house)))

## Numeric Summary
summary(house[,2:8])

## Characteristics among Each Variable
NumTransM = house %>% count(house$`X7 month`)
colnames(NumTransM) = c("Months","Counts")
TransByMonths = ggplot(NumTransM, aes(x=factor(NumTransM$Months), y=NumTransM$Counts, group = NumTransM$Months)) + 
  geom_bar(stat="identity",fill="steelblue")+
  labs(x ='Months', y = 'Transactions') + 
  theme_minimal()

NumTransS = house %>% count(house$`X4 number of convenience stores`)
colnames(NumTransS) = c("Stores","Counts")
TransByStore = ggplot(NumTransS, aes(x=factor(NumTransS$Stores), y=NumTransS$Counts)) + 
  geom_bar(stat="identity",fill="steelblue")+
  labs(x ='Number of Stores', y = 'Transactions')  +
  theme_minimal()

PriceByStore = ggplot(house, aes(x=factor(house$`X4 number of convenience stores`), y=house$`Y house price of unit area`)) + 
  geom_boxplot(aes(fill=factor(house$`X4 number of convenience stores`))) +
  stat_summary(fun.y=mean, geom="point", color = "yellow", shape=15, size = 0.5) +
  labs(x ='Number of Stores', y = 'House Price')  + 
  theme(legend.position="none")

PriceByMonths = ggplot(house, aes(x=factor(house$`X7 month`), y=house$`Y house price of unit area`)) + 
  geom_boxplot(aes(fill=factor(house$`X7 month`))) +
  stat_summary(fun.y=mean, geom="point", color = "yellow", shape=15, size = 0.5) +
  labs(x ='Months', y = 'Price')  + 
  theme(legend.position="none")

PriceByAge = ggplot(house, aes(x=house$`X2 house age`, y=house$`Y house price of unit area`)) + 
  geom_point() + 
  geom_smooth() +
  labs(x ='Age', y = 'Price')  + 
  theme(legend.position="none")

PriceByMRT = ggplot(house, aes(x=house$`X3 distance to the nearest MRT station`, y=house$`Y house price of unit area`)) + 
  geom_point() + 
  geom_smooth() +
  labs(x ='Distance to the Nearest MRT Station', y = 'Price')  + 
  theme(legend.position="none")

PriceHis = ggplot(house, aes(x = house$`Y house price of unit area`)) + 
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.05) +
  labs(x ='House Price')

AgeeHis = ggplot(house, aes(x = house$`X2 house age`)) + 
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.05) +
  labs(x ='House Age')

MRTHis = ggplot(house, aes(x = house$`X3 distance to the nearest MRT station`)) + 
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.05) +
  labs(x ='MRT Stations')

PriceByLatitude = ggplot(house, aes(x=house$`X5 latitude`, y=house$`Y house price of unit area`)) + 
  geom_point() + 
  geom_smooth() +
  labs(x ='Latitude', y = 'Price')  + 
  theme(legend.position="none")

PriceByLongitude = ggplot(house, aes(x=house$`X6 longitude`, y=house$`Y house price of unit area`)) + 
  geom_point() + 
  geom_smooth() +
  labs(x ='Longitude', y = 'Price')  + 
  theme(legend.position="none")

plot_grid(PriceHis, AgeeHis, MRTHis,PriceByAge, PriceByMRT, PriceByLatitude, PriceByLongitude,  PriceByStore, PriceByMonths, TransByMonths, TransByStore,labels = "AUTO")

## Map
price = ggplot(data = house, aes(x = `X6 longitude`, y = `X5 latitude`, color = `Y house price of unit area`)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "House Price", color = "Price per Unit Area") +
  scale_color_gradient(low = "lightblue", high = "darkblue")

age = ggplot(data = house, aes(x = `X6 longitude`, y = `X5 latitude`, color = `X2 house age`)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "House Age", color = "Year") +
  scale_color_gradient(low = "lightblue", high = "darkblue")

mrt = ggplot(data = house, aes(x = `X6 longitude`, y = `X5 latitude`, color = `X3 distance to the nearest MRT station`)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "MRT Stations", color = "Distancce(Meter)") +
  scale_color_gradient(low = "lightblue", high = "darkblue")

stores = ggplot(data = house, aes(x = `X6 longitude`, y = `X5 latitude`, color = `X4 number of convenience stores`)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", title = "Convenience Stores", color = "Total Number") +
  scale_color_gradient(low = "lightblue", high = "darkblue")

plot_grid(price, age, mrt, stores, labels = "AUTO")

## Correlations
corrplot(cor(house[,2:8]),method="pie", type="upper", order="hclust", sig.level = 0.05)

# Multiple Linear Regression
full_model = lm(HousePrice ~ TransactionDate + HouseAge + DistanceToMRT + NumberOfStores + Latitude + Longitude, data = SelectData)

summary(full_model)

full_train_mse = mean((train$HousePrice-predict(full_model))^2)
full_test_mse = mean((test$HousePrice-predict(full_model))^2)

## Diagnostic Analysis 
### VIF
Vif = as.data.frame(sort(vif(full_model), decreasing = TRUE))

### Independence of Error
dw = dwtest(SelectData$HousePrice ~ SelectData$TransactionDate + SelectData$HouseAge + SelectData$DistanceToMRT + SelectData$NumberOfStores + SelectData$Latitude + SelectData$Longitude)

### Residuals
plot(full_model)

### High Leverages
n = length(SelectData$HousePrice)
p = 6
#2*p/n
lev = influence(full_model)$hat
#lev[lev>2*p/n]
leverages=halfnorm(lev,4)

### High-influential Points
cook = cooks.distance(full_model)
halfnorm(cook, 4, ylab="Cook's distances")

### Outliers
checkout = rstudent(full_model)
r = abs(qt(0.05/(2*n),n-p-1))
check = sort(checkout,decreasing = TRUE)
out = rbind(r,check[check>r])

### Training and Testing Data without Outlier
SelectData0 = SelectData[-271,-1]
sample_size <- floor(0.8 * nrow(SelectData0))
set.seed(123)
train_smp <- sample(nrow(SelectData0), size = sample_size)
train <- SelectData0[train_smp, ]
y_train = train$HousePrice
x_train = train[,-ncol(train)]
test <- SelectData0[-train_smp, ]
y_test = test$HousePrice
x_test = test[,-ncol(test)]


### New Full Model
full.model = lm(HousePrice ~ TransactionDate + HouseAge + DistanceToMRT + NumberOfStores + Latitude + Longitude, data = train)

### Sequential ANOVA Model
interaction = lm(HousePrice ~ TransactionDate * HouseAge * DistanceToMRT * NumberOfStores * Latitude * Longitude, data = train)

### Goodness of Fit
anova(full.model,interaction)

## BoxCox Transformation
boxcox(full.model)

### Transformed Model
trans_model = lm(HousePrice^(2/11) ~ TransactionDate + HouseAge + log(DistanceToMRT) + NumberOfStores + Latitude + Longitude, data = train)

summary(trans_model)

trans_train_mse = mean((test$HousePrice-predict(trans_model))^2)
trans_test_mse = mean((test$HousePrice-predict(trans_model))^2)

# Ridge Regression
ridge_train = train
ridge_train$DistanceToMRT = log(ridge_train$DistanceToMRT)
ridge_train$HousePrice = (ridge_train$HousePrice)^(2/11)

ridge = cv.glmnet(x = data.matrix(ridge_train[,-ncol(ridge_train)]), y = ridge_train$HousePrice, nfolds = 10, alpha = 0)
par(mfrow = c(1,2))
plot(ridge)
plot_glmnet(ridge$glmnet.fit, "lambda")

ridge_predicted <- predict(ridge, s = "lambda.min", newx = data.matrix(ridge_train[,-ncol(ridge_train)]))

ridge_train_mse=mean((ridge_predicted - train$HousePrice)^2)
ridge_test_mse=mean((ridge_predicted - test$HousePrice)^2)

ridge_sst <- sum((ridge_train$HousePrice - mean(ridge_train$HousePrice))^2)
ridge_sse <- sum((ridge_predicted - ridge_train$HousePrice)^2)
ridge_rsq <- 1 - ridge_sse / ridge_sst

# Gradient Boosting Regression
gbm = gbm(HousePrice ~.,data = train, distribution = "gaussian",cv.folds = 10, shrinkage = .01, n.minobsinnode = 10, n.trees = 500)
summary(gbm)

gbm_train_mse=mean((train$HousePrice-predict.gbm(gbm, x_test))^2)
gbm_test_mse=mean((test$HousePrice-predict.gbm(gbm, x_test))^2)

gbm_tss =  sum((y_test - mean(y_test))^2 )
gbm_rss =  sum((y_test-predict.gbm(gbm, x_test))^2)
gbm_rsq  =  1 - (gbm_rss/gbm_tss)

