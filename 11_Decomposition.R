library(rvest)
library(dplyr)
library(forecast)
library(ggplot2)
library(fpp2)
library(seasonal)
library(Ecdat)
library(lubridate)
library(MLmetrics)

data = read.csv("03_2010_df_625 Variables.csv")

tsdata = data[,c("Date","Load")]
tsdata$Date=as.Date(tsdata$Date)
ts_train = tsdata[tsdata$Date<'2011-01-01',]
ts_test = tsdata[(tsdata$Date<'2012-01-01') & (tsdata$Date>='2011-01-01'),]
head(tsdata)
glimpse(tsdata)
class(tsdata$Load)
ms=msts(ts_train$Load,seasonal.periods=c(7,2*7,30.5),ts.frequency=30.5)
autoplot(ms)
model=mstl(ms) # lambda = 0 do the multiplicative decomposition
autoplot(model)
fc=forecast(model,h = nrow(ts_test), level = 0,allow.multiplicative.trend = T)
fc$fitted

trainerror=data.frame(accuracy(fc$x,fc$fitted))
testerror=data.frame(accuracy(fc$mean[1:8640],ts_test$Load[1:8640]))
testerror

#### use boxcox (give up)
bl=BoxCox.lambda(ts_train$Load)
bl
box_Load=read.csv('04_Box_Cox_load.csv')
box_train = box_Load[tsdata$Date<'2011-01-01',]
box_test = box_Load[(tsdata$Date<'2012-01-01') & (tsdata$Date>='2011-01-01'),]

ms2=msts(box_train$box,seasonal.periods=c(7,2*7,30.5,30.5*3),ts.frequency=365)
autoplot(ms2)
model2=mstl(ms2) # lambda = 0 do the multiplicative decomposition
autoplot(model2)
fc2=forecast(model2,h = nrow(box_test), level = 0,allow.multiplicative.trend = T)
fc2$fitted

trainerror2=data.frame(accuracy(fc2$x,fc2$fitted))
testerror2=data.frame(accuracy(fc2$mean,box_test$X))
testerror2

MAPE(InvBoxCox(fc2$x,lambda = bl),InvBoxCox(fc2$fitted,lambda = bl))

MAPE(InvBoxCox(fc2$mean[1:8640],lambda = bl),InvBoxCox(box_test$box[1:8640],lambda = bl))


