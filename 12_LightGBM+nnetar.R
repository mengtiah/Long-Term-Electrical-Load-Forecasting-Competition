library(rvest)
library(dplyr)
library(forecast)
library(ggplot2)
library(seasonal)
library(lubridate)
library(scales)
library(tidyverse)
library(zoo)
library(readr)
library(stringr)
library(DT)
library(tools)
library(prophet)
library(readxl)
library(timeDate)

data = read.csv("06_2010_625_Variables_BoxL.csv")
cols= read.csv('LightGBM_Feature Selected.csv')
data$Date=as.Date(data$Date)
data <- na.omit(data)
df.train = data[data$Date<'2011-01-01',]
df.test = data[(data$Date<'2012-01-01') & (data$Date>='2011-01-01'),]

#drops <- c('Load_BoxCox','Load','Date','Hour','month','weekday','quarter','month.week')
df.train.x = df.train[ , names(df.train %in% cols)]
df.test.x = df.test[ , names(df.test %in% cols)]


nn_model = nnetar(ts(df.train$Load_BoxCox,frequency = 24), p = 72, P = 2, size = 10, xreg = df.train.x)
forecasttest = forecast(nn_model,h=nrow(df.test), xreg=df.test.x) 

bl=0.239913526672966

MAPE(InvBoxCox(nn_model$x[73:26256],lambda = bl),InvBoxCox(nn_model$fitted[73:26256],lambda = bl))

MAPE(InvBoxCox(forecasttest$mean,lambda = bl),InvBoxCox(df.test$Load_BoxCox,lambda = bl))

