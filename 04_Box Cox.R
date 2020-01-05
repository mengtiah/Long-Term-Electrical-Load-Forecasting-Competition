library(lubridate)
library(MLmetrics)
library(readxl)
library(forecast)

df <- read_excel('0_Data.xlsx')

tf=as.Date(df$Date)<as.Date("2011-01-01")
train=df[tf,]
#str(trn_tst)
bl=BoxCox.lambda(train$Load)
bl

tf2=as.Date(df$Date)<as.Date("2012-01-01")

box_value = BoxCox(train$Load,lambda = bl)
trn_tst=df[tf2,]

box <- BoxCox(trn_tst$Load, lambda = bl)
box_load=data.frame(box)

head(box_load)
write.csv(box_load,'04_Box_Cox_load.csv')

str(box_load)

train=read.csv('train_result.csv')
test=read.csv('test_result.csv')

MAPE(InvBoxCox(train$Train_Y,lambda = bl),InvBoxCox(train$Pred_Y_Train,lambda = bl))

MAPE(InvBoxCox(test$Test_Y,lambda = bl),InvBoxCox(test$Pred_Y,lambda = bl))

df <- read_excel('0_Data.xlsx')

tf=as.Date(df$Date)<as.Date("2012-01-01")
train=df[tf,]
#str(trn_tst)
bl=BoxCox.lambda(train$Load)
bl

tf2=as.Date(df$Date)<as.Date("2012-01-01")

box_value = BoxCox(train$Load,lambda = bl)
trn_tst=df[tf2,]

box <- BoxCox(trn_tst$Load, lambda = bl)
box_load=data.frame(box)

head(box_load)
write.csv(box_load,'04_2012_Box_Cox_load.csv')

