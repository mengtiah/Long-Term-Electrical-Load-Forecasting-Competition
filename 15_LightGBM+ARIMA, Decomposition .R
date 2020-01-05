train=read.csv('train_result.csv')
test=read.csv('test_result.csv')

MAPE(InvBoxCox(train$Train_Y,lambda = bl),InvBoxCox(train$Pred_Y_Train,lambda = bl))

MAPE(InvBoxCox(test$Test_Y,lambda = bl),InvBoxCox(test$Pred_Y,lambda = bl))

predict_train=InvBoxCox(train$Pred_Y_Train,lambda = bl)
predict_test=InvBoxCox(test$Pred_Y,lambda = bl)

train_y=InvBoxCox(train$Train_Y,lambda = bl)
test_y=InvBoxCox(test$Test_Y,lambda = bl)

residual_y_train=train_y-predict_train
residual_y_test=test_y-predict_test

y.train=ts(residual_y_train,frequency = 24)
y.test=ts(residual_y_test,frequency = 24)

ms2=msts(y.train,seasonal.periods=c(7,2*7,30.5,30.5*3),ts.frequency=365)
autoplot(ms2)
model2=mstl(ms2) # lambda = 0 do the multiplicative decomposition
autoplot(model2)
fc2=forecast(model2,h = length(y.test), level = 0,allow.multiplicative.trend = T)
fc2$fitted

trainerror2=data.frame(accuracy(fc2$x,fc2$fitted))
testerror2=data.frame(accuracy(fc2$mean,y.test))
testerror2


M3.2=stl(y.train,s.window = 'periodic',robust=T) #s.window = determine the seasonal component
#robust=T (recommend) ogical indicating if robust fitting be used in the loess procedure.
autoplot(M3.2)

?seasadj #Returns seasonally adjusted data constructed by removing the seasonal component.

seasadj(M3.2) 
seasadj(M3.1)

# Forecast using ts decomposition

#use naive
M3.2 %>% forecast(h=length(y.test), method='naive')
M3.2F = forecast(M3.2, h=length(y.test), method='naive')

accuracy(M3.2F$fitted[2:25256], y.test[2:25256])

# use SES
M3.2F = forecast(M3.2, h=length(y.test), method='ets')

accuracy(M3.2F$fitted, y.test)

# use ARIMA
M3.2F = forecast(M3.2, h=length(y.test), method='arima')

accuracy(M3.2F$fitted, y.test) 

