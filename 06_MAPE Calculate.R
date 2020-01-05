train=read.csv('train_result.csv')
test=read.csv('test_result.csv')

MAPE(InvBoxCox(train$Train_Y,lambda = bl),InvBoxCox(train$Pred_Y_Train,lambda = bl))

MAPE(InvBoxCox(test$Test_Y,lambda = bl),InvBoxCox(test$Pred_Y,lambda = bl))

