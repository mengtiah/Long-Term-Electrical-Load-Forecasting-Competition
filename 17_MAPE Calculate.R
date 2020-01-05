train=read.csv('Final_train_result.csv')
test=read.csv('Final_test_result.csv')

MAPE(InvBoxCox(train$Train_Y,lambda = bl),InvBoxCox(train$Pred_Y_Train,lambda = bl))

#MAPE(InvBoxCox(test$Test_Y,lambda = bl),InvBoxCox(test$Pred_Y,lambda = bl))

df1= data.frame(InvBoxCox(train$Pred_Y_Train,lambda = bl))
df2 = data.frame(InvBoxCox(test$Pred_Y,lambda = bl))

write.table(df1,'2008-2011 Predicted Load')
write.table(df2,'2020 Predicted Load')
