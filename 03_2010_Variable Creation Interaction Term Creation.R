library(dplyr)
library(ggplot2)
library(readxl)
library(leaps)
library(MLmetrics)
library(forecast)
library(timeDate)
library(zoo)
library(lubridate)

df = read.csv('02_new_data_join_2010.csv')
drops <- c("X")
df=df[ , !(names(df) %in% drops)]
colnames(df)

df$Temp.51.7. = as.integer(df$Temp.51.7.)-1

temp_cols=colnames(df[,grepl("temp_", colnames(df))])

for (i in temp_cols){
  df[, ncol(df)+1] <- abs(df[i])^0.5*(-1)^(df[i]<0)
  names(df)[ncol(df)]=paste0(i,'_0.5')
  df[,ncol(df)+1] <- df[i]^2
  names(df)[ncol(df)] <- paste0(i,'_2')
  df[,ncol(df)+1] <- df[i]^3
  names(df)[ncol(df)] <- paste0(i,'_3')
}


all_temp_cols=colnames(df[,grepl("temp_", colnames(df))])
all_tempDistance_cols=colnames(df[,grepl("Distance", colnames(df))])
all_Temperature_cols=colnames(df[,grepl("Temperature", colnames(df))])
mean_encoding=colnames(df[,grepl("Mean_Encoding", colnames(df))])
all_dummy = c('holiday','Temp.51.7.','DayTime','NightTime') #'DayTime','NightTime'

for (i in mean_encoding){
  for (j in all_temp_cols){
    df[, ncol(df)+1] <- df[i]*df[j]
    names(df)[ncol(df)]=paste0(i,'.',j)
  }
}

for (i in mean_encoding){
  for (j in all_tempDistance_cols){
    df[, ncol(df)+1] <- df[i]*df[j]
    names(df)[ncol(df)]=paste0(i,'.',j)
  }
}

for (i in mean_encoding){
  for (j in all_Temperature_cols){
    df[, ncol(df)+1] <- df[i]*df[j]
    names(df)[ncol(df)]=paste0(i,'.',j)
  }
}

for (i in all_dummy){
  for (j in all_temp_cols){
    df[, ncol(df)+1] <- df[i]*df[j]
    names(df)[ncol(df)]=paste0(i,'.',j)
  }
}

for (i in all_dummy){
  for (j in all_tempDistance_cols){
    df[, ncol(df)+1] <- df[i]*df[j]
    names(df)[ncol(df)]=paste0(i,'.',j)
  }
}

for (i in all_dummy){
  for (j in all_Temperature_cols){
    df[, ncol(df)+1] <- df[i]*df[j]
    names(df)[ncol(df)]=paste0(i,'.',j)
  }
}

write.csv(df,'03_2010_variable_creation.csv')
