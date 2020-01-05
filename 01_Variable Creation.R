library(dplyr)
library(ggplot2)
library(readxl)
library(leaps)
library(MLmetrics)
library(forecast)
library(timeDate)
library(zoo)
library(lubridate)

df = read_xlsx('0_Data.xlsx')

df = df %>% mutate(trend = row_number())

df$Date=as.Date(df$Date)

df$month = months(df$Date)
df$weekday = weekdays(df$Date)

##Creating Dummies
{
  holidays = data.frame(holidayNYSE(year = 2008))
  holidays = rbind(holidays, data.frame(holidayNYSE(year = 2009)))
  holidays = rbind(holidays, data.frame(holidayNYSE(year = 2010)))
  holidays = rbind(holidays, data.frame(holidayNYSE(year = 2011)))
  holidays = rbind(holidays, data.frame(holidayNYSE(year = 2012)))
  colnames(holidays) = 'hol'
  holidays$hol = as.Date(holidays$hol)
  
  df$holiday = 0
  df$holiday[df$Date %in% holidays$hol] = 1
}

sum(df$holiday)

df$TempDistance = abs(df$Temperature - 51.7) #roundly see lowest load at temperature 51.7 
df['Temp>51.7?']=df$Temperature>51.7
colnames(df)
l = c(1,2,3,4,5,6,12,24,48)

for (i in l){
  df[, ncol(df)+1] = lag(df$Temperature, i)
  names(df)[ncol(df)]=paste0('Temperature_Lag',i)
  df[,ncol(df)+1] <- lag(df$Temperature, i)^2
  names(df)[ncol(df)] <- paste0("Temperature_Lag",i,"_2")
  df[,ncol(df)+1] <- lag(df$Temperature, i)^3
  names(df)[ncol(df)] <- paste0("Temperature_Lag",i,"_3")  
  df[,ncol(df)+1] <- lag(df$TempDistance, i)
  names(df)[ncol(df)] <- paste0("TempDistance_Lag",i)
  df[,ncol(df)+1] <- lag(df$TempDistance, i)^2
  names(df)[ncol(df)] <- paste0("TempDistance_Lag",i,"_2")
  df[,ncol(df)+1] <- lag(df$TempDistance, i)^0.5
  names(df)[ncol(df)] <- paste0("TempDistance_Lag",i,"_0.5")
}

df['Temperature_2'] = df$Temperature^2
df['Temperature_3'] = df$Temperature^3
df['Temperature_0.5'] = abs(df$Temperature)^0.5*(-1)^(df$Temperature<0)
df['TempDistance_2'] = df$TempDistance^2
df['TempDistance_3'] = df$TempDistance^3
df['TempDistance_0.5'] = abs(df$TempDistance)^0.5

df$day <-  day(df$Date)

df <- df %>%
  mutate(month.week = ifelse(day <=7, 1,
                             ifelse(day <= 14, 2,
                                    ifelse(day <= 21, 3,
                                           ifelse(day <= 28, 4, 5)))))


# create breaks
breaks <- c(0,5,12,17,22,24)
# labels for the breaks
labels <- c("Midnight", "Morning", "Afternoon", "Evening","Midnight")

df$Time_of_day <- cut(x=df$Hour, breaks = breaks, labels = labels, include.lowest=TRUE)

breaks2<- c(0,6,18,24)
labels2 <- c("Night Time", "Day Time", "Night Time")
df$Day_Night_Time <- cut(x=df$Hour, breaks = breaks2, labels = labels2, include.lowest=TRUE)
df$DayTime=0
df$DayTime[df$Day_Night_Time == 'Day Time'] = 1
df$NightTime=0
df$NightTime[df$Day_Night_Time == 'Night Time'] = 1

write.csv(df,'01_variable_creation.csv')

