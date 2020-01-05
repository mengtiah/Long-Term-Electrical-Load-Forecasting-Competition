df = read.csv('03_2010_df_625 Variables.csv')

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

colnames(df)

write.csv(df,'03_2010_v3_variable_creation.csv')

df = read.csv('03_2011_df_625 Variables.csv')

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

colnames(df)

write.csv(df,'03_2011_v3_variable_creation.csv')
