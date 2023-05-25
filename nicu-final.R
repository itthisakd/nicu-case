baby.df <- read.csv("~/Desktop/ucl/25DA2/NICU.csv") # load the NICU dataset

names(baby.df)
print(baby.df)

baby.df$Census <- with(baby.df, Admits*ALOS*12/365) # create the Census attribute

# install.packages(c("forecast"))

# load the package 
library(forecast)
# extract available census data from July 2008 to Feb 2013
census <- baby.df$Census[-(1:12)]
# create a time series object of Census
census.ts <- ts(census, start = c(2008, 7), end = c(2013, 2), 
                freq = 12)
alos.ts <- ts(baby.df$ALOS[-(1:12)], start = c(2008, 7), end = c(2013, 2), 
              freq = 12)
admits.ts <- ts(baby.df$Admits, start = c(2007, 7), end = c(2013, 2), 
              freq = 12)


plot(census.ts, ylab = "Census")

library(ggplot2)
autoplot(census.ts, ylab = "Census")

ggseasonplot(census.ts, ylab = "Census")

# Figure 4: seasonal plot of ALOS

ggseasonplot(alos.ts, ylab = "ALOS")

baby.df %>% ggplot(aes(x=Month, y=ALOS, group=Month)) + 
  geom_boxplot()+
  labs(title="Boxplot of ALOS, by Month")+
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun",
                                           "Jul","Aug","Sep","Oct","Nov","Dec"))
# Figure 5: seasonal plot of Admits

ggseasonplot(admits.ts, ylab = "Admits")

baby.df %>% ggplot(aes(x=Month, y=Admits, group=Month)) + 
  geom_boxplot()+
  labs(title="Boxplot of Admits, by Month")+
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))

library(zoo)
# calculate the 3-month centered moving average
census.ma_c <- rollmean(census.ts, 3, align = 'center')
plot(census.ts, ylab = "Census", col = 'blue') # plot the original time series
lines(census.ma_c, col = 'red') # add the centered moving average to the plot

# calculate the trailing moving averages
census.ma_l <- rollmean(census.ts, 3, align = 'right')
plot(census.ts, ylab = "Census", col = 'blue') # plot the original time series
lines(census.ma_l, col = 'red') # add the trailing moving averages to the plot

# create the training dataset 
train.ts <- window(census.ts, start = c(2008, 7), end = c(2012, 8))
# create the testing dataset
test.ts <- window(census.ts, start = c(2012, 9), end = c(2013, 2))

census.ets.ANN <- ets(train.ts, model = "ANN") # generate a simple exponential smoother
print(census.ets.ANN) # print the generated model

plot(census.ets.ANN)

census.ets.AAN <- ets(train.ts, model = "AAN") # generate a Holt's linear trend model
print(census.ets.AAN) # print the generated model

plot(census.ets.AAN)

census.ets.AAA <- ets(train.ts, model = "AAA") # generate the Holt-Winters seasonality model
print(census.ets.AAA) # print the generated model

plot(census.ets.AAA)


#---------------- Evaluate & Predict ------------------------------

h = length(test.ts)

# make prediction from the simple exponential smoother
census.ets.ANN.pred <- forecast(census.ets.ANN, h = h)
# make prediction from the Holt's linear trend model
census.ets.AAN.pred <- forecast(census.ets.AAN, h = h)
# make prediction from Holt-Winters seasonality model
census.ets.AAA.pred <- forecast(census.ets.AAA, h = h)


accuracy(census.ets.ANN.pred, test.ts)

accuracy(census.ets.AAN.pred, test.ts)

accuracy(census.ets.AAA.pred, test.ts)

# ------------------- Predictions ------------------------------

# refit the Holt's linear trend model to the whole dataset
model.best <- ets(census.ts, mode = 'AAN')
# forecast 'census' 17 months ahead (to July 2014).
census.forecast <- forecast(model.best, h = 17, level=0.8)
# find the mean predicted census in 17 months
mean_census <- round(forecast$mean[17], 1)
mean_census

census.forecast

model.best

# Figure 11
# plot the predictions of census in the next 17 months
plot(census.forecast, ylab = "Census")
# add reference lines showing the current and planned census
abline(h = 45, col = "red")
text(2014, 46, "Current Beds (45 beds)", cex = 1)
abline(h = 53, col = "red")
text(2014, 54, "Planned Beds (53 beds)", cex = 1)
