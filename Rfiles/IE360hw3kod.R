library(tidyverse)
library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)
library(zoo)
library(urca)


setwd("/Users/ahmetguden/Desktop/GitHub/spring21-ahmetguden/data")
df <- as.data.table(read_csv("epias2.csv"))

df$Date <- as.Date(df$Date, format = "%d.%m.%Y")

df$DateTime <- paste(df$Date,df$Time)
df$DateTime <- as.POSIXct(df$DateTime,format="%Y-%m-%d %H:%M" ,tz="UTC")
df <- df[,4:3]
df$Consumption <- df$Consumption*1000
head(df)

df[2067,2] <- 28413.3
df[2067,]
df[which.min(Consumption)]
df[DateTime=="2016-03-27 02:00:00" , Consumption ]
df[DateTime=="2016-03-27 02:00:00" , Consumption:= 28413.3]

df[DateTime=="2016-03-27 02:00:00 "]

ggplot(df, aes(x=DateTime , y=Consumption)) +
  geom_line() +
  labs(title = "Electricity Consumption", x="Date" )

summary(ur.kpss(df$Consumption))

#hourly decompose
hourly_ts <- ts(df$Consumption,frequency = 24)
hourly_dec <- decompose(hourly_ts,type ="additive")
plot(hourly_dec)
plot(hourly_dec$random)


#daily decompose

daily_ts <- ts(df$Consumption,frequency = 24*7)
daily_dec <- decompose(daily_ts,type ="additive")
plot(daily_dec)
plot(daily_dec$random)

##monthly decompse

monthly_ts <- ts(df$Consumption,frequency = (24*7*52))
monthly_dec <- decompose(monthly_ts,type="additive")
plot(monthly_dec)
plot(monthly_dec$random)

df[,monthly:=as.numeric(monthly_dec$seasonal)]
df[,daily:=as.numeric(daily_dec$seasonal)]
df[,hourly:=as.numeric(hourly_dec$seasonal)]

ggplot(df[1:24*2], aes(x=DateTime, y=hourly)) +
  geom_line() +
  geom_point() +
  labs(title="Hourly Seasonality" , x="Date", y="Hourly Effect" )



ggplot(df[1:(168*2)], aes(x=DateTime, y=daily)) +
  geom_line() +
  geom_point() +
  labs(title="Daily Seasonality" , x="Date", y="Daily Effect" )

ggplot(df[1:(8736*2)], aes(x=DateTime, y=monthly)) +
  geom_line() +
  geom_point() +
  labs(title="Monthly Seasonality" , x="Date", y="Monthly Effect" )


#168


plot(daily_dec$random)

df <- df[,Seasonality:=as.numeric(daily_dec$seasonal)]
df <- df[,Trend:=as.numeric(daily_dec$trend)]
df <- df[,Random:=as.numeric(daily_dec$random)]

ggplot(df, aes(x=DateTime, y=Trend)) +
  geom_line() +
  labs(title="Trend" , x="Date", y="Trend Effect" )

summary(ur.kpss(daily_dec$random))


ggAcf(daily_dec$random, lag.max = 168) + 
  labs(title = "ACF of the random component")


ggPacf(daily_dec$random, lag.max = 168) +
  labs(title = "PACF of the random component")

##AAAARRRRR
ar1 <-  arima(df[,Random], order = c(1,0,0))
ar2 <-  arima(df[,Random], order = c(2,0,0))
ar3 <-  arima(df[,Random], order = c(3,0,0))
ar4 <-  arima(df[,Random], order = c(4,0,0))
ar5 <-  arima(df[,Random], order = c(5,0,0))

AIC_ar <- c(ar1=AIC(ar1), ar2=AIC(ar2) , ar3=AIC(ar3), ar4=AIC(ar4) , ar5= AIC(ar5))
which.min(AIC_ar)

####MMMMMAAAAAA

ma1 <-  arima(df[,Random], order = c(0,0,1))
ma2 <-  arima(df[,Random], order = c(0,0,2))
ma3 <-  arima(df[,Random], order = c(0,0,3))
ma4 <-  arima(df[,Random], order = c(0,0,4))
ma5 <-  arima(df[,Random], order = c(0,0,5))

AIC_ma <- c(ma1=AIC(ma1),ma2=AIC(ma2), ma3=AIC(ma3) , ma4=AIC(ma4),ma5=AIC(ma5))
which.min(AIC_ma)

#AARRMMAAAAAA

arma1 <-arima(df[,Random], order = c(5,0,5))
AIC(arma1)







arma2 <- arima(df[,Random], order = c(4,0,4))
AIC(arma2)




summary(arma2)
arma2$coef
arma2$residuals



df[,Residuals:=residuals(arma2)]
df[,fitted:=Random - Residuals]
df[,fitted:=as.numeric(fitted) +as.numeric(Trend) + as.numeric(Seasonality)]


ggplot(df, aes(x=DateTime)) +
  geom_line(aes(y=Consumption, col="actual")) +
  geom_line(aes(y=fitted, col="fitted")) +
  labs(title = "Fitted and actual values of consumption over time" , x="Date" ) 


ggplot(df[DateTime>="2016-03-19 14:00:00 " & DateTime<="2016-03-29 14:00:00"], aes(x=DateTime)) +
  geom_line(aes(y=Consumption, col="actual")) +
  geom_line(aes(y=fitted, col="fitted")) +
  labs(title = "Fitted and actual values of consumption over time" , x="Date" )


## Forecast

last_trend <- tail(daily_dec$trend[!is.na(daily_dec$trend)],1)
seasonal_effect <- daily_dec$seasonal[46765:47208]
model_forecast <- predict(arma2, n.ahead = (15*24) + 84)$pred
model_forecast <- model_forecast + last_trend + seasonal_effect

forecasted <- tail(model_forecast, (15*24))
actual <- tail(df$Consumption, (15*24))


dfdf <- data.frame(forecasts=forecasted,actuals=actual)
forecast_dates <-as.vector(df[46849:47208,1])
dfdf <- cbind(dfdf,forecast_dates)

ggplot(dfdf, aes(x=DateTime)) +
  geom_line(aes(y=actuals , col="actual")) +
  geom_line(aes(y=forecasts, col="forecasts")) +
  labs(title="Actual values vs Forecasts" , x="Date" , y="Consumption")

accu=function(actual,forecast) {
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  bias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  WMAPE=MAD/mean
  metrics=data.frame(n,mean,sd,CV,bias,MAPE,WMAPE)
  return(metrics) }

accu <- accu(actual,forecasted)
accu

dfdf <- as.data.table(dfdf)
dfdf <- dfdf[,error:=actuals-forecasts]
dfdf <- dfdf[,APE:= abs(error/actuals)]
dfdf <- dfdf[,bias:= (error/actuals)]

metrics <- dfdf[ , .(DailyMAPE = sum(APE) /24 , DailyBias = sum(bias) /24) , by=.(Date=as.Date(DateTime))] 
metrics
