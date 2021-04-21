

library(zoo)
library(tidyverse)
library(EVDS)
library(lubridate)
install.packages("devtools")
devtools::install_github("algopoly/EVDS")

set_evds_key("TX5OzOJpfZ")
dolar_import <- as.data.frame(get_series(series= c("TP.DK.USD.A.YTL"), start_date="15-01-2018" , end_date="15-01-2021"))
dolar_evds<- na.omit(dolar_import[,2:3])

colnames(dolar_evds) <-  c("Date" , "ExchangeRate")
dolar_evds$Date <-as.Date(dolar_evds$Date, format= "%d-%m-%Y")
dolar_evds$ExchangeRate <- as.numeric(dolar_evds$ExchangeRate)
head(dolar_evds)

ggplot(dolar_evds, aes(Date,ExchangeRate)) +
  geom_line() + 
  labs(title="TL per USD over time" ,x="Date",y = "TL per USD") +
  scale_x_date(date_breaks = "2 months", limits = as.Date(c("2018-02-01", "2021-01-01"))) +
  theme(axis.text.x = element_text(angle = 90))

setwd("/Users/ahmetguden/Desktop/GitHub/spring21-ahmetguden/data")
dolar_trends <- na.omit(as.data.frame(read_csv("dolarkuru.csv")))

colnames(dolar_trends) <- c("Date" , "SearchVolume")
dolar_trends$Date <-  as.Date(dolar_trends$Date, format = "%m/%d/%y")
dolar_trends$SearchVolume <- as.numeric(dolar_trends$SearchVolume)
head(dolar_trends)
ggplot(dolar_trends, aes(Date,SearchVolume)) +
  geom_line() + 
  labs(title="Search Volume of 'Dolar Kuru' " ,x="Date",y = "Search Volume") +
  scale_x_date(date_breaks = "2 months",limits = as.Date(c("2018-02-01", "2021-01-01"))) +
  theme(axis.text.x = element_text(angle = 90))


set_evds_key("TX5OzOJpfZ")
cpi_import <- as.data.frame(get_series(series= c("TP.FG.J0"), start_date="15-01-2018" , end_date="15-01-2021"))

cpi <- cpi_import[,2:3]
titles_cpi <- c("Date" , "CPI")
colnames(cpi) <-  titles_cpi
char_dates  <- c("2018-01-15",  "2018-02-15" , "2018-03-15"  ,"2018-04-15"  ,"2018-05-15",  "2018-06-15" , "2018-07-15",  "2018-08-15"  ,"2018-09-15" ,
                 "2018-10-15", "2018-11-15", "2018-12-15" ,"2019-01-15" , "2019-02-15"  ,"2019-03-15"  ,"2019-04-15"  ,"2019-05-15"  ,"2019-06-15" ,
                 "2019-07-15" , "2019-08-15" , "2019-09-15",  "2019-10-15" ,"2019-11-15" ,"2019-12-15" ,"2020-01-15"  ,"2020-02-15"  ,"2020-03-15" ,
                 "2020-04-15"  ,"2020-05-15"  ,"2020-06-15"  ,"2020-07-15"  ,"2020-08-15"  ,"2020-09-15",  "2020-10-15" ,"2020-11-15" ,"2020-12-15",
                 "2021-01-15" )
dates <- as.Date(char_dates, format = "%Y-%m-%d")

cpi$Date <- dates
cpi$CPI  <- as.numeric(cpi$CPI)

ggplot(cpi, aes(Date,CPI)) +
  geom_point() +
  geom_line() +
  labs(title = "Consumer Price Index over Time" , x="Date" ,y="CPI") +
  scale_x_date(date_breaks = "2 months",  limits = as.Date(c("2018-02-01", "2021-01-01"))) +
  theme(axis.text.x = element_text(angle = 90))

enflasyon <- as.data.frame(read_csv("enflasyon.csv")) 

enflasyon$Date <- as.Date(enflasyon$Date, format = "%m/%d/%y")
enflasyon$SearchVolume <- as.numeric(enflasyon$SearchVolume)

ggplot(enflasyon, aes(Date,SearchVolume)) +
  geom_line() +
  scale_x_date(date_breaks = "2 months", limits = as.Date(c("2018-02-01", "2021-01-01"))) +
  theme(axis.text.x = element_text(angle = 90))

set_evds_key("TX5OzOJpfZ")
unemp_import <- as.data.frame(get_series(series= c("TP.YISGUCU2.G8" ), start_date="15-01-2018" , end_date="15-01-2021"))
unemp_rate_evds <- unemp_import[,2:3]

colnames(unemp_rate_evds) <- c("Date" , "UnemployementRate")

unemp_rate_evds$Date <- dates

unemp_rate_evds$UnemployementRate <- as.numeric(unemp_rate_evds$UnemployementRate)

ggplot(unemp_rate_evds, aes(Date,UnemployementRate)) +
  geom_point() +
  geom_line() +
  labs(title = "Unemployment Rate over Time", x="Date" , y="Unemployment Rate") +
  scale_x_date(date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 90))

unemp_rate_trends <- as.data.frame(read_csv("unemprate.csv"))

unemp_rate_trends$Date <- as.Date(unemp_rate_trends$Date,format = "%m/%d/%y")
unemp_rate_trends$SearchVolume <- as.numeric(unemp_rate_trends$SearchVolume)

ggplot(unemp_rate_trends, aes(Date,SearchVolume)) +
  geom_line() +
  labs(title = "Search Volume of 'İşsizlik Oranı'" , x= "Date" , y= "Search Volume") +
  scale_x_date(date_breaks = "2 months", limits = as.Date(c("2018-02-01", "2021-01-01"))) +
  theme(axis.text.x = element_text(angle = 90))





