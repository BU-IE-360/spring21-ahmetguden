library(zoo)
library(tidyverse)
library(EVDS)
library(lubridate)
devtools::install_github("algopoly/EVDS")

set_evds_key("TX5OzOJpfZ")

df1 <- as.data.frame(get_series(series ="TP.AKONUTSAT2.TOPLAM", start_date = "01-03-2013",end_date ="01-03-2021"))
df2 <- df1[,2:3]
df2$items.TP_AKONUTSAT2_TOPLAM <- as.numeric(df2$items.TP_AKONUTSAT2_TOPLAM)

ggplot(df2,aes(x=items.Tarih, y=items.TP_AKONUTSAT2_TOPLAM)) +
  geom_point()
df2$items.Tarih <- as.Date(df2$items.Tarih, format = "%Y-%m")
df2$items.Tarih
