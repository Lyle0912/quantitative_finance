setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library("dplyr")
library("lubridate")
library("readxl")
library("openxlsx")
library("zoo")
library("tidyr")

# exchange rates
usd_cad <- read.csv("exchange_rates/dollar_canadian.csv", col.names = c("date", "usd_cad"), dec = ".", colClasses = c("Date", "numeric"), na.strings = ".")
usd_eur <- read.csv("exchange_rates/dollar_eur.csv", col.names = c("date", "usd_eur"), dec = ".", colClasses = c("Date", "numeric"), na.strings = ".")
usd_gbp <- read.csv("exchange_rates/dollar_pound.csv", col.names = c("date", "usd_gbp"), dec = ".", colClasses = c("Date", "numeric"), na.strings = ".")
#usd_jpy <- read.csv("exchange_rates/dollar_yen.csv", col.names = c("date", "usd_yen"), dec = ".", colClasses = c("Date", "numeric"), na.strings = ".")


exchange_rates <- data.frame(date = seq(as.Date("2017-12-01"), as.Date("2023-08-10"), by="days")) %>%
  left_join(usd_cad %>% mutate(date = ymd(date)), by = join_by(date)) %>%       
  left_join(usd_eur %>% mutate(date = ymd(date)), by = join_by(date)) %>%
  left_join(usd_gbp %>% mutate(date = ymd(date)), by = join_by(date)) #%>%
#  left_join(usd_jpy %>% mutate(date = ymd(date)), by = join_by(date))


# stocks
ftse100 <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = mdy(Date)) %>%
  mutate(ftse100 = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100"))

#nikkei225 <- read.csv("stocks/nikkei225.csv") %>%
#  mutate(date = ymd(Date)) %>%
#  mutate(nikkei225 = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225"))

sp500 <- read.csv("stocks/sp500.csv") %>%
  mutate(date = mdy(Datum)) %>%
  mutate(sp500 = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500"))

sptsx <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = mdy(Date)) %>%
  mutate(sptsx = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx"))

stoxx600 <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = dmy(Datum)) %>%
  mutate(stoxx600 = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100, by = join_by(date)) %>%
#  left_join(nikkei225, by = join_by(date)) %>%
  left_join(sp500, by = join_by(date)) %>%
  left_join(sptsx, by = join_by(date)) %>%
  left_join(stoxx600, by = join_by(date))


# cpi
cpi_ca <- read.csv("cpi/cpi_can.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(cpi_ca = as.numeric(CPALCY01CAM661N)) %>%
  dplyr::select(c("date", "cpi_ca"))

cpi_eu <- read.csv("cpi/cpi_eu.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(cpi_eu = as.numeric(CP0000EU272020M086NEST)) %>%
  dplyr::select(c("date", "cpi_eu"))

#cpi_jp <- read.csv("cpi/cpi_jpn.csv") %>%
#  mutate(date = ymd(DATE)) %>%
#  mutate(cpi_jp = as.numeric(JPNCPIALLMINMEI)) %>%
#  dplyr::select(c("date", "cpi_jp"))

cpi_uk <- read.csv("cpi/cpi_uk.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(cpi_uk = as.numeric(GBRCPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_uk"))

cpi_us <- read.csv("cpi/cpi_us.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(cpi_us = as.numeric(USACPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_us"))

exchange_rates <- exchange_rates %>%
  left_join(cpi_ca, by = join_by(date)) %>%
  left_join(cpi_eu, by = join_by(date)) %>%
#  left_join(cpi_jp, by = join_by(date)) %>%
  left_join(cpi_uk, by = join_by(date)) %>%
  left_join(cpi_us, by = join_by(date))


# gdp

gdp_ca <- read.csv("gdp/gdp_can.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_ca = as.numeric(NAEXKP01CAQ189S)) %>%
  dplyr::select(c("date", "gdp_ca"))

gdp_eu <- read.csv("gdp/gdp_eu.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_eu = as.numeric(CPMNACSCAB1GQEU272020)) %>%
  dplyr::select(c("date", "gdp_eu"))

#gdp_jp <- read.csv("gdp/gdp_jpn.csv") %>%
#  mutate(date = ymd(DATE)) %>%
#  mutate(gdp_jp = as.numeric(JPNNGDP)) %>%
#  dplyr::select(c("date", "gdp_jp"))

gdp_uk <- read.csv("gdp/gdp_uk.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_uk = as.numeric(UKNGDP)) %>%
  dplyr::select(c("date", "gdp_uk"))

gdp_us <- read.csv("gdp/gdp_us.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_us = as.numeric(GDP)) %>%
  dplyr::select(c("date", "gdp_us"))

exchange_rates <- exchange_rates %>%
  left_join(gdp_ca, by = join_by(date)) %>%
  left_join(gdp_eu, by = join_by(date)) %>%
#  left_join(gdp_jp, by = join_by(date)) %>%
  left_join(gdp_uk, by = join_by(date)) %>%
  left_join(gdp_us, by = join_by(date))


# gdp index

gdp_ca_index <- read.csv("gdp/gdp_can_index.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_ca_index = as.numeric(NAEXKP01CAQ189S_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_ca_index"))

gdp_eu_index <- read.csv("gdp/gdp_eu_index.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_eu_index = as.numeric(CPMNACSCAB1GQEU272020_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_eu_index"))

#gdp_jp_index <- read.csv("gdp/gdp_jpn_index.csv") %>%
#  mutate(date = ymd(DATE)) %>%
#  mutate(gdp_jp_index = as.numeric(JPNNGDP_NBD20180101)) %>%
#  dplyr::select(c("date", "gdp_jp_index"))

gdp_uk_index <- read.csv("gdp/gdp_uk_index.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_uk_index = as.numeric(UKNGDP_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_uk_index"))

gdp_us_index <- read.csv("gdp/gdp_us_index.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(gdp_us_index = as.numeric(GDP_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_us_index"))

exchange_rates <- exchange_rates %>%
  left_join(gdp_ca_index, by = join_by(date)) %>%
  left_join(gdp_eu_index, by = join_by(date)) %>%
#  left_join(gdp_jp_index, by = join_by(date)) %>%
  left_join(gdp_uk_index, by = join_by(date)) %>%
  left_join(gdp_us_index, by = join_by(date))


# policy rates

polrate_ca <- read.csv("interest_rates/ffr_can.csv", skip = 11) %>%
  mutate(date = ymd(Date)) %>%
  mutate(polrate_ca = as.numeric(V39079)) %>%
  dplyr::select(c("date", "polrate_ca"))

polrate_eu <- read.csv("interest_rates/ffr_eu.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(polrate_eu = as.numeric(ECBMRRFR)) %>%
  dplyr::select(c("date", "polrate_eu"))

polrate_uk <- read.csv("interest_rates/ffr_uk.csv") %>%
  mutate(date = dmy(Date)) %>%
  mutate(polrate_uk = as.numeric(Bank.Rate)) %>%
  dplyr::select(c("date", "polrate_uk"))

polrate_us_lower <- read.csv("interest_rates/ffr_us_lower.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(polrate_us_lower = as.numeric(DFEDTARL)) %>%
  dplyr::select(c("date", "polrate_us_lower"))

polrate_us_upper <- read.csv("interest_rates/ffr_us_upper.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(polrate_us_upper = as.numeric(DFEDTARU)) %>%
  dplyr::select(c("date", "polrate_us_upper"))

exchange_rates <- exchange_rates %>%
  left_join(polrate_ca, by = join_by(date)) %>%
  fill(polrate_ca, .direction = "down") %>%
  left_join(polrate_eu, by = join_by(date)) %>%
  fill(polrate_eu, .direction = "down") %>%
  left_join(polrate_uk, by = join_by(date)) %>%
  fill(polrate_uk, .direction = "down") %>%
  left_join(polrate_us_lower, by = join_by(date)) %>%
  fill(polrate_us_lower, .direction = "down") %>%
  left_join(polrate_us_upper, by = join_by(date)) %>%
  fill(polrate_us_upper, .direction = "down")

#exchange_rates$polrate_jp <- rep(-0.1, 2079) 



# m1
m1_ca <- read.csv("m1/m1_can.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(m1_ca = as.numeric(MANMM101CAM189S)) %>%
  dplyr::select(c("date", "m1_ca"))

m1_eu <- read.csv("m1/m1_eu.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(m1_eu = as.numeric(MANMM101EZM189S)) %>%
  dplyr::select(c("date", "m1_eu"))

#m1_jp <- read.csv("m1/m1_jpn.csv") %>%
#  mutate(date = ymd(DATE)) %>%
#  mutate(m1_jp = as.numeric(MANMM101JPM189S)) %>%
#  dplyr::select(c("date", "m1_jp"))

m1_uk <- read.csv("m1/m1_uk.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(m1_uk = as.numeric(MANMM101GBM189S)) %>%
  dplyr::select(c("date", "m1_uk"))

m1_us <- read.csv("m1/m1_us.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(m1_us = as.numeric(MANMM101USM189S)) %>%
  dplyr::select(c("date", "m1_us"))

exchange_rates <- exchange_rates %>%
  left_join(m1_ca, by = join_by(date)) %>%
  left_join(m1_eu, by = join_by(date)) %>%
#  left_join(m1_jp, by = join_by(date)) %>%
  left_join(m1_uk, by = join_by(date)) %>%
  left_join(m1_us, by = join_by(date))


# m2
m2_ca <- read.csv("m2/m2_can.csv", skip = 51) %>%
  mutate(date = ymd(date)) %>%
  mutate(m2_ca = as.numeric(V41552786_E1)) %>%
  dplyr::select(c("date", "m2_ca"))

m2_eu <- read.csv("m2/m2_eu.csv") %>%
  mutate(date = ymd(date)) %>%
  mutate(m2_eu = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_eu"))

#m2_jp <- read.csv("m2/m2_jpn.csv") %>%
#  mutate(date = ymd(date)) %>%
#  mutate(m2_jp = as.numeric(s1)) %>%
#  dplyr::select(c("date", "m2_jp"))

m2_uk <- read.csv("m2/m2_uk.csv") %>%
  mutate(date = dmy(Date)) %>%
  mutate(m2_uk = as.numeric(LPMVQXV)) %>%
  dplyr::select(c("date", "m2_uk"))

m2_us <- read.csv("m2/m2_us.csv") %>%
  mutate(date = ymd(date)) %>%
  mutate(m2_us = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_us"))

exchange_rates <- exchange_rates %>%
  left_join(m2_ca, by = join_by(date)) %>%
  left_join(m2_eu, by = join_by(date)) %>%
#  left_join(m2_jp, by = join_by(date)) %>%
  left_join(m2_uk, by = join_by(date)) %>%
  left_join(m2_us, by = join_by(date))

# uncertainty
uncertainty_ca <- read.csv("uncertainty/uncertainty_can.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(uncertainty_ca = as.numeric(CANEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_ca"))

uncertainty_eu <- read.csv("uncertainty/uncertainty_eu.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(uncertainty_eu = as.numeric(EUEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_eu"))

#uncertainty_jp <- read_excel("uncertainty/uncertainty_jpn.xlsx") %>%
#  mutate(date = ymd(paste(Year, "-", Month, "-01"))) %>%
#  mutate(uncertainty_jp = as.numeric(`News-based Economic Policy Uncertainty Index`)) %>%
#  dplyr::select(c("date", "uncertainty_jp"))

uncertainty_uk <- read.csv("uncertainty/uncertainty_uk.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(uncertainty_uk = as.numeric(UKEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_uk"))

uncertainty_us <- read.csv("uncertainty/uncertainty_us.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(uncertainty_us = as.numeric(USEPUINDXD)) %>%
  dplyr::select(c("date", "uncertainty_us"))

exchange_rates <- exchange_rates %>%
  left_join(uncertainty_ca, by = join_by(date)) %>%
  left_join(uncertainty_eu, by = join_by(date)) %>%
#  left_join(uncertainty_jp, by = join_by(date)) %>%
  left_join(uncertainty_uk, by = join_by(date)) %>%
  left_join(uncertainty_us, by = join_by(date))


# unemployment
unrate_ca <- read.csv("unemployment/unemployment_can.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(unrate_ca = as.numeric(LRHUTTTTCAM156S)) %>%
  dplyr::select(c("date", "unrate_ca"))

unrate_eu <- read_excel("unemployment/unrate_eu2.xlsx") %>%
  mutate(date = ymd(paste(TIME, "-01"))) %>%
  mutate(unrate_eu = as.numeric(unrate)) %>%
  dplyr::select(c("date", "unrate_eu"))

#unrate_jp <- read.csv("unemployment/unemployment_jpn.csv") %>%
#  mutate(date = ymd(DATE)) %>%
#  mutate(unrate_jp = as.numeric(LRHUTTTTJPM156S)) %>%
#  dplyr::select(c("date", "unrate_jp"))

unrate_uk <- read.csv("unemployment/unemployment_uk.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(unrate_uk = as.numeric(LRHUTTTTGBM156S)) %>%
  dplyr::select(c("date", "unrate_uk"))

unrate_us <- read.csv("unemployment/unemployment_us.csv") %>%
  mutate(date = ymd(DATE)) %>%
  mutate(unrate_us = as.numeric(LRHUTTTTUSM156S)) %>%
  dplyr::select(c("date", "unrate_us"))

exchange_rates <- exchange_rates %>%
  left_join(unrate_ca, by = join_by(date)) %>%
  left_join(unrate_eu, by = join_by(date)) %>%
#  left_join(unrate_jp, by = join_by(date)) %>%
  left_join(unrate_uk, by = join_by(date)) %>%
  left_join(unrate_us, by = join_by(date))



# lagged monthly data

cpi_reporting_lag <- 14
m1_reporting_lag <- 14
m2_reporting_lag <- 14
uncertainty_reporting_lag <- 14
unrate_reporting_lag <- 14

# cpi
cpi_ca_lag1m <- read.csv("cpi/cpi_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_ca_lag1m = as.numeric(CPALCY01CAM661N)) %>%
  dplyr::select(c("date", "cpi_ca_lag1m"))

cpi_eu_lag1m <- read.csv("cpi/cpi_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_eu_lag1m = as.numeric(CP0000EU272020M086NEST)) %>%
  dplyr::select(c("date", "cpi_eu_lag1m"))

#cpi_jp_lag1m <- read.csv("cpi/cpi_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(1) %m+% days(cpi_reporting_lag)) %>%
#  mutate(cpi_jp_lag1m = as.numeric(JPNCPIALLMINMEI)) %>%
#  dplyr::select(c("date", "cpi_jp_lag1m"))

cpi_uk_lag1m <- read.csv("cpi/cpi_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_uk_lag1m = as.numeric(GBRCPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_uk_lag1m"))

cpi_us_lag1m <- read.csv("cpi/cpi_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_us_lag1m = as.numeric(USACPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_us_lag1m"))

exchange_rates <- exchange_rates %>%
  left_join(cpi_ca_lag1m, by = join_by(date)) %>%
  fill(cpi_ca_lag1m, .direction = "down") %>%
  left_join(cpi_eu_lag1m, by = join_by(date)) %>%
  fill(cpi_eu_lag1m, .direction = "down") %>%
#  left_join(cpi_jp_lag1m, by = join_by(date)) %>%
#  fill(cpi_jp_lag1m, .direction = "down") %>%
  left_join(cpi_uk_lag1m, by = join_by(date)) %>%
  fill(cpi_uk_lag1m, .direction = "down") %>%
  left_join(cpi_us_lag1m, by = join_by(date)) %>%
  fill(cpi_us_lag1m, .direction = "down")



# m1
m1_ca_lag1m <- read.csv("m1/m1_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_ca_lag1m = as.numeric(MANMM101CAM189S)) %>%
  dplyr::select(c("date", "m1_ca_lag1m"))

m1_eu_lag1m <- read.csv("m1/m1_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_eu_lag1m = as.numeric(MANMM101EZM189S)) %>%
  dplyr::select(c("date", "m1_eu_lag1m"))

#m1_jp_lag1m <- read.csv("m1/m1_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(1) %m+% days(m1_reporting_lag)) %>%
#  mutate(m1_jp_lag1m = as.numeric(MANMM101JPM189S)) %>%
#  dplyr::select(c("date", "m1_jp_lag1m"))

m1_uk_lag1m <- read.csv("m1/m1_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_uk_lag1m = as.numeric(MANMM101GBM189S)) %>%
  dplyr::select(c("date", "m1_uk_lag1m"))

m1_us_lag1m <- read.csv("m1/m1_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_us_lag1m = as.numeric(MANMM101USM189S)) %>%
  dplyr::select(c("date", "m1_us_lag1m"))

exchange_rates <- exchange_rates %>%
  left_join(m1_ca_lag1m, by = join_by(date)) %>%
  fill(m1_ca_lag1m, .direction = "down") %>%
  left_join(m1_eu_lag1m, by = join_by(date)) %>%
  fill(m1_eu_lag1m, .direction = "down") %>%
#  left_join(m1_jp_lag1m, by = join_by(date)) %>%
#  fill(m1_jp_lag1m, .direction = "down") %>%
  left_join(m1_uk_lag1m, by = join_by(date)) %>%
  fill(m1_uk_lag1m, .direction = "down") %>%
  left_join(m1_us_lag1m, by = join_by(date)) %>%
  fill(m1_us_lag1m, .direction = "down")

# m2
m2_ca_lag1m <- read.csv("m2/m2_can.csv", skip = 51) %>%
  mutate(date = ymd(date) %m+% months(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_ca_lag1m = as.numeric(V41552786_E1)) %>%
  dplyr::select(c("date", "m2_ca_lag1m"))

m2_eu_lag1m <- read.csv("m2/m2_eu.csv") %>%
  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_eu_lag1m = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_eu_lag1m"))

#m2_jp_lag1m <- read.csv("m2/m2_jpn.csv") %>%
#  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
#  mutate(m2_jp_lag1m = as.numeric(s1)) %>%
#  dplyr::select(c("date", "m2_jp_lag1m"))

m2_uk_lag1m <- read.csv("m2/m2_uk.csv") %>%
  mutate(date = dmy(Date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_uk_lag1m = as.numeric(LPMVQXV)) %>%
  dplyr::select(c("date", "m2_uk_lag1m"))

m2_us_lag1m <- read.csv("m2/m2_us.csv") %>%
  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_us_lag1m = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_us_lag1m"))

exchange_rates <- exchange_rates %>%
  left_join(m2_ca_lag1m, by = join_by(date)) %>%
  fill(m2_ca_lag1m, .direction = "down") %>%
  left_join(m2_eu_lag1m, by = join_by(date)) %>%
  fill(m2_eu_lag1m, .direction = "down") %>%
#  left_join(m2_jp_lag1m, by = join_by(date)) %>%
#  fill(m2_jp_lag1m, .direction = "down") %>%
  left_join(m2_uk_lag1m, by = join_by(date)) %>%
  fill(m2_uk_lag1m, .direction = "down") %>%
  left_join(m2_us_lag1m, by = join_by(date)) %>%
  fill(m2_us_lag1m, .direction = "down")


# uncertainty
uncertainty_ca_lag1m <- read.csv("uncertainty/uncertainty_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_ca_lag1m = as.numeric(CANEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_ca_lag1m"))

uncertainty_eu_lag1m <- read.csv("uncertainty/uncertainty_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_eu_lag1m = as.numeric(EUEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_eu_lag1m"))

#uncertaainty_jp_lag1m <- read_excel("uncertainty/uncertainty_jpn.xlsx") %>%
#  mutate(date = ymd(paste(Year, "-", Month, "-01")) %m+% months(1) %m+% days(uncertainty_reporting_lag)) %>%
#  mutate(uncertaainty_jp_lag1m = as.numeric(`News-based Economic Policy Uncertainty Index`)) %>%
#  dplyr::select(c("date", "uncertaainty_jp_lag1m"))

uncertainty_uk_lag1m <- read.csv("uncertainty/uncertainty_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_uk_lag1m = as.numeric(UKEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_uk_lag1m"))

uncertainty_us_lag1m <- read.csv("uncertainty/uncertainty_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_us_lag1m = as.numeric(USEPUINDXD)) %>%
  dplyr::select(c("date", "uncertainty_us_lag1m"))

exchange_rates <- exchange_rates %>%
  left_join(uncertainty_ca_lag1m, by = join_by(date)) %>%
  fill(uncertainty_ca_lag1m, .direction = "down") %>%
  left_join(uncertainty_eu_lag1m, by = join_by(date)) %>%
  fill(uncertainty_eu_lag1m, .direction = "down") %>%
#  left_join(uncertaainty_jp_lag1m, by = join_by(date)) %>%
#  fill(uncertaainty_jp_lag1m, .direction = "down") %>%
  left_join(uncertainty_uk_lag1m, by = join_by(date)) %>%
  fill(uncertainty_uk_lag1m, .direction = "down") %>%
  left_join(uncertainty_us_lag1m, by = join_by(date)) %>%
  fill(uncertainty_us_lag1m, .direction = "down")

# unemployment
unrate_ca_lag1m <- read.csv("unemployment/unemployment_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_ca_lag1m = as.numeric(LRHUTTTTCAM156S)) %>%
  dplyr::select(c("date", "unrate_ca_lag1m"))

unrate_eu_lag1m <- read_excel("unemployment/unrate_eu2.xlsx") %>%
  mutate(date = ymd(paste(TIME, "-01")) %m+% months(1) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_eu_lag1m = as.numeric(unrate)) %>%
  dplyr::select(c("date", "unrate_eu_lag1m"))

#unrate_jp_lag1m <- read.csv("unemployment/unemployment_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(1) %m+% days(unrate_reporting_lag)) %>%
#  mutate(unrate_jp_lag1m = as.numeric(LRHUTTTTJPM156S)) %>%
#  dplyr::select(c("date", "unrate_jp_lag1m"))

unrate_uk_lag1m <- read.csv("unemployment/unemployment_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_uk_lag1m = as.numeric(LRHUTTTTGBM156S)) %>%
  dplyr::select(c("date", "unrate_uk_lag1m"))

unrate_us_lag1m <- read.csv("unemployment/unemployment_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(1) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_us_lag1m = as.numeric(LRHUTTTTUSM156S)) %>%
  dplyr::select(c("date", "unrate_us_lag1m"))

exchange_rates <- exchange_rates %>%
  left_join(unrate_ca_lag1m, by = join_by(date)) %>%
  fill(unrate_ca_lag1m, .direction = "down") %>%
  left_join(unrate_eu_lag1m, by = join_by(date)) %>%
  fill(unrate_eu_lag1m, .direction = "down") %>%
#  left_join(unrate_jp_lag1m, by = join_by(date)) %>%
#  fill(unrate_jp_lag1m, .direction = "down") %>%
  left_join(unrate_uk_lag1m, by = join_by(date)) %>%
  fill(unrate_uk_lag1m, .direction = "down") %>%
  left_join(unrate_us_lag1m, by = join_by(date)) %>%
  fill(unrate_us_lag1m, .direction = "down")


# cpi
cpi_ca_lag2m <- read.csv("cpi/cpi_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_ca_lag2m = as.numeric(CPALCY01CAM661N)) %>%
  dplyr::select(c("date", "cpi_ca_lag2m"))

cpi_eu_lag2m <- read.csv("cpi/cpi_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_eu_lag2m = as.numeric(CP0000EU272020M086NEST)) %>%
  dplyr::select(c("date", "cpi_eu_lag2m"))

#cpi_jp_lag2m <- read.csv("cpi/cpi_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(2) %m+% days(cpi_reporting_lag)) %>%
#  mutate(cpi_jp_lag2m = as.numeric(JPNCPIALLMINMEI)) %>%
#  dplyr::select(c("date", "cpi_jp_lag2m"))

cpi_uk_lag2m <- read.csv("cpi/cpi_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_uk_lag2m = as.numeric(GBRCPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_uk_lag2m"))

cpi_us_lag2m <- read.csv("cpi/cpi_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_us_lag2m = as.numeric(USACPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_us_lag2m"))

exchange_rates <- exchange_rates %>%
  left_join(cpi_ca_lag2m, by = join_by(date)) %>%
  fill(cpi_ca_lag2m, .direction = "down") %>%
  left_join(cpi_eu_lag2m, by = join_by(date)) %>%
  fill(cpi_eu_lag2m, .direction = "down") %>%
#  left_join(cpi_jp_lag2m, by = join_by(date)) %>%
#  fill(cpi_jp_lag2m, .direction = "down") %>%
  left_join(cpi_uk_lag2m, by = join_by(date)) %>%
  fill(cpi_uk_lag2m, .direction = "down") %>%
  left_join(cpi_us_lag2m, by = join_by(date)) %>%
  fill(cpi_us_lag2m, .direction = "down")



# m1
m1_ca_lag2m <- read.csv("m1/m1_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_ca_lag2m = as.numeric(MANMM101CAM189S)) %>%
  dplyr::select(c("date", "m1_ca_lag2m"))

m1_eu_lag2m <- read.csv("m1/m1_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_eu_lag2m = as.numeric(MANMM101EZM189S)) %>%
  dplyr::select(c("date", "m1_eu_lag2m"))

#m1_jp_lag2m <- read.csv("m1/m1_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(2) %m+% days(m1_reporting_lag)) %>%
#  mutate(m1_jp_lag2m = as.numeric(MANMM101JPM189S)) %>%
#  dplyr::select(c("date", "m1_jp_lag2m"))

m1_uk_lag2m <- read.csv("m1/m1_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_uk_lag2m = as.numeric(MANMM101GBM189S)) %>%
  dplyr::select(c("date", "m1_uk_lag2m"))

m1_us_lag2m <- read.csv("m1/m1_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_us_lag2m = as.numeric(MANMM101USM189S)) %>%
  dplyr::select(c("date", "m1_us_lag2m"))

exchange_rates <- exchange_rates %>%
  left_join(m1_ca_lag2m, by = join_by(date)) %>%
  fill(m1_ca_lag2m, .direction = "down") %>%
  left_join(m1_eu_lag2m, by = join_by(date)) %>%
  fill(m1_eu_lag2m, .direction = "down") %>%
#  left_join(m1_jp_lag2m, by = join_by(date)) %>%
#  fill(m1_jp_lag2m, .direction = "down") %>%
  left_join(m1_uk_lag2m, by = join_by(date)) %>%
  fill(m1_uk_lag2m, .direction = "down") %>%
  left_join(m1_us_lag2m, by = join_by(date)) %>%
  fill(m1_us_lag2m, .direction = "down")

# m2
m2_ca_lag2m <- read.csv("m2/m2_can.csv", skip = 51) %>%
  mutate(date = ymd(date) %m+% months(2) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_ca_lag2m = as.numeric(V41552786_E1)) %>%
  dplyr::select(c("date", "m2_ca_lag2m"))

m2_eu_lag2m <- read.csv("m2/m2_eu.csv") %>%
  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_eu_lag2m = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_eu_lag2m"))

#m2_jp_lag2m <- read.csv("m2/m2_jpn.csv") %>%
#  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
#  mutate(m2_jp_lag2m = as.numeric(s1)) %>%
#  dplyr::select(c("date", "m2_jp_lag2m"))

m2_uk_lag2m <- read.csv("m2/m2_uk.csv") %>%
  mutate(date = dmy(Date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_uk_lag2m = as.numeric(LPMVQXV)) %>%
  dplyr::select(c("date", "m2_uk_lag2m"))

m2_us_lag2m <- read.csv("m2/m2_us.csv") %>%
  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_us_lag2m = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_us_lag2m"))

exchange_rates <- exchange_rates %>%
  left_join(m2_ca_lag2m, by = join_by(date)) %>%
  fill(m2_ca_lag2m, .direction = "down") %>%
  left_join(m2_eu_lag2m, by = join_by(date)) %>%
  fill(m2_eu_lag2m, .direction = "down") %>%
#  left_join(m2_jp_lag2m, by = join_by(date)) %>%
#  fill(m2_jp_lag2m, .direction = "down") %>%
  left_join(m2_uk_lag2m, by = join_by(date)) %>%
  fill(m2_uk_lag2m, .direction = "down") %>%
  left_join(m2_us_lag2m, by = join_by(date)) %>%
  fill(m2_us_lag2m, .direction = "down")


# uncertainty
uncertainty_ca_lag2m <- read.csv("uncertainty/uncertainty_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_ca_lag2m = as.numeric(CANEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_ca_lag2m"))

uncertainty_eu_lag2m <- read.csv("uncertainty/uncertainty_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_eu_lag2m = as.numeric(EUEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_eu_lag2m"))

#uncertaainty_jp_lag2m <- read_excel("uncertainty/uncertainty_jpn.xlsx") %>%
#  mutate(date = ymd(paste(Year, "-", Month, "-01")) %m+% months(2) %m+% days(uncertainty_reporting_lag)) %>%
#  mutate(uncertaainty_jp_lag2m = as.numeric(`News-based Economic Policy Uncertainty Index`)) %>%
#  dplyr::select(c("date", "uncertaainty_jp_lag2m"))

uncertainty_uk_lag2m <- read.csv("uncertainty/uncertainty_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_uk_lag2m = as.numeric(UKEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_uk_lag2m"))

uncertainty_us_lag2m <- read.csv("uncertainty/uncertainty_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_us_lag2m = as.numeric(USEPUINDXD)) %>%
  dplyr::select(c("date", "uncertainty_us_lag2m"))

exchange_rates <- exchange_rates %>%
  left_join(uncertainty_ca_lag2m, by = join_by(date)) %>%
  fill(uncertainty_ca_lag2m, .direction = "down") %>%
  left_join(uncertainty_eu_lag2m, by = join_by(date)) %>%
  fill(uncertainty_eu_lag2m, .direction = "down") %>%
#  left_join(uncertaainty_jp_lag2m, by = join_by(date)) %>%
#  fill(uncertaainty_jp_lag2m, .direction = "down") %>%
  left_join(uncertainty_uk_lag2m, by = join_by(date)) %>%
  fill(uncertainty_uk_lag2m, .direction = "down") %>%
  left_join(uncertainty_us_lag2m, by = join_by(date)) %>%
  fill(uncertainty_us_lag2m, .direction = "down")

# unemployment
unrate_ca_lag2m <- read.csv("unemployment/unemployment_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_ca_lag2m = as.numeric(LRHUTTTTCAM156S)) %>%
  dplyr::select(c("date", "unrate_ca_lag2m"))

unrate_eu_lag2m <- read_excel("unemployment/unrate_eu2.xlsx") %>%
  mutate(date = ymd(paste(TIME, "-01")) %m+% months(2) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_eu_lag2m = as.numeric(unrate)) %>%
  dplyr::select(c("date", "unrate_eu_lag2m"))

#unrate_jp_lag2m <- read.csv("unemployment/unemployment_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(2) %m+% days(unrate_reporting_lag)) %>%
#  mutate(unrate_jp_lag2m = as.numeric(LRHUTTTTJPM156S)) %>%
#  dplyr::select(c("date", "unrate_jp_lag2m"))

unrate_uk_lag2m <- read.csv("unemployment/unemployment_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_uk_lag2m = as.numeric(LRHUTTTTGBM156S)) %>%
  dplyr::select(c("date", "unrate_uk_lag2m"))

unrate_us_lag2m <- read.csv("unemployment/unemployment_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(2) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_us_lag2m = as.numeric(LRHUTTTTUSM156S)) %>%
  dplyr::select(c("date", "unrate_us_lag2m"))

exchange_rates <- exchange_rates %>%
  left_join(unrate_ca_lag2m, by = join_by(date)) %>%
  fill(unrate_ca_lag2m, .direction = "down") %>%
  left_join(unrate_eu_lag2m, by = join_by(date)) %>%
  fill(unrate_eu_lag2m, .direction = "down") %>%
#  left_join(unrate_jp_lag2m, by = join_by(date)) %>%
#  fill(unrate_jp_lag2m, .direction = "down") %>%
  left_join(unrate_uk_lag2m, by = join_by(date)) %>%
  fill(unrate_uk_lag2m, .direction = "down") %>%
  left_join(unrate_us_lag2m, by = join_by(date)) %>%
  fill(unrate_us_lag2m, .direction = "down")


# cpi
cpi_ca_lag3m <- read.csv("cpi/cpi_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_ca_lag3m = as.numeric(CPALCY01CAM661N)) %>%
  dplyr::select(c("date", "cpi_ca_lag3m"))

cpi_eu_lag3m <- read.csv("cpi/cpi_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_eu_lag3m = as.numeric(CP0000EU272020M086NEST)) %>%
  dplyr::select(c("date", "cpi_eu_lag3m"))

#cpi_jp_lag3m <- read.csv("cpi/cpi_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(3) %m+% days(cpi_reporting_lag)) %>%
#  mutate(cpi_jp_lag3m = as.numeric(JPNCPIALLMINMEI)) %>%
#  dplyr::select(c("date", "cpi_jp_lag3m"))

cpi_uk_lag3m <- read.csv("cpi/cpi_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_uk_lag3m = as.numeric(GBRCPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_uk_lag3m"))

cpi_us_lag3m <- read.csv("cpi/cpi_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(cpi_reporting_lag)) %>%
  mutate(cpi_us_lag3m = as.numeric(USACPIALLMINMEI)) %>%
  dplyr::select(c("date", "cpi_us_lag3m"))

exchange_rates <- exchange_rates %>%
  left_join(cpi_ca_lag3m, by = join_by(date)) %>%
  fill(cpi_ca_lag3m, .direction = "down") %>%
  left_join(cpi_eu_lag3m, by = join_by(date)) %>%
  fill(cpi_eu_lag3m, .direction = "down") %>%
#  left_join(cpi_jp_lag3m, by = join_by(date)) %>%
#  fill(cpi_jp_lag3m, .direction = "down") %>%
  left_join(cpi_uk_lag3m, by = join_by(date)) %>%
  fill(cpi_uk_lag3m, .direction = "down") %>%
  left_join(cpi_us_lag3m, by = join_by(date)) %>%
  fill(cpi_us_lag3m, .direction = "down")



# m1
m1_ca_lag3m <- read.csv("m1/m1_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_ca_lag3m = as.numeric(MANMM101CAM189S)) %>%
  dplyr::select(c("date", "m1_ca_lag3m"))

m1_eu_lag3m <- read.csv("m1/m1_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_eu_lag3m = as.numeric(MANMM101EZM189S)) %>%
  dplyr::select(c("date", "m1_eu_lag3m"))

#m1_jp_lag3m <- read.csv("m1/m1_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(3) %m+% days(m1_reporting_lag)) %>%
#  mutate(m1_jp_lag3m = as.numeric(MANMM101JPM189S)) %>%
#  dplyr::select(c("date", "m1_jp_lag3m"))

m1_uk_lag3m <- read.csv("m1/m1_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_uk_lag3m = as.numeric(MANMM101GBM189S)) %>%
  dplyr::select(c("date", "m1_uk_lag3m"))

m1_us_lag3m <- read.csv("m1/m1_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(m1_reporting_lag)) %>%
  mutate(m1_us_lag3m = as.numeric(MANMM101USM189S)) %>%
  dplyr::select(c("date", "m1_us_lag3m"))

exchange_rates <- exchange_rates %>%
  left_join(m1_ca_lag3m, by = join_by(date)) %>%
  fill(m1_ca_lag3m, .direction = "down") %>%
  left_join(m1_eu_lag3m, by = join_by(date)) %>%
  fill(m1_eu_lag3m, .direction = "down") %>%
 # left_join(m1_jp_lag3m, by = join_by(date)) %>%
#  fill(m1_jp_lag3m, .direction = "down") %>%
  left_join(m1_uk_lag3m, by = join_by(date)) %>%
  fill(m1_uk_lag3m, .direction = "down") %>%
  left_join(m1_us_lag3m, by = join_by(date)) %>%
  fill(m1_us_lag3m, .direction = "down")

# m2
m2_ca_lag3m <- read.csv("m2/m2_can.csv", skip = 51) %>%
  mutate(date = ymd(date) %m+% months(3) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_ca_lag3m = as.numeric(V41552786_E1)) %>%
  dplyr::select(c("date", "m2_ca_lag3m"))

m2_eu_lag3m <- read.csv("m2/m2_eu.csv") %>%
  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_eu_lag3m = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_eu_lag3m"))

#m2_jp_lag3m <- read.csv("m2/m2_jpn.csv") %>%
#  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
#  mutate(m2_jp_lag3m = as.numeric(s1)) %>%
#  dplyr::select(c("date", "m2_jp_lag3m"))

m2_uk_lag3m <- read.csv("m2/m2_uk.csv") %>%
  mutate(date = dmy(Date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_uk_lag3m = as.numeric(LPMVQXV)) %>%
  dplyr::select(c("date", "m2_uk_lag3m"))

m2_us_lag3m <- read.csv("m2/m2_us.csv") %>%
  mutate(date = ymd(date) %m+% days(1) %m+% days(m2_reporting_lag)) %>%
  mutate(m2_us_lag3m = as.numeric(s1)) %>%
  dplyr::select(c("date", "m2_us_lag3m"))

exchange_rates <- exchange_rates %>%
  left_join(m2_ca_lag3m, by = join_by(date)) %>%
  fill(m2_ca_lag3m, .direction = "down") %>%
  left_join(m2_eu_lag3m, by = join_by(date)) %>%
  fill(m2_eu_lag3m, .direction = "down") %>%
#  left_join(m2_jp_lag3m, by = join_by(date)) %>%
#  fill(m2_jp_lag3m, .direction = "down") %>%
  left_join(m2_uk_lag3m, by = join_by(date)) %>%
  fill(m2_uk_lag3m, .direction = "down") %>%
  left_join(m2_us_lag3m, by = join_by(date)) %>%
  fill(m2_us_lag3m, .direction = "down")


# uncertainty
uncertainty_ca_lag3m <- read.csv("uncertainty/uncertainty_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_ca_lag3m = as.numeric(CANEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_ca_lag3m"))

uncertainty_eu_lag3m <- read.csv("uncertainty/uncertainty_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_eu_lag3m = as.numeric(EUEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_eu_lag3m"))

#uncertaainty_jp_lag3m <- read_excel("uncertainty/uncertainty_jpn.xlsx") %>%
#  mutate(date = ymd(paste(Year, "-", Month, "-01")) %m+% months(3) %m+% days(uncertainty_reporting_lag)) %>%
#  mutate(uncertaainty_jp_lag3m = as.numeric(`News-based Economic Policy Uncertainty Index`)) %>%
#  dplyr::select(c("date", "uncertaainty_jp_lag3m"))

uncertainty_uk_lag3m <- read.csv("uncertainty/uncertainty_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_uk_lag3m = as.numeric(UKEPUINDXM)) %>%
  dplyr::select(c("date", "uncertainty_uk_lag3m"))

uncertainty_us_lag3m <- read.csv("uncertainty/uncertainty_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(uncertainty_reporting_lag)) %>%
  mutate(uncertainty_us_lag3m = as.numeric(USEPUINDXD)) %>%
  dplyr::select(c("date", "uncertainty_us_lag3m"))

exchange_rates <- exchange_rates %>%
  left_join(uncertainty_ca_lag3m, by = join_by(date)) %>%
  fill(uncertainty_ca_lag3m, .direction = "down") %>%
  left_join(uncertainty_eu_lag3m, by = join_by(date)) %>%
  fill(uncertainty_eu_lag3m, .direction = "down") %>%
#  left_join(uncertaainty_jp_lag3m, by = join_by(date)) %>%
#  fill(uncertaainty_jp_lag3m, .direction = "down") %>%
  left_join(uncertainty_uk_lag3m, by = join_by(date)) %>%
  fill(uncertainty_uk_lag3m, .direction = "down") %>%
  left_join(uncertainty_us_lag3m, by = join_by(date)) %>%
  fill(uncertainty_us_lag3m, .direction = "down")

# unemployment
unrate_ca_lag3m <- read.csv("unemployment/unemployment_can.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_ca_lag3m = as.numeric(LRHUTTTTCAM156S)) %>%
  dplyr::select(c("date", "unrate_ca_lag3m"))

unrate_eu_lag3m <- read_excel("unemployment/unrate_eu2.xlsx") %>%
  mutate(date = ymd(paste(TIME, "-01")) %m+% months(3) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_eu_lag3m = as.numeric(unrate)) %>%
  dplyr::select(c("date", "unrate_eu_lag3m"))

#unrate_jp_lag3m <- read.csv("unemployment/unemployment_jpn.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(3) %m+% days(unrate_reporting_lag)) %>%
#  mutate(unrate_jp_lag3m = as.numeric(LRHUTTTTJPM156S)) %>%
#  dplyr::select(c("date", "unrate_jp_lag3m"))

unrate_uk_lag3m <- read.csv("unemployment/unemployment_uk.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_uk_lag3m = as.numeric(LRHUTTTTGBM156S)) %>%
  dplyr::select(c("date", "unrate_uk_lag3m"))

unrate_us_lag3m <- read.csv("unemployment/unemployment_us.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(unrate_reporting_lag)) %>%
  mutate(unrate_us_lag3m = as.numeric(LRHUTTTTUSM156S)) %>%
  dplyr::select(c("date", "unrate_us_lag3m"))

exchange_rates <- exchange_rates %>%
  left_join(unrate_ca_lag3m, by = join_by(date)) %>%
  fill(unrate_ca_lag3m, .direction = "down") %>%
  left_join(unrate_eu_lag3m, by = join_by(date)) %>%
  fill(unrate_eu_lag3m, .direction = "down") %>%
#  left_join(unrate_jp_lag3m, by = join_by(date)) %>%
#  fill(unrate_jp_lag3m, .direction = "down") %>%
  left_join(unrate_uk_lag3m, by = join_by(date)) %>%
  fill(unrate_uk_lag3m, .direction = "down") %>%
  left_join(unrate_us_lag3m, by = join_by(date)) %>%
  fill(unrate_us_lag3m, .direction = "down")






# lagged gdp data

gdp_reporting_lag <- 14

gdp_ca_index_lag1q <- read.csv("gdp/gdp_can_index.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(gdp_reporting_lag)) %>%
  mutate(gdp_ca_index_lag1q = as.numeric(NAEXKP01CAQ189S_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_ca_index_lag1q"))

gdp_eu_index_lag1q <- read.csv("gdp/gdp_eu_index.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(gdp_reporting_lag)) %>%
  mutate(gdp_eu_index_lag1q = as.numeric(CPMNACSCAB1GQEU272020_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_eu_index_lag1q"))

#gdp_jp_index_lag1q <- read.csv("gdp/gdp_jpn_index.csv") %>%
#  mutate(date = ymd(DATE) %m+% months(3) %m+% days(gdp_reporting_lag)) %>%
#  mutate(gdp_jp_index_lag1q = as.numeric(JPNNGDP_NBD20180101)) %>%
#  dplyr::select(c("date", "gdp_jp_index_lag1q"))

gdp_uk_index_lag1q <- read.csv("gdp/gdp_uk_index.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(gdp_reporting_lag)) %>%
  mutate(gdp_uk_index_lag1q = as.numeric(UKNGDP_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_uk_index_lag1q"))

gdp_us_index_lag1q <- read.csv("gdp/gdp_us_index.csv") %>%
  mutate(date = ymd(DATE) %m+% months(3) %m+% days(gdp_reporting_lag)) %>%
  mutate(gdp_us_index_lag1q = as.numeric(GDP_NBD20180101)) %>%
  dplyr::select(c("date", "gdp_us_index_lag1q"))

exchange_rates <- exchange_rates %>%
  left_join(gdp_ca_index_lag1q, by = join_by(date)) %>%
  fill(gdp_ca_index_lag1q, .direction = "down") %>%
  left_join(gdp_eu_index_lag1q, by = join_by(date)) %>%
  fill(gdp_eu_index_lag1q, .direction = "down") %>%
#  left_join(gdp_jp_index_lag1q, by = join_by(date)) %>%
#  fill(gdp_jp_index_lag1q, .direction = "down") %>%
  left_join(gdp_uk_index_lag1q, by = join_by(date)) %>%
  fill(gdp_uk_index_lag1q, .direction = "down") %>%
  left_join(gdp_us_index_lag1q, by = join_by(date)) %>%
  fill(gdp_us_index_lag1q, .direction = "down")



# policy rate announced changes

polrate_ca_anncmnt <- read.csv("interest_rates/ffr_can.csv", skip = 11) %>%
  mutate(date = ymd(Date) %m+% days(-1)) %>%
  mutate(polrate_ca_anncmnt = as.numeric(V39079)) %>%
  dplyr::select(c("date", "polrate_ca_anncmnt"))

polrate_eu_anncmnt <- read.csv("interest_rates/ffr_eu.csv") %>%
  mutate(date = ymd(DATE) %m+% days(-6)) %>%
  mutate(polrate_eu_anncmnt = as.numeric(ECBMRRFR)) %>%
  dplyr::select(c("date", "polrate_eu_anncmnt"))

polrate_us_lower_anncmnt <- read.csv("interest_rates/ffr_us_lower.csv") %>%
  mutate(date = ymd(DATE) %m+% days(-1)) %>%
  mutate(polrate_us_lower_anncmnt = as.numeric(DFEDTARL)) %>%
  dplyr::select(c("date", "polrate_us_lower_anncmnt"))

polrate_us_upper_anncmnt <- read.csv("interest_rates/ffr_us_upper.csv") %>%
  mutate(date = ymd(DATE) %m+% days(-1)) %>%
  mutate(polrate_us_upper_anncmnt = as.numeric(DFEDTARU)) %>%
  dplyr::select(c("date", "polrate_us_upper_anncmnt"))

exchange_rates <- exchange_rates %>%
  left_join(polrate_ca_anncmnt, by = join_by(date)) %>%
  fill(polrate_ca_anncmnt, .direction = "down") %>%
  left_join(polrate_eu_anncmnt, by = join_by(date)) %>%
  fill(polrate_eu_anncmnt, .direction = "down") %>%
  left_join(polrate_us_lower_anncmnt, by = join_by(date)) %>%
  fill(polrate_us_lower_anncmnt, .direction = "down") %>%
  left_join(polrate_us_upper_anncmnt, by = join_by(date)) %>%
  fill(polrate_us_upper_anncmnt, .direction = "down")






# lagged stocks
ftse100_lag1d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 1)) %>%
  mutate(ftse100_lag1d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag1d"))

#nikkei225_lag1d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 1)) %>%
#  mutate(nikkei225_lag1d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag1d"))

sp500_lag1d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 1)) %>%
  mutate(sp500_lag1d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag1d"))

sptsx_lag1d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 1)) %>%
  mutate(sptsx_lag1d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag1d"))

stoxx600_lag1d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 1)) %>%
  mutate(stoxx600_lag1d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag1d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag1d, by = join_by(date)) %>%
#  left_join(nikkei225_lag1d, by = join_by(date)) %>%
  left_join(sp500_lag1d, by = join_by(date)) %>%
  left_join(sptsx_lag1d, by = join_by(date)) %>%
  left_join(stoxx600_lag1d, by = join_by(date))


ftse100_lag2d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 2)) %>%
  mutate(ftse100_lag2d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag2d"))

#nikkei225_lag2d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 2)) %>%
#  mutate(nikkei225_lag2d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag2d"))

sp500_lag2d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 2)) %>%
  mutate(sp500_lag2d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag2d"))

sptsx_lag2d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 2)) %>%
  mutate(sptsx_lag2d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag2d"))

stoxx600_lag2d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 2)) %>%
  mutate(stoxx600_lag2d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag2d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag2d, by = join_by(date)) %>%
#  left_join(nikkei225_lag2d, by = join_by(date)) %>%
  left_join(sp500_lag2d, by = join_by(date)) %>%
  left_join(sptsx_lag2d, by = join_by(date)) %>%
  left_join(stoxx600_lag2d, by = join_by(date))


ftse100_lag3d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 3)) %>%
  mutate(ftse100_lag3d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag3d"))

#nikkei225_lag3d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 3)) %>%
#  mutate(nikkei225_lag3d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag3d"))

sp500_lag3d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 3)) %>%
  mutate(sp500_lag3d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag3d"))

sptsx_lag3d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 3)) %>%
  mutate(sptsx_lag3d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag3d"))

stoxx600_lag3d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 3)) %>%
  mutate(stoxx600_lag3d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag3d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag3d, by = join_by(date)) %>%
#  left_join(nikkei225_lag3d, by = join_by(date)) %>%
  left_join(sp500_lag3d, by = join_by(date)) %>%
  left_join(sptsx_lag3d, by = join_by(date)) %>%
  left_join(stoxx600_lag3d, by = join_by(date))


ftse100_lag4d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 4)) %>%
  mutate(ftse100_lag4d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag4d"))

#nikkei225_lag4d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 4)) %>%
#  mutate(nikkei225_lag4d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag4d"))

sp500_lag4d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 4)) %>%
  mutate(sp500_lag4d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag4d"))

sptsx_lag4d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 4)) %>%
  mutate(sptsx_lag4d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag4d"))

stoxx600_lag4d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 4)) %>%
  mutate(stoxx600_lag4d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag4d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag4d, by = join_by(date)) %>%
#  left_join(nikkei225_lag4d, by = join_by(date)) %>%
  left_join(sp500_lag4d, by = join_by(date)) %>%
  left_join(sptsx_lag4d, by = join_by(date)) %>%
  left_join(stoxx600_lag4d, by = join_by(date))

ftse100_lag5d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 5)) %>%
  mutate(ftse100_lag5d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag5d"))

#nikkei225_lag5d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 5)) %>%
#  mutate(nikkei225_lag5d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag5d"))

sp500_lag5d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 5)) %>%
  mutate(sp500_lag5d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag5d"))

sptsx_lag5d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 5)) %>%
  mutate(sptsx_lag5d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag5d"))

stoxx600_lag5d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 5)) %>%
  mutate(stoxx600_lag5d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag5d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag5d, by = join_by(date)) %>%
#  left_join(nikkei225_lag5d, by = join_by(date)) %>%
  left_join(sp500_lag5d, by = join_by(date)) %>%
  left_join(sptsx_lag5d, by = join_by(date)) %>%
  left_join(stoxx600_lag5d, by = join_by(date))



ftse100_lag6d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 6)) %>%
  mutate(ftse100_lag6d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag6d"))

#nikkei225_lag6d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 6)) %>%
#  mutate(nikkei225_lag6d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag6d"))

sp500_lag6d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 6)) %>%
  mutate(sp500_lag6d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag6d"))

sptsx_lag6d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 6)) %>%
  mutate(sptsx_lag6d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag6d"))

stoxx600_lag6d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 6)) %>%
  mutate(stoxx600_lag6d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag6d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag6d, by = join_by(date)) %>%
#  left_join(nikkei225_lag6d, by = join_by(date)) %>%
  left_join(sp500_lag6d, by = join_by(date)) %>%
  left_join(sptsx_lag6d, by = join_by(date)) %>%
  left_join(stoxx600_lag6d, by = join_by(date))


ftse100_lag7d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 7)) %>%
  mutate(ftse100_lag7d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag7d"))

#nikkei225_lag7d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 7)) %>%
#  mutate(nikkei225_lag7d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag7d"))

sp500_lag7d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 7)) %>%
  mutate(sp500_lag7d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag7d"))

sptsx_lag7d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 7)) %>%
  mutate(sptsx_lag7d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag7d"))

stoxx600_lag7d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 7)) %>%
  mutate(stoxx600_lag7d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag7d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag7d, by = join_by(date)) %>%
#  left_join(nikkei225_lag7d, by = join_by(date)) %>%
  left_join(sp500_lag7d, by = join_by(date)) %>%
  left_join(sptsx_lag7d, by = join_by(date)) %>%
  left_join(stoxx600_lag7d, by = join_by(date))


ftse100_lag8d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 8)) %>%
  mutate(ftse100_lag8d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag8d"))

#nikkei225_lag8d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 8)) %>%
#  mutate(nikkei225_lag8d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag8d"))

sp500_lag8d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 8)) %>%
  mutate(sp500_lag8d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag8d"))

sptsx_lag8d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 8)) %>%
  mutate(sptsx_lag8d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag8d"))

stoxx600_lag8d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 8)) %>%
  mutate(stoxx600_lag8d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag8d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag8d, by = join_by(date)) %>%
#  left_join(nikkei225_lag8d, by = join_by(date)) %>%
  left_join(sp500_lag8d, by = join_by(date)) %>%
  left_join(sptsx_lag8d, by = join_by(date)) %>%
  left_join(stoxx600_lag8d, by = join_by(date))


ftse100_lag9d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 9)) %>%
  mutate(ftse100_lag9d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag9d"))

#nikkei225_lag9d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 9)) %>%
#  mutate(nikkei225_lag9d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag9d"))

sp500_lag9d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 9)) %>%
  mutate(sp500_lag9d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag9d"))

sptsx_lag9d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 9)) %>%
  mutate(sptsx_lag9d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag9d"))

stoxx600_lag9d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 9)) %>%
  mutate(stoxx600_lag9d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag9d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag9d, by = join_by(date)) %>%
#  left_join(nikkei225_lag9d, by = join_by(date)) %>%
  left_join(sp500_lag9d, by = join_by(date)) %>%
  left_join(sptsx_lag9d, by = join_by(date)) %>%
  left_join(stoxx600_lag9d, by = join_by(date))


ftse100_lag10d <- read.csv("stocks/ftse100.csv") %>%
  mutate(date = lag(mdy(Date), 10)) %>%
  mutate(ftse100_lag10d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "ftse100_lag10d"))

#nikkei225_lag10d <- read.csv("stocks/nikkei225.csv") %>%
#  arrange(desc(ymd(Date))) %>%
#  filter(!is.na(as.numeric(Close))) %>%
#  mutate(date = lag(ymd(Date), 10)) %>%
#  mutate(nikkei225_lag10d = as.numeric(Close)) %>%
#  dplyr::select(c("date", "nikkei225_lag10d"))

sp500_lag10d <- read.csv("stocks/sp500.csv") %>%
  mutate(date = lag(mdy(Datum), 10)) %>%
  mutate(sp500_lag10d = as.numeric(Schluss.Letzter)) %>%
  dplyr::select(c("date", "sp500_lag10d"))

sptsx_lag10d <- read.csv("stocks/sptsx.csv") %>%
  mutate(date = lag(mdy(Date), 10)) %>%
  mutate(sptsx_lag10d = as.numeric(gsub("\\,", "", as.character(Price)))) %>%
  dplyr::select(c("date", "sptsx_lag10d"))

stoxx600_lag10d <- read.csv("stocks/stoxx600.csv", dec = ",") %>%
  mutate(date = lag(dmy(Datum), 10)) %>%
  mutate(stoxx600_lag10d = as.numeric(Zuletzt)) %>%
  dplyr::select(c("date", "stoxx600_lag10d"))


exchange_rates <- exchange_rates %>%
  left_join(ftse100_lag10d, by = join_by(date)) %>%
#  left_join(nikkei225_lag10d, by = join_by(date)) %>%
  left_join(sp500_lag10d, by = join_by(date)) %>%
  left_join(sptsx_lag10d, by = join_by(date)) %>%
  left_join(stoxx600_lag10d, by = join_by(date))

# time series structure for exchange rates






# removing exchange rate na rows
exchange_rates <- exchange_rates %>%
  filter(!is.na(usd_cad)) %>%
  filter(date < "2023-04-01") %>%
  fill(names(exchange_rates), .direction = "up") %>%
  mutate(usd_cad_lag1d = lag(usd_cad, 1),
         usd_eur_lag1d = lag(usd_eur, 1),
         usd_gbp_lag1d = lag(usd_gbp, 1),
#         usd_yen_lag1d = lag(usd_yen, 1),
         
         usd_cad_lag2d = lag(usd_cad, 2),
         usd_eur_lag2d = lag(usd_eur, 2),
         usd_gbp_lag2d = lag(usd_gbp, 2),
#         usd_yen_lag2d = lag(usd_yen, 2),
         
         usd_cad_lag3d = lag(usd_cad, 3),
         usd_eur_lag3d = lag(usd_eur, 3),
         usd_gbp_lag3d = lag(usd_gbp, 3),
#         usd_yen_lag3d = lag(usd_yen, 3),
         
         usd_cad_lag4d = lag(usd_cad, 4),
         usd_eur_lag4d = lag(usd_eur, 4),
         usd_gbp_lag4d = lag(usd_gbp, 4),
#         usd_yen_lag4d = lag(usd_yen, 4),
         
         usd_cad_lag5d = lag(usd_cad, 5),
         usd_eur_lag5d = lag(usd_eur, 5),
         usd_gbp_lag5d = lag(usd_gbp, 5),
#         usd_yen_lag5d = lag(usd_yen, 5),
         
         usd_cad_lag6d = lag(usd_cad, 6),
         usd_eur_lag6d = lag(usd_eur, 6),
         usd_gbp_lag6d = lag(usd_gbp, 6),
#         usd_yen_lag6d = lag(usd_yen, 6),
         
         usd_cad_lag7d = lag(usd_cad, 7),
         usd_eur_lag7d = lag(usd_eur, 7),
         usd_gbp_lag7d = lag(usd_gbp, 7),
#         usd_yen_lag7d = lag(usd_yen, 7),
         
         usd_cad_lag8d = lag(usd_cad, 8),
         usd_eur_lag8d = lag(usd_eur, 8),
         usd_gbp_lag8d = lag(usd_gbp, 8),
#         usd_yen_lag8d = lag(usd_yen, 8),
         
         usd_cad_lag9d = lag(usd_cad, 9),
         usd_eur_lag9d = lag(usd_eur, 9),
         usd_gbp_lag9d = lag(usd_gbp, 9),
#         usd_yen_lag9d = lag(usd_yen, 9),
         
         usd_cad_lag10d = lag(usd_cad, 10),
         usd_eur_lag10d = lag(usd_eur, 10),
         usd_gbp_lag10d = lag(usd_gbp, 10),
#         usd_yen_lag10d = lag(usd_yen, 10),
         
         usd_cad_lag19d = lag(usd_cad, 19),
         usd_eur_lag19d = lag(usd_eur, 19),
         usd_gbp_lag19d = lag(usd_gbp, 19),
#         usd_yen_lag19d = lag(usd_yen, 19),
         
         usd_cad_lag20d = lag(usd_cad, 20),
         usd_eur_lag20d = lag(usd_eur, 20),
         usd_gbp_lag20d = lag(usd_gbp, 20),
#         usd_yen_lag20d = lag(usd_yen, 20),
         
         usd_cad_lag21d = lag(usd_cad, 21),
         usd_eur_lag21d = lag(usd_eur, 21),
         usd_gbp_lag21d = lag(usd_gbp, 21),
#         usd_yen_lag21d = lag(usd_yen, 21),
         
         usd_cad_lag22d = lag(usd_cad, 22),
         usd_eur_lag22d = lag(usd_eur, 22),
         usd_gbp_lag22d = lag(usd_gbp, 22),
#         usd_yen_lag22d = lag(usd_yen, 22),
         
         usd_cad_lag23d = lag(usd_cad, 23),
         usd_eur_lag23d = lag(usd_eur, 23),
         usd_gbp_lag23d = lag(usd_gbp, 23)) %>%
#         usd_yen_lag23d = lag(usd_yen, 23)) %>%
  dplyr::select(-c("ftse100", "sp500", "sptsx", "stoxx600", 
                   "cpi_ca", "cpi_eu", "cpi_us", "cpi_uk",
                   "gdp_ca", "gdp_eu", "gdp_uk", "gdp_us",
                   "gdp_ca_index", "gdp_eu_index", "gdp_uk_index", "gdp_us_index",
                   "m1_ca", "m1_eu", "m1_uk", "m1_us",
                   "m2_ca", "m2_eu", "m2_uk", "m2_us",
                   "uncertainty_ca", "uncertainty_eu", "uncertainty_uk", "uncertainty_us",
                   "unrate_ca", "unrate_eu", "unrate_uk", "unrate_us")) %>%
  filter(date > "2018-03-31")



write.csv(exchange_rates, "exchange_rates.csv")
write.xlsx(exchange_rates, "exchange_rates.xlsx")





