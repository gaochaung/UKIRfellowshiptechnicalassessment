## clear the workspace ########
rm(list = ls())
####### load package ##########
install.packages("DBI")
install.packages("odbc")
install.packages("dplyr")
install.packages("dbplyr")
install.packages("haven")
install.packages("writexl")
install.packages("tidyverse")
install.packages("plotly")
library(odbc)
library(DBI)
library(dplyr)
library(dbplyr)
library(readxl)
library(plotly)
library(lubridate)

####### load data #############
# read the price file
setwd("./material")
getwd()
price <- read.csv("electricity-prices-day-a.csv")

# read the espeni file
espeni <- read.csv("espeni.csv")

###############################
####### energy demand #########
###############################
espeni$ELEXM_SETTLEMENT_DATE_floor <- floor_date(as.Date(as.character(espeni$ELEXM_SETTLEMENT_DATE),format="%Y-%m-%d"), "month")  #cahnge date to the first of each month
price$X. <- as.Date(as.character(price$X.),format="%Y-%m-%d")
all <- merge(espeni, price, by.x = "ELEXM_SETTLEMENT_DATE_floor", by.y = "X.", all.x = TRUE)
# change demand as monthly
all1 <- all %>% 
  group_by(ELEXM_SETTLEMENT_DATE_floor) %>% 
  mutate(sum_demand_monthly = sum(POWER_ESPENI_MW))
all1 <- unique(all1[,c("ELEXM_SETTLEMENT_DATE_floor","sum_demand_monthly","Price")])

#### plot #####################
par(mfrow=c(2,1))
# plot price change vs time
plot(all1$ELEXM_SETTLEMENT_DATE_floor, all1$Price, xlab = "", ylab = "Energy price")
# plot demand change vs time
plot(all1$ELEXM_SETTLEMENT_DATE_floor, all1$sum_demand_monthly, xlab = "Time", ylab = "Energy demand")


##### zoom in and plot ########
all2 <- all1[all1$ELEXM_SETTLEMENT_DATE_floor>="2018-01-01",]
all2 <- all2[all2$ELEXM_SETTLEMENT_DATE_floor<"2020-01-01",]

par(mfrow=c(2,1))
# plot price change vs time
plot(Price ~ ELEXM_SETTLEMENT_DATE_floor, all2, xaxt = "n", type = "l",  xlab = "", ylab = "Energy price")
axis(1, all2$ELEXM_SETTLEMENT_DATE_floor, format(all2$ELEXM_SETTLEMENT_DATE_floor, "%Y-%m"), cex.axis = .7)
# plot demand change vs time
plot(sum_demand_monthly ~ ELEXM_SETTLEMENT_DATE_floor, all2, xaxt = "n", type = "l", xlab = "Time", ylab = "Energy demand")
axis(1, all2$ELEXM_SETTLEMENT_DATE_floor, format(all2$ELEXM_SETTLEMENT_DATE_floor, "%Y-%m"), cex.axis = .7)


###############################
###### energy security ########
###############################
#calculate energy supply per slot
all <- espeni
all$total_supply <- all$POWER_ELEXM_CCGT_MW +
  all$POWER_ELEXM_OIL_MW+
  all$POWER_ELEXM_COAL_MW+
  all$POWER_ELEXM_NUCLEAR_MW+
  all$POWER_ELEXM_WIND_MW+
  all$POWER_ELEXM_PS_MW+
  all$POWER_ELEXM_NPSHYD_MW+
  all$POWER_ELEXM_OCGT_MW+
  all$POWER_ELEXM_OTHER_POSTCALC_MW+
  all$POWER_ELEXM_BIOMASS_POSTCALC_MW+
  all$POWER_NGEM_EMBEDDED_SOLAR_GENERATION_MW+
  all$POWER_NGEM_EMBEDDED_WIND_GENERATION_MW
all$energy_gap <- all$POWER_ESPENI_MW-all$total_supply
all <- all[!(is.na(all$energy_gap)), ]
barplot(all$energy_gap)
Q <- quantile(as.double(all$energy_gap), probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(as.double(all$energy_gap), na.rm = TRUE)
up <- Q[2]+1*iqr   # Upper Range
all1 <- all[all$energy_gap>=up,] 
all1$ELEXM_utc_f <- as_datetime(all1$ELEXM_utc)
all1$timeofday <- format(as.POSIXct(all1$ELEXM_utc_f),format = "%H")
all1$timeofyear <- format(as.POSIXct(all1$ELEXM_utc_f),format = "%m")

par(mfrow=c(2,1))
plot(table(all1$timeofday),xlab = "Time of the day", ylab = "Count of energy crisis")
plot(table(all1$timeofyear),xlab = "Time of the year", ylab = "Count of energy crisis")

##### calcualte p value ############
all$ELEXM_utc_f <- as_datetime(all$ELEXM_utc)
all$timeofday <- format(as.POSIXct(all$ELEXM_utc_f),format = "%H")
all$timeofyear <- format(as.POSIXct(all$ELEXM_utc_f),format = "%m")
all$crisis <- 0
all[all$energy_gap>=up,"crisis"] <- 1

all$POWER_ELEXM_CCGT_MW_r <- all$POWER_ELEXM_CCGT_MW/all$total_supply
all$POWER_ELEXM_OIL_MW_r <- all$POWER_ELEXM_OIL_MW/all$total_supply
all$POWER_ELEXM_COAL_MW_r <- all$POWER_ELEXM_COAL_MW/all$total_supply
all$POWER_ELEXM_NUCLEAR_MW_r <- all$POWER_ELEXM_NUCLEAR_MW/all$total_supply
all$POWER_ELEXM_WIND_MW_r <- all$POWER_ELEXM_WIND_MW/all$total_supply 
all$POWER_ELEXM_PS_MW_r <- all$POWER_ELEXM_PS_MW/all$total_supply
all$POWER_ELEXM_NPSHYD_MW_r <- all$POWER_ELEXM_NPSHYD_MW/all$total_supply
all$POWER_ELEXM_OCGT_MW_r <- all$POWER_ELEXM_OCGT_MW/all$total_supply
all$POWER_ELEXM_OTHER_POSTCALC_MW_r <- all$POWER_ELEXM_OTHER_POSTCALC_MW/all$total_supply
all$POWER_ELEXM_BIOMASS_POSTCALC_MW_r <- all$POWER_ELEXM_BIOMASS_POSTCALC_MW/all$total_supply
all$POWER_NGEM_EMBEDDED_SOLAR_GENERATION_MW_r <- all$POWER_NGEM_EMBEDDED_SOLAR_GENERATION_MW/all$total_supply
all$POWER_NGEM_EMBEDDED_WIND_GENERATION_MW_r <- all$POWER_NGEM_EMBEDDED_WIND_GENERATION_MW/all$total_supply

##########################################
VarList <- c("timeofday","timeofyear","POWER_ELEXM_CCGT_MW_r","POWER_ELEXM_OIL_MW_r", "POWER_ELEXM_COAL_MW_r", "POWER_ELEXM_NUCLEAR_MW_r", "POWER_ELEXM_WIND_MW_r", 
              "POWER_ELEXM_PS_MW_r",                       "POWER_ELEXM_NPSHYD_MW_r",                   "POWER_ELEXM_OCGT_MW_r",                    
              "POWER_ELEXM_OTHER_POSTCALC_MW_r",           "POWER_ELEXM_BIOMASS_POSTCALC_MW_r",         "POWER_NGEM_EMBEDDED_SOLAR_GENERATION_MW_r",
              "POWER_NGEM_EMBEDDED_WIND_GENERATION_MW_r")
quantityTable <- data.frame(VarList)
colnames(quantityTable)[1] <- "Var"
quantityTable$p <- ""
for (rc in VarList) {
D <- all[,c("crisis", rc)]
colnames(D)[2] <- "var2"
p <- p_value_calculate_kruskal(D)
quantityTable[quantityTable$Var==rc,"p"] <- p
}

