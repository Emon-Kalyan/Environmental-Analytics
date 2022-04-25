setwd("C:/Users/ASUS/Documents/Exeter MSc in BA Learning/Environmental Analytics/food production and supply sustainability_1/NFA 2018.csv")
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(psych)
library(lubridate)
library(ggthemes)
nfa_usa_data <- read.csv("USA_NFA.csv")
View(nfa_usa_data)
##Let us first do some EDA of the data
dim(nfa_usa_data)
nfa_usa_data1 <- nfa_usa_data %>% drop_na()
dim(nfa_usa_data1)
str(nfa_usa_data1)
##No data is missing in the USA data which is good
##We understand that the data given is a time series data 
##so we can see how the consumption land vs production land
##has been changing over the years
#Let us check how consumption and Biocapacity
#in hectares had been changing
nfa_usa_data1$UN_region  <- as.factor(nfa_usa_data1$UN_region)
nfa_usa_data1$UN_subregion <- as.factor(nfa_usa_data1$UN_subregion)
nfa_usa_data1$record <- as.factor(nfa_usa_data1$record)
str(nfa_usa_data1)
nfa_usa_data1$year <- as.numeric(nfa_usa_data1$year)
usa1 <- nfa_usa_data1 %>% filter(record == "BiocapTotGHA"| record == "EFConsTotGHA")
View(usa1)
usa2 <- usa1[c("record", "year", "total")]
View(usa2)
usa2_wide <- pivot_wider(usa2, names_from = "record", values_from = "total")
dim(usa2_wide)
str(usa2_wide)
##Thus we see that a record of 54 years have been recorded
cor(usa2_wide$BiocapTotGHA, usa2_wide$EFConsTotGHA)
ggplot(usa2_wide, aes(BiocapTotGHA,EFConsTotGHA)) + geom_point() +
  geom_smooth()
ggplot(usa2_wide, aes(year,EFConsTotGHA)) + geom_line()+ geom_smooth() +
  scale_x_continuous(breaks =  seq(1961, 2014, 4))
ggplot(usa2_wide, aes(year,BiocapTotGHA)) + geom_line()+ geom_smooth() +
  scale_x_continuous(breaks =  seq(1961, 2014, 4))
##From the above graphs we see that bio capacity of North America in
##hectares have been increasing and also the consumption
##But the consumption curve has show  slight saturation and decline
##over the last few years
##We can see that there is a high correlation between
##biocapacity and consumption but also the slope of consumption vs
##capacity has fallen in last few years which is good for sustainability
##Let us visualize the trends by different types of land
nfa_usa_data1 %>% filter(record == "BiocapTotGHA"|
                       record == "EFConsTotGHA"|
                         record == "EFProdTotGHA") %>%
  ggplot(aes(year,crop_land)) + geom_line()+ geom_smooth() +
  facet_grid(~ record, scales = "free") + labs(title = "Evolution of crop land from 1961 to 2014 for USA") +
  scale_x_continuous(breaks =  seq(1961, 2014, 10)) +  theme_bw()

nfa_usa_data1 %>% filter(record == "BiocapTotGHA"|
                           record == "EFConsTotGHA"|
                           record == "EFExportsTotGHA"|
                           record == "EFImportsTotGHA"|
                           record == "EFProdTotGHA") %>%
  ggplot(aes(year,grazing_land)) + geom_line()+ 
  facet_grid(~ record) +
  scale_x_continuous(breaks =  seq(1961, 2014, 10))
nfa_usa_data1 %>% filter(record == "BiocapTotGHA"|
                           record == "EFConsTotGHA"|
                           record == "EFExportsTotGHA"|
                           record == "EFImportsTotGHA"|
                           record == "EFProdTotGHA") %>%
  ggplot(aes(year,forest_land)) + geom_line()+ 
  facet_grid(~ record) +
  scale_x_continuous(breaks =  seq(1961, 2014, 10))
nfa_usa_data1 %>% filter(record == "BiocapTotGHA"|
                           record == "EFConsTotGHA"|
                           record == "EFExportsTotGHA"|
                           record == "EFImportsTotGHA"|
                           record == "EFProdTotGHA") %>%
  ggplot(aes(year,fishing_ground)) + geom_line()+ 
  facet_grid(~ record) +
  scale_x_continuous(breaks =  seq(1961, 2014, 10))
nfa_usa_data1 %>% filter(record == "BiocapTotGHA"|
                           record == "EFConsTotGHA"|
                           record == "EFExportsTotGHA"|
                           record == "EFImportsTotGHA"|
                           record == "EFProdTotGHA") %>%
  ggplot(aes(year,built_up_land)) + geom_line()+ 
  facet_grid(~ record) +
  scale_x_continuous(breaks =  seq(1961, 2014, 10))
nfa_usa_data1 %>% filter(record == "BiocapTotGHA"|
                           record == "EFConsTotGHA"|
                           record == "EFExportsTotGHA"|
                           record == "EFImportsTotGHA"|
                           record == "EFProdTotGHA") %>%
  ggplot(aes(year, carbon)) + geom_line()+ 
  facet_wrap(~ record, 2) +
  scale_x_continuous(breaks =  seq(1961, 2014, 10))
View(nfa_usa_data1)
data1 <- pivot_wider(nfa_usa_data1[c("year", "record", "total")],names_from = record, 
            values_from = total)
ggplot(data1, aes(BiocapTotGHA,EFProdTotGHA)) + geom_line()
cor(data1$BiocapTotGHA, data1$EFProdTotGHA)
##There has been mismatch between capacity an production due to one
##one or more of the types of land usage
##Let us find out which of these have been causing the issue
ggplot(data1, aes(BiocapTotGHA,EFProdTotGHA)) + geom_line() + 
  geom_vline(xintercept = 1.05e+09)
##What is biocapacity? What is biocapacity in geography?
#Biocapacity is therefore the ecosystems' capacity
#to produce biological materials used by people and 
#to absorb waste material generated by humans, 
#under current management schemes and extraction technologies.
ggplot(data1, aes(EFConsTotGHA,BiocapTotGHA)) + geom_line() + 
  geom_hline(yintercept = 1.00e+09) + geom_smooth() + 
  geom_abline(intercept = 9.00e+08)
##For sustainability the rate of growth of consumption should be 
##lesser than the rate of growth of biocapacity. We can see that
##towards the end of the curve which represents the
##recent years the rate of growth of biocapacity has been lesser than the
##rate of growth of consumption which is not good for sustainability
##Let us check which kind of land area is show the lagged 
##growth of biocapacity

nfa_usa_data1 %>%filter(record == "EFConsTotGHA"| record == "BiocapTotGHA") %>%
ggplot(mapping = aes(year, crop_land, colour = record)) + geom_line()

nfa_usa_data1 %>%filter(record == "EFConsTotGHA"| record == "BiocapTotGHA") %>%
  ggplot(mapping = aes(year, grazing_land, colour = record)) + geom_line()

nfa_usa_data1 %>%filter(record == "EFConsTotGHA"| record == "BiocapTotGHA") %>%
  ggplot(mapping = aes(year, forest_land, colour = record)) + geom_line()

nfa_usa_data1 %>%filter(record == "EFConsTotGHA"| record == "BiocapTotGHA") %>%
  ggplot(mapping = aes(year, fishing_ground, colour = record)) + geom_line()

nfa_usa_data1 %>%filter(record == "EFConsTotGHA"| record == "BiocapTotGHA") %>%
  ggplot(mapping = aes(year, built_up_land, colour = record)) + geom_line()

nfa_usa_data1 %>%filter(record == "EFConsTotGHA"| record == "BiocapTotGHA") %>%
  ggplot(mapping = aes(year, carbon)) + geom_smooth() + scale_x_continuous(breaks = seq(1961,2014,10))

##We thus see that consumption has always been over capacity for grazing land
##Let us create a time series data for crop_land
cropland <- pivot_wider(nfa_usa_data1[c("year", "record", "crop_land")],names_from = record, 
                                                          values_from = crop_land)
View(cropland)
dim(cropland)
cropland_tsall <- ts(cropland[c(2:11)], start = 1961, frequency = 1)
cropland_tscons<- ts(cropland$EFConsTotGHA, start = 1961, frequency = 1)
cropland_tsbiocap<- ts(cropland$BiocapTotGHA, start = 1961, frequency = 1)
cropland_tsprod<- ts(cropland$EFProdTotGHA, start = 1961, frequency = 1)
start(cropland_tsbiocap)
end(cropland_tsbiocap)
time(cropland_tsbiocap)
deltat(cropland_tsbiocap)
frequency(cropland_tsbiocap)
cycle(cropland_tsbiocap)
class(cropland_tsbiocap)
plot(cropland_tsbiocap)
ts.plot(cropland_tsall, col = 1:4, xlab = "Year", ylab = "Different parameters",
        main = "Different bio capacity parameters in GHA for America, 1961-2014")
legend("topleft", colnames(cropland_tsall), lty = 1, col = 1:2, bty = "n")
ts.plot(cropland_tsbiocap)
ts.plot(cropland_tscons)
ts.plot(cropland_tsprod)
##Let us examine the trends and patterns of the parameter
##over the years
head(cropland_tsall)
ts.plot(cropland_tsbiocap)
##We can see that the total biocapacity has been growing over years
##the trend is almost linear
##We do not over any seasonality or variance pattern
##To get the differnced time series, we have to take the diff using
##the diff() function
cropland_tsbiocap_diff <- diff(cropland_tsbiocap)
ts.plot(cropland_tsbiocap_diff)
##We can see that the pattern of the differenced time series has variation
##There had been increased variance around 1980 and 1990s
##The variance again decreased over years
##The difference time series for biocapacity is a white noise time
##since we can see no clear pattern. There is variance which
##increases and again decreases and constant mean
##but no clear pattern
##A white noise has constant mean and variance but no clear pattern
##Let us create a white noise model for totalbiocapacity(GHA) for cropland
wn_model_forbiocapdiff <- arima(cropland_tsbiocap_diff, 
                                order = c(0,0,0))
wn_model_forbiocapdiff
mean(cropland_tsbiocap_diff)
var(cropland_tsbiocap_diff)
cropland_tsbiocap_2000 <- cropland_tsbiocap[1:40]
fit <- arima(cropland_tsbiocap, order = c(0,1,0))
##Since the difference of our biocapacity series is a white noise series
##the original series is a random walk series
pred <- predict(fit, n.ahead = 1)
pred$pred
c(
  mean(cropland_tsbiocap[41] - pred$pred), # Mean Error
  sqrt(mean((cropland_tsbiocap[41] - pred$pred)^2)), # Root Mean Squared Error
  mean(abs(cropland_tsbiocap[41] - pred$pred)))
pred
install.packages("forecast")
library(forecast)
forecast(cropland_tsbiocap, model = fit)
cropland_tsbiocap[50] 
#rwf(cropland_tsbiocap, h  = 10)
autoplot(cropland_tsbiocap)
fit1 <- auto.arima(cropland_tsbiocap)
fit1
fit1 %>%
  forecast(h = 14) %>%
  autoplot() + xlab("Year")
##We can forecast the croplands biocapita using autoarima function
##We can repeat the same forecasting for cropland_tscons and cropland_tsprod
autoplot(cropland_tscons)
fit2 <- auto.arima(cropland_tscons)
fit2
fit2 %>%
  forecast(h = 16) %>%
  autoplot() + xlab("Year")
autoplot(cropland_tsprod)
fit3 <- auto.arima(cropland_tscons)
fit3
fit3 %>%
  forecast(h = 16) %>%
  autoplot() + xlab("Year")
autoplot(cropland_tsbiocap)
autoplot(cropland_tscons)
autoplot(cropland_tsprod)
ggAcf(cropland_tsbiocap)
ggAcf(cropland_tscons)
ggAcf(cropland_tsprod)
##all the above ts looks trended by they seem to be non-seasonal or cyclic
##They might be part of long run cycle
##Thus these series should not not be used for long run prediction
##-----------------Analyzing the Nitrogen Use Efficiency-----###
nitro_data <- read.csv("nitrogen-use-efficiency.csv")
str(nitro_data)
dim(nitro_data)
ggplot(nitro_data, aes(Year, nitrogen_efficiency)) + geom_line() + geom_smooth() +
  labs(x = "Years", y = "Nitrogen Use Efficiency",
  title = "Nitrogen Fertilizer Usage efficiency of USA.") + 
  scale_x_continuous(breaks = seq(1961, 2014,10)) + theme_bw()
                                            
a10 <- ts(nitro_data[4], start = c(1961,1), frequency = 1)
# Load the fpp2 package
install.packages("fpp2")
library(fpp2)

# Create plots of the a10 data
autoplot(a10)
Acf(a10) 
Box.test(a10)
#ggseasonplot(a10)
##The nitrogen data is not seasonal
# Produce a polar coordinate season plot for the a10 data
#ggseasonplot(a10, polar = TRUE)
##The nitrogen data do not show any trend seasonality or cyclicity
gglagplot(a10)
ggAcf(a10)
##In the autocorrelation function although we have the first few spikes
##within the blue lines, we do not have most of the spikes
##well within the blue line i.e. the autocorrelation is not
##significantly different than zero
##Thus the series is not a total white nosie series.
##Another test to confirm is the series is or not a white noise
Box.test(a10, lag = 24, fitdf = 0, type = "Ljung")
##Since the p-value is < 0.05 we shall consider this a non-white noise series
fcnitro <- naive(a10, h = 10)
autoplot(fcnitro)
summary(fcnitro)
checkresiduals(fcnitro)
autoplot(residuals(fcnitro))
ggAcf(residuals(fcnitro))
##The above methods show that the mean of the residuals obtained from
##the fcnitro forecast model is approx ~ 0, the ACF has almost all
##values well within the limits and histogram is almost normal
##Also the p-value of the residuals is slightly mroe than 0.05
accuracy(fcnitro)
nitrotrain <- window(a10,  end = 2005)
nitrotest <- window(a10, start = 2006)
fcnaivetrain <- naive(nitrotrain, h = 15)
autoplot(fcnaivetrain) + autolayer(nitrotest, series = "Test data")
accuracy(fcnaivetrain, nitrotest)
mean_fc <- meanf(nitrotrain, h = 10)
accuracy(mean_fc, nitrotest)
checkresiduals(fcnaivetrain)
##The comparison shows that the naive forecast gave a better accuracy
# Compute cross-validated errors for up to 8 steps ahead
e <- tsCV(a10, forecastfunction = naive, h = 8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


##We can use simple exponential estimation for forecasting
fc_ses_nitro1 <- ses(a10, h =10)
summary(fc_ses_nitro1)
autoplot(fc_ses_nitro1)
fc_ses_nitro <- ses(nitrotrain, h = 20)
summary(fc_ses_nitro)
autoplot(fc_ses_nitro)
summary(fc_ses_nitro)
checkresiduals(fc_ses_nitro)
autoplot(residuals(fc_ses_nitro))
ggAcf(residuals(fc_ses_nitro))
fc_ses_nitro %>% forecast(h = 18) %>% autoplot() 
autoplot(fc_ses_nitro) + autolayer(nitrotest, series = "Test data")
accuracy(fc_ses_nitro, nitrotest)
forecastsnue <- forecast(fc_ses_nitro)
forecastnue <- data.frame(forecastsnue)
forecastnue
write.csv(forecastnue, "nueforecasts2.csv")
##All these methods estimate only the average of future values
##From the forcasted values we see that the Nitrogen efficiency
##can be as low as 51%
autoplot(fc_ses_nitro) + autolayer(fitted(fc_ses_nitro))
##Let's compare both the methods for nitrogen data


##Let's also try auto arima forecasting for the nitro_data
autoplot(a10)
ts.plot(a10)
autoplot(log(a10))
fit <- auto.arima(a10, lambda = 0)
summary(fit)
d <- 1
D <- 1
fit %>% forecast(h = 14) %>% autoplot()


##-----------------Analyzing the crops land biocapacity-----###
library(fpp2)

# Create plots of the a10 data
autoplot(cropland_tsbiocap)+ labs(x="years", 
                                 y="cropland Biocapacity", title="Time Series plot of biocapacity")

##The crop lands bio capacity data does not any seasonality but it shows an upward trend
gglagplot(cropland_tsbiocap)
ggAcf(cropland_tsbiocap) + labs(title="Autocorrelation plot of biocapacity")
##In the autocorrelation function although we have the first few spikes
##within the blue lines, we do not have most of the spikes
##well within the blue line i.e. the autocorrelation is not
##significantly different than zero
##Thus the series is not a total white nosie series.
##Another test to confirm is the series is or not a white noise
Box.test(cropland_tsbiocap, lag = 24, fitdf = 0, type = "Ljung")
##Since the p-value is < 0.05 we shall not consider this a non-white noise series
croplandbiocapfc <- naive(cropland_tsbiocap, h = 10)
autoplot(croplandbiocapfc)
summary(croplandbiocapfc)
checkresiduals(croplandbiocapfc)
autoplot(residuals(croplandbiocapfc))
ggAcf(residuals(croplandbiocapfc))
##The above methods show that the mean of the residuals obtained from
##the fcnitro forecast model is approx ~ 0, the ACF has almost all
##values well within the limits and histogram is almost normal
##Also the p-value of the residuals is slightly mroe than 0.05
accuracy(croplandbiocapfc)
biocaptrain <- window(cropland_tsbiocap,  end = 2009)
biocaptest <- window(cropland_tsbiocap, start = 2010)
fcnaivebiocap <- naive(biocaptrain, h = 4)
autoplot(fcnaivebiocap) + autolayer(biocaptest, series = "Test data")
accuracy(fcnaivebiocap, biocaptest)
mean_fc <- meanf(biocaptrain, h = 10)
accuracy(mean_fc, biocaptest)
checkresiduals(fcnaivebiocap)
##The comparison shows that the naive forecast gave a better accuracy
# Compute cross-validated errors for up to 8 steps ahead
e <- tsCV(cropland_tsbiocap, forecastfunction = naive, h = 8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)
mse
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


##We can use simple exponential estimation for forecasting
fc_ses_biocap <- ses(cropland_tsbiocap, h =10)
summary(fc_ses_biocap)
autoplot(fc_ses_biocap)
summary(fc_ses_biocap)
checkresiduals(fc_ses_biocap)
autoplot(residuals(fc_ses_biocap))
ggAcf(residuals(croplandbiocapfc))
##All these methods estimate only the average of future values
##From the forcasted values we see that the Nitrogen efficiency
##can be as low as 51%
autoplot(fc_ses_biocap) + autolayer(fitted(fc_ses_biocap))
##Let's try ses on train and test for biocap data
biocapsesfit <- ses(biocaptrain, h = 15)
autoplot(biocapsesfit) + autolayer(biocaptest, series = "Test data")
accuracy(biocapsesfit, biocaptest)
mean_fc <- meanf(biocaptrain, h = 10)
accuracy(mean_fc, biocaptest)
checkresiduals(biocapsesfit)
forecasts1 <- forecast(biocapsesfit)
forecastdf <- data.frame(forecasts1)
write.csv(forecastdf, "biocapforecasts.csv")
##Let's compare both the methods for nitrogen data
##From the accuracy comparison of both the methods above we see that
##ses model is giving better accuracy for the forecasts of the biocapacity data

##Let's also try auto arima forecasting for the bio cap data
biocaparima <- auto.arima(biocaptrain, lambda = 0)
summary(biocaparima)
d <- 1
D <- 1
biocaparima %>% forecast(h = 14) %>% autoplot() + autolayer(biocaptest, series = "Test data")
autoplot(biocaparima) + autolayer(biocaptest, series = "Test data")
checkresiduals(biocaparima)
##Let's also try ets forecasting for the bio cap data
biocapets <- ets(biocaptrain)
summary(biocapets)
d <- 1
D <- 1
biocapets %>% forecast(h = 14) %>% autoplot() + autolayer(biocaptest, series = "Test data")
checkresiduals(biocapets)
##Let us try applying the holt's method
biocapholt <- holt(biocaptrain, h = 14)
summary(biocapholt)
autoplot(biocapholt)
checkresiduals(biocapholt)
biocapholt %>% forecast(h = 14) %>% autoplot() + autolayer(biocaptest, series = "Test data")
accuracy(biocapsesfit, biocaptest)
  
  
  
  
  
  
  
  
  
  
  
  
  
)
