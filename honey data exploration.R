library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(psych)
honey_data <- read.csv("vHoneyNeonic_v02.csv")
str(honey_data)
##Let's first do an EDA of the data,
dim(honey_data)
dim(honey_data %>% drop_na())##SInce it dropped only 8% of the data, it's fine
honey_data1 <- honey_data %>% filter(year > 2003)
colnames(honey_data)

##The numeric variable summary
numvarsumm <- function(x){n = length(x)
average = mean(x, na.rm = TRUE)
nmiss = sum(is.na(x)|x == "")
skewness = skew(x)
s <- sd(x,na.rm=T)
min <- min(x,na.rm=T)
pctl <- quantile(x, na.rm=T, p=c(0.01,0.05,0.1,0.25,0.5,0.75,0.9, 0.95,0.99))
max <- max(x,na.rm=T)
UC <- average+3*s
LC <- average-3*s
return(c(n=n, nmiss = nmiss , skewness = skewness, mean = average, stdev=s,min = min, pctl=pctl, max=max, UC=UC, LC=LC))
}
##The categorical variable summary
catvarssumm <- function(x){
  n = length(x)
  nmiss <- sum(is.na(x)|x=="")
  return(c(n =n, nmiss = nmiss))
}
num_vars <- c("numcol", "yieldpercol", "totalprod", "stocks",
              "priceperlb", "prodvalue", "nCLOTHIANIDIN","nIMIDACLOPRID",
              "nTHIAMETHOXAM","nACETAMIPRID", "nTHIACLOPRID", "nAllNeonic")
char_vars <- c("state", "StateName", "Region")
honeynumsumm <- t(apply(honey_data[num_vars], 2, numvarsumm))
#honeysumm1 <- t(apply(honey_data1[num_vars], 2, numvarsumm))
#View(honeysumm1)
View(honeynumsumm)
honetcatsumm <- t(apply(honey_data[char_vars], 2, catvarssumm))
View(honetcatsumm)
write.csv(honeynumsumm, "Numsum1.csv")
#write.csv(honetcatsumm, "catvarsumm.csv")
##In the next step we treat the outliers in numeric continuous and categorical variables and then do the missing value treatment.
uppercaptreat <- function(x) { x <- replace(x, which(x > quantile(x, probs = c(0.99), na.rm = TRUE)), quantile(x, probs = c(0.99), na.rm = TRUE)) }
lowercaptreat <- function(x) { x <- replace(x, which(x < quantile(x, probs = c(0.01), na.rm = TRUE)), quantile(x, probs = c(0.01), na.rm = TRUE)) }
honeycont1 <- apply(honey_data[num_vars], 2, FUN = uppercaptreat)
honeycont1 <- data.frame(honeycont1)
View(honeycont1)
honeycont2 <- apply(honeycont1, 2, lowercaptreat)
honeycont2 <- data.frame(honeycont2)
honeynumsumm1 <- t(apply(honeycont2[num_vars], 2, numvarsumm))
View(honeynumsumm1)
colnames(honeycont2)
ggplot(honeycont2, aes(x = nTHIACLOPRID)) + geom_density()
##Treating the missing numeric data by imputing with the mean
missimputation <- function(x){ x <- replace (x, which(is.na(x) == TRUE | x == "#NULL!"), median(x, na.rm = TRUE))}
honeycont2 <- apply(honeycont2,2, missimputation)
honeycont2 <- data.frame(honeycont2)
#View(honeycont2)
honeynumsumm3 <- t(apply(honeycont2[num_vars], 2, FUN = numvarsumm))
View(honeynumsumm3)
write.csv(honeynumsumm3, "honeynumsumm4.csv")
colnames(honeycont2)
ggplot(honeycont2, aes(x = nTHIACLOPRID)) + geom_density()
##Thus our data is treated now and is ready for further analysis
##Let us combine the character variables with the treated numeric variables
honey_final <- cbind(honeycont2, honey_data[char_vars])
str(honey_final)
dim(honey_final)
honey_final['year'] <- honey_data$year
head(honey_final %>% filter(year < 2003))
write.csv(honey_final, "honey_final1.csv")
##Let us see how total production is associated with the 
##number of colonies. 
corr <- round(cor(honey_final[num_vars]), 2)
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
ggplot(honey_final, aes(numcol, yieldpercol)) + geom_point()
View(corr)
write.csv(corr, "correlation.csv")
View(honey_final)
##Our key question here is to understand if honey production is 
##effected by different factors and which factor induces higher production
##We can see how honey production varies across different states
levels(as.factor(honey_final$Region))
summary(aov(totalprod ~ StateName, data = honey_final))
summary(aov(totalprod ~ Region, data = honey_final))
##Thus we see that the total production significantly varies across 
##states and regions
##Next we divide the dataset into testing an validation
set.seed(123)
trainind <- sample(1:nrow(honey_final),size = floor(0.70*nrow(honey_final)))
trainlog <- honey_final[trainind,]
testlog <- honey_final[-trainind,]
View(trainlog)
fit1 <- lm(totalprod ~ numcol  +   nIMIDACLOPRID + nACETAMIPRID +
             nAllNeonic  +  California + Colorado +
             Florida + Georgia +  Louisiana + Michigan +
             Minnesota + Montana + Nebraska + New_York +
             North_Dakota + Oregon + 
             Texas + Utah + Washington + Wisconsin +
             Wyoming, data = trainlog )
##We check the summary of fit1 model to get the significant dependent variables and the adjusted-R square
summary(fit1)
library(MASS)
step(fit1, direction = "both" )
##First we check the validation of the model on train dataset
trainloga <- cbind(trainlog, predhoneyprod = predict(fit1, newdata = trainlog))
##Decile analysis for training dataset predicted values
declocations <- quantile(trainloga$predhoneyprod, probs = seq(0.1, 0.9, by = 0.1 ))
##categorising the predtotalspent into decile ranks using the declocation 
trainloga$decilerank <- findInterval(trainloga$predhoneyprod,c(-Inf, declocations, Inf))
View(trainloga)
require(sqldf)
table <- sqldf("select avg(totalprod) as average_totalprod,
               avg(predhoneyprod) as average_honeyprod,
               decilerank from trainloga GROUP BY decilerank")
View(table)
##We also calculate the mape, corr and min_max_accuracy by implementitng the model on train dataset
actuals_preds <- data.frame(cbind(actuals=trainloga$totalprod, predicted= trainloga$predhoneyprod))  
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)
correlation_accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals))/actuals_preds$actuals)
mape
min_max_accuracy
correlation_accuracy

##Next we validate the model on the test dataset
testloga <- cbind(testlog, predhoneyprod = predict(fit1, newdata = testlog))

##Decile analysis for test dataset predicted values
declocations1 <- quantile(testloga$predhoneyprod, probs = seq(0.1, 0.9, by = 0.1 ))

##categorising the predtotalspent into decile ranks using the declocation 
testloga$decilerank <- findInterval(testloga$predhoneyprod,c(-Inf, declocations1, Inf))
View(testloga)

##Next we present the average of logtotalspent and totalspent against predlogtoalspent and predtotalspent in a table in groups of different deciles using sql
require(sqldf)
table2 <- sqldf("select avg(totalprod) as average_totalprod, 
                         avg(predhoneyprod) as average_honeyprod
                          from testloga GROUP BY decilerank ")
View(table2)

##We also calculate the mape, corr and min_max_accuracy by implementitng the model on test dataset
actuals_preds <- data.frame(cbind(actuals=testloga$totalprod, predicted= testloga$predhoneyprod))  
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals))/actuals_preds$actuals)
mape

min_max_accuracy
correlation_accuracy
ggplot(testloga, aes(totalprod, predhoneyprod)) + geom_point() +
  geom_abline(color = "darkblue") + geom_jitter()
##Thus we have got the relationship between the total production
##of honey with the number of colonies
##We see that total production of honey is highly positively influenced
##by in crease in the number of colonies
## We also get to know that the use of the 
##The amount in kg of CLOTHIANIDIN applied increased honey production
##On the amount in kg of THIAMETHOXAM applied is less influential
##for honey production. The amount in kg of THIACLOPRID applied
##does not influence honey production.
fit2 <- lm(yieldpercol ~ numcol + nIMIDACLOPRID + nACETAMIPRID, data = trainlog )
summary(fit2)
honey_final$Arizona <- ifelse(honey_final$StateName == "Arizona", 1,0)
honey_final$Arkansas <- ifelse(honey_final$StateName == "Arkansas", 1,0)
honey_final$California <- ifelse(honey_final$StateName == "california", 1,0)
honey_final$Colorado <- ifelse(honey_final$StateName == "Colorado", 1,0)
honey_final$Florida <- ifelse(honey_final$StateName == "Florida", 1,0)
honey_final$Georgia <- ifelse(honey_final$StateName == "Georgia", 1,0)
honey_final$Idaho <- ifelse(honey_final$StateName == "Idaho", 1,0)
honey_final$Louisiana <- ifelse(honey_final$StateName == "Louisiana", 1,0)
honey_final$Michigan <- ifelse(honey_final$StateName == "Michigan", 1,0)
honey_final$Minnesota <- ifelse(honey_final$StateName == "Minnesota", 1,0)
honey_final$Montana <- ifelse(honey_final$StateName == "Montana", 1,0)
honey_final$Nebraska <- ifelse(honey_final$StateName == "Nebraska", 1,0)
honey_final$New_York <- ifelse(honey_final$StateName == "New York", 1,0)
honey_final$North_Dakota <- ifelse(honey_final$StateName == "North Dakota", 1,0)
honey_final$Oregon <- ifelse(honey_final$StateName == "Oregon", 1,0)
honey_final$South_Dakota <- ifelse(honey_final$StateName == "South_Dakota", 1,0)
honey_final$Texas <- ifelse(honey_final$StateName == "Texas", 1,0)
honey_final$Utah <- ifelse(honey_final$StateName == "Utah", 1,0)
honey_final$Washington <- ifelse(honey_final$StateName == "Washington", 1,0)
honey_final$Wisconsin <- ifelse(honey_final$StateName == "Wisconsin", 1,0)
honey_final$Wyoming <- ifelse(honey_final$StateName == "Wyoming", 1,0)
set.seed(123)
trainind <- sample(1:nrow(honey_final),size = floor(0.70*nrow(honey_final)))
trainlog <- honey_final[trainind,]
testlog <- honey_final[-trainind,]
View(trainlog)
fit2 <- lm(numcol ~ Arizona +  California + Colorado +
            Florida + Georgia +  Louisiana + Michigan +
             Minnesota + Montana + Nebraska + New_York +
             North_Dakota + Oregon + South_Dakota +
             Texas + Utah + Washington + Wisconsin +
             Wyoming, data = trainlog)
summary(fit2)
fit2 <- lm(totalprod ~ numcol + nIMIDACLOPRID +
             nACETAMIPRID + Florida + Michigan + Minnesota +
             Montana + North_Dakota + Texas ,  data = trainlog)
summary(fit2)
fit3 <- lm(totalprod ~ numcol + nCLOTHIANIDIN +
              nTHIACLOPRID + Florida + Michigan + Minnesota +
              Montana + North_Dakota + Texas ,  data = trainlog)
summary(fit3)
##First we check the validation of the model on train dataset
##The coefficients of one fertilizer is changing sign because of high collinearity
trainlogloc <- cbind(trainlog, predhoneyprod = predict(fit2, newdata = trainlog))
##Decile analysis for training dataset predicted values
declocations <- quantile(trainlogloc$predhoneyprod, probs = seq(0.1, 0.9, by = 0.1 ))
##categorising the predtotalspent into decile ranks using the declocation 
trainlogloc$decilerank <- findInterval(trainlogloc$predhoneyprod,c(-Inf, declocations, Inf))
View(trainlogloc)
require(sqldf)
table <- sqldf("select avg(totalprod) as average_totalprod,
               avg(predhoneyprod) as average_honeyprod,
               decilerank from trainlogloc GROUP BY decilerank")
View(table)
##We also calculate the mape, corr and min_max_accuracy by implementitng the model on train dataset
actuals_preds <- data.frame(cbind(actuals=trainlogloc$totalprod, predicted= trainlogloc$predhoneyprod))  
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)
correlation_accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals))/actuals_preds$actuals)
mape
min_max_accuracy
correlation_accuracy
##Next we validate the model on the test dataset
testlogloc <- cbind(testlog, predhoneyprod = predict(fit2, newdata = testlog))

##Decile analysis for test dataset predicted values
declocations1 <- quantile(testlogloc$predhoneyprod, probs = seq(0.1, 0.9, by = 0.1 ))

##categorising the predtotalspent into decile ranks using the declocation 
testlogloc$decilerank <- findInterval(testlogloc$predhoneyprod,c(-Inf, declocations1, Inf))
View(testlogloc)

##Next we present the average of logtotalspent and totalspent against predlogtoalspent and predtotalspent in a table in groups of different deciles using sql
require(sqldf)
table2 <- sqldf("select avg(totalprod) as average_totalprod, 
                         avg(predhoneyprod) as average_honeyprod
                          from testlogloc GROUP BY decilerank ")
View(table2)

##We also calculate the mape, corr and min_max_accuracy by implementitng the model on test dataset
actuals_preds <- data.frame(cbind(actuals=testlogloc$totalprod, predicted= testloga$predhoneyprod))  
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals))/actuals_preds$actuals)
mape
min_max_accuracy
correlation_accuracy
##Looking at actuals and prediction with plot
ggplot(testlogloc, aes(totalprod, predhoneyprod)) + geom_point() +
  geom_abline(color = "darkblue") + geom_jitter()

##Let us create a column which includes the quantile bracket of the 
##number of colonies
numcolquantlocs <- quantile(honey_final$numcol, probs = seq(0, 1, by = 0.25 ))
honey_final$numcolquantile <- findInterval(honey_final$numcol,c(-Inf, numcolquantlocs , Inf))
View(honey_final)
numcolquantlocs
max(honey_final$numcol)
quantile(honey_final$numcol)

install.packages("ranger")
library(ranger)
seed = set.seed(123)
(numcolmodel <- ranger(numcol ~ nCLOTHIANIDIN +
                        nTHIACLOPRID + Florida + Michigan + Minnesota +
                        Montana + North_Dakota + Texas, data = honey_final,
                      num.trees = 500, respect.unordered.factors = "order",
                      seed = seed))
##We can predict the number of colonies using the random forest
## decision tree model

(numcolmodel <- ranger( numcol ~ totalprod + nCLOTHIANIDIN +
                         nTHIACLOPRID + Florida + Michigan + Minnesota +
                         Montana + North_Dakota + Texas, data = trainlogloc,
                       num.trees = 500, respect.unordered.factors = "order",
                       seed = seed))
trainlogloc$randomnumcolpred <- predict(numcolmodel, trainlogloc)$predictions
  trainlogloc %>% mutate(residual = randomnumcolpred - numcol) %>%
    summarise(rmse = sqrt(mean(residual^2)))
testlogloc$randomnumcolpred <- predict(numcolmodel, testlogloc)$predictions
  testlogloc %>% mutate(residual = randomnumcolpred - numcol) %>%
    summarise(rmse = sqrt(mean(residual^2)))
honey_final$randomnumcolpred <- predict(numcolmodel, honey_final)$predictions
ggplot(honey_final, aes(x = randomnumcolpred, y = numcol)) + 
    geom_point() + 
    geom_abline()  
##We can predict the total production using the random forest
## decision tree model

(totalprodrandommodel <- ranger( totalprod ~ numcol + nCLOTHIANIDIN +
                          nTHIACLOPRID + Florida + Michigan + Minnesota +
                          Montana + North_Dakota + Texas, data = trainlogloc,
                        num.trees = 500, respect.unordered.factors = "order",
                        seed = seed))
trainlogloc$randomtotprod <- predict(totalprodrandommodel, trainlogloc)$predictions
trainlogloc %>% mutate(residual = randomtotprod - totalprod) %>%
  summarise(rmse = sqrt(mean(residual^2)))
testlogloc$randomtotprod <- predict(totalprodrandommodel, testlogloc)$predictions
testlogloc %>% mutate(residual = randomtotprod - totalprod) %>%
  summarise(rmse = sqrt(mean(residual^2)))
honey_final$randomtotprod <- predict(totalprodrandommodel, honey_final)$predictions
ggplot(honey_final, aes(x = randomtotprod, y = totalprod)) + 
  geom_point() + 
  geom_abline()  
##One-hot-encoding
install.packages("vtreat")
(outcome <- "numcol")
(vars <- c("StateName"))
library(vtreat)
library(magrittr)
treatplan <- designTreatmentsZ(honey_final, vars, verbose = FALSE)

# Get the "clean" and "lev" variables from the scoreFrame
(newvars <- treatplan %>%
    use_series(scoreFrame) %>%               
    filter(code %in% c("clean", "lev")) %>%  # get the variables you care about
    use_series(varName))                     # get the varName column

# Prepare the training data
honey.treat <- prepare(treatplan, trainlogloc,  varRestriction = newvars)

honey.test <- prepare(treatplan, testlogloc, varRestriction = newvars)

# Call str() on the treated data
str(honey.treat) 
str(honey.test)
install.packages("xgboost")
library(xgboost)
# Run xgb.cv
cv <- xgb.cv(data = as.matrix(honey.treat), 
             label = trainlogloc$numcol,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 5,
             early_stopping_rounds = 10,
             verbose = 0   # silent
)

# Get the evaluation log
elog <- cv$evaluation_log
elog %>% summarize(ntrees.train = which.min(train_rmse_mean),
                   ntrees.test = which.min(test_rmse_mean))

#Using reg:squarederror
cv <- xgb.cv(data = as.matrix(honey.treat), 
             label = trainlogloc$numcol,
             nrounds = 100,
             nfold = 5,
             objective = "reg:squarederror",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0   # silent
)
# Get the evaluation log
elog <- cv$evaluation_log
elog %>% summarize(ntrees.train = which.min(train_rmse_mean),
                   ntrees.test = which.min(test_rmse_mean))
honey_model_xgb <- xgboost(data = as.matrix(honey.treat), 
                          label = trainlogloc$numcol,  
                          nrounds = 100,      
                          objective = "reg:squarederror", 
                          eta = 0.3,
                          max_depth = 6,
                          verbose = 0  # silent
)
testlogloc$pred_xgb <- predict(honey_model_xgb, as.matrix(honey.test))
ggplot(testlogloc, aes(x = pred_xgb, y = numcol)) + 
  geom_point() + 
  geom_abline()
testlogloc %>%
  mutate(residuals = numcol - pred_xgb) %>%
  summarize(rmse = sqrt(mean(residuals^2)))

testlogloc %>% 
  gather(key = valuetype, value = value, numcol, pred_xgb) %>%
  filter(year > 2000) %>% # first two weeks
  ggplot(aes(x = totalprod, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("Total Prod", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted number of colonies, Gradient Boosting model")
summary(aov(numcol ~ StateName, data = honey_final))
install.packages("DiagrammeR")
library(DiagrammeR)
xgb.plot.tree(model = honey_model_xgb, trees = 1, feature_names = colnames(trainlogloc))

##Let's get the latitudes and longitudes for the US States
states_loc <- read.csv("chipotle_stores.csv")
str(states_loc)
str(honey_final)
datawidloc <- inner_join(honey_final, states_loc, by = "StateName")
str(datawidloc)
UnitedStatesHoney <- datawidloc[,c("totalprod", "StateName", 
                                   "latitude", "longitude")]
str(UnitedStatesHoney)
UnitedStatesHoney <- UnitedStatesHoney[order(UnitedStatesHoney$StateName)]
ggplot(UnitedStatesHoney, aes(longitude, latitude,   fill=quantilerank)) +
  geom_polygon(color="grey") + theme_void() + coord_map()

quantilelocations <- quantile(UnitedStatesHoney$totalprod, probs = seq(0, 1, by = 0.25 ))
UnitedStatesHoney$quantilerank <- findInterval(UnitedStatesHoney$totalprod,c(-Inf, quantilelocations, Inf))
View(UnitedStatesHoney)

ggplot(states_loc, aes(latitude, longitude)) + geom_polygon(color = "grey") +
  theme_void() + coord_map()
install.packages("sf")
library(sf)
philly_crimes_sf <-  st_read("chipotle_stores.csv", quiet = TRUE)
plot(philly_crimes_sf)
ggplot(honey_final, aes(totalproduction))
levels(as.factor(honey_final$StateNames))

honey_final


###----------------sugar import and export-------------------###

sugar_us <- read.csv("sugar_import and export.csv")
str(sugar_us)
dim(sugar_us)
sugar_us <- sugar_us[,-c(9,10,11,12,13,14,15)]
sugar_us$Year = as.factor(sugar_us$ ï..Year)
sugar_us1 <- sugar_us %>% filter( Year %in% c("2010/11","2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19"))
View(sugar_us1)
sugar_us$Total.sugar.production = as.integer(sugar_us$Total.sugar.production)
sugar_us$Total.Sugar.Imports = as.numeric(sugar_us$Total.Sugar.Imports)
sugar_us$Cane.Sugar.Production = as.numeric(sugar_us$Cane.Sugar.Production)
sugar_us$Honey.Imported = as.numeric(sugar_us$Honey.Imported)
sugar_us$Honey.Imported = (sugar_us$Honey.Imported)*1000
View(sugar_us)
View(sugar_us1)
ggplot(sugar_us1, aes(Total.sugar.production, Year)) + 
  geom_col() + coord_flip() + 
  labs(x = "grocery departments", y = "count of orders", title = "Volume of orders by grocery deptments" ) + 
  geom_hline( aes(yintercept = 60000) ) + theme_bw()
sugar_long1 <- sugar_us1 %>% pivot_longer(c("Total.sugar.production",
                                            "Total.Sugar.Imports",
                                            "Cane.Sugar.Production",
                                            "Honey.Imported"), names_to = "KPI",values_to = 
                                            "Volume in metric ton")
View(sugar_long1)
dim(sugar_long1)
sugar_long1 <- sugar_long1[,c(5,6,7)]
sugar_long1$Volume <- sugar_long1$`Volume in metric ton`
ggplot(sugar_long1, aes(Year, Volume, fill = KPI, position = "dodge")) + geom_col()


sug <- sugar_long1 %>% filter(KPI %in% c("Total.Sugar.Imports", "Honey.Imported")) 
View(sug)
ggplot(sug) +
  geom_col(aes(Volume, Year, fill = KPI)) + labs(x = "Volume(in metric tons)",
                                                 y = "Year",
                                                 title = "Sugar Imports to USA")









































































