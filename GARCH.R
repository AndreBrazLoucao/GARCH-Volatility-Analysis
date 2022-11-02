#Install packages
install.packages("dplyr") 
install.packages("tidyverse") 
install.packages("rugarch") 
install.packages("moments") 
install.packages("forecast") 
install.packages("ggpubr")
install.packages("zoo") 
install.packages("xts")
install.packages("aTSA")
install.packages("tseries") 
install.packages("car") 
#Load packages
library(dplyr)
library(tidyverse)
library(rugarch)
library(moments)
library(forecast)
library(ggpubr)
library(zoo)
library(xts)
library(aTSA)
library(tseries)
library(car)

#Read Data
DAXraw <- read.csv("DAXraw.csv")

#Transform data to get relevant vlaues
DAXraw$Date <- as.Date(DAXraw$Date)

DAXraw$Adj.Close <- as.numeric(DAXraw$Adj.Close)

DAXclean <- DAXraw[!DAXraw$Open=="null", ]

DAXclean <- DAXclean %>% select(1,6)

#Form return series 
Returns <- diff(DAXclean$Adj.Close, lag=1)

LogReturns <- diff(log(DAXclean$Adj.Close), lag=1)
DAXclean <- tail(DAXclean, -1)

#?????
#sqReturns <- Returns^2
#sqLogReturns <- LogReturns^2
#absLogReturns <- abs(LogReturns)
#?????

#Adding return measures to data
DAXclean$Returns  <- Returns

DAXclean$LogReturns <- LogReturns

DAXclean$sqLogReturns <- sqLogReturns

DAXclean$sqReturns <- sqReturns

DAXclean$absLogReturns <- absLogReturns

DAXts <- xts(DAXclean$LogReturns, order.by =  DAXclean$Date, frequency = 260)

##Splitting the data into Training and Test data
train <- head(DAXts, round(length(DAXts) * 0.8))
rest <- length(DAXts) - length(train)
test <- tail(DAXts, rest)

#Data Frames with different return measures split into training and test data sets
train_df <- DAXclean[1:length(train),]
test_df <- DAXclean[(length(train)+1):length(DAXts),]


##Basic Measures of Data
skewness(train)
kurtosis(train)

#Decomposition of Log Returns

#Test for normal  distribution (used to justify the additive model)
ggdensity(train, 
          main = "Density plot of Log Returns",
          xlab = "Log Return")

#QQ-Plot of train data
ggqqplot(train)

#Decomposition of time series 
DAXdecomp <- ts(train_df$LogReturns, frequency = 260)

plot(decompose(DAXdecomp)



#Modeling the mean equation
#AIC as information criterion to minimize
auto.arima(train, ic = "aic")
arma21 <- arima(train, order= c(1,0,0)) 

#BIC as information criterion to minimize
auto.arima(train, ic = "bic")
arma00 <- arima(train, order= c(0,0,0)) 

#Test for ARCH effects
arch.test(arma00)


##GARCH(1,1) Model Specifications
#Choosing the correct mean equation
#Specifying and fitting the model with an ARMA(1,0) mean equation
GarchAIC <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(1, 0)),
                            distribution.model = "norm")

GarchAIC_fit <- ugarchfit(spec= GarchAIC,
                               data = train)


#Specifying and fitting the model with an ARMA(1,1) mean equation
GarchBIC <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0)),
                            distribution.model = "norm")

GarchBIC_fit <- ugarchfit(spec= GarchBIC,
                               data = train)

#Set up a data frame to compare IC
MeanEq_Eval <- data.frame(infocriteria(GarchAIC_fit),
                          infocriteria(GarchBIC_fit))

names(MeanEq_Eval)[1] <- "ARMA(1,0) / AIC"
names(MeanEq_Eval)[2] <- "ARMA(0,0) / BIC"


#Choosing distribution of Innovations
#Specifying models with different distributions and fitting them to the train data 
Garch_std <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(0, 0)),
                                distribution.model = "std")

Garch_std_fit <- ugarchfit(spec= Garch_std,
                                   data = train)


Garch_sstd <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                                 mean.model = list(armaOrder = c(0, 0)),
                                 distribution.model = "sstd")

Garch_sstd_fit <- ugarchfit(spec= Garch_sstd,
                                     data = train)


Garch_ged <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0)),
                            distribution.model = "ged")

Garch_ged_fit <- ugarchfit(spec= Garch_ged,
                            data = train)


Garch_sged <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                                 mean.model = list(armaOrder = c(0, 0)),
                                 distribution.model = "sged")

Garch_sged_fit <- ugarchfit(spec= Garch_sged,
                                     data = train)


#Creating data frame for Evaluating IC of models with different distributions
EvaluationDist <-  data.frame(infocriteria(Garch_std_fit),
                          infocriteria(Garch_sstd_fit),
                          infocriteria(Garch_ged_fit),
                          infocriteria(Garch_sged_fit))
names(EvaluationDist)[1] <- "std"
names(EvaluationDist)[2] <- "sstd"
names(EvaluationDist)[3] <- "ged"
names(EvaluationDist)[4] <- "sged"


#Setting up different higher order GARCH models to evaluate
Garch21 <- ugarchspec(variance.model = list(garchOrder = c(2, 1)),
                                 mean.model = list(armaOrder = c(0, 0)),
                                 distribution.model = "sged")

Garch21_fit <- ugarchfit(spec= Garch21,
                         data = train)


Garch12 <- ugarchspec(variance.model = list(garchOrder = c(1, 2)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "sged")

Garch12_fit <- ugarchfit(spec= Garch12,
                         data = train)


Garch22 <- ugarchspec(variance.model = list(garchOrder = c(2, 2)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "sged")

Garch22_fit <- ugarchfit(spec= Garch22,
                         data = train)


Garch31 <- ugarchspec(variance.model = list(garchOrder = c(3, 1)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "sged")

Garch31_fit <- ugarchfit(spec= Garch31,
                         data = train)

Garch13 <- ugarchspec(variance.model = list(garchOrder = c(1, 3)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "sged")

Garch13_fit <- ugarchfit(spec= Garch13,
                         data = train)


Garch23 <- ugarchspec(variance.model = list(garchOrder = c(2, 3)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "sged")

Garch23_fit <- ugarchfit(spec= Garch23,
                         data = train)


Garch32 <- ugarchspec(variance.model = list(garchOrder = c(3, 2)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "sged")

Garch32_fit <- ugarchfit(spec= Garch32,
                         data = train)



Garch33 <- ugarchspec(variance.model = list(garchOrder = c(3, 3)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "sged")

Garch33_fit <- ugarchfit(spec= Garch33,
                         data = train)

#Creating data frame for evaluation based on IC
EvaluationHO <-  data.frame(infocriteria(Garch21_fit),
                          infocriteria(Garch12_fit),
                          infocriteria(Garch22_fit),
                          infocriteria(Garch31_fit),
                          infocriteria(Garch13_fit),
                          infocriteria(Garch23_fit),
                          infocriteria(Garch32_fit),
                          infocriteria(Garch33_fit))

names(EvaluationHO)[1] <- "GARCH(2,1)"
names(EvaluationHO)[2] <- "GARCH(1,2)"
names(EvaluationHO)[3] <- "GARCH(2,2)"
names(EvaluationHO)[4] <- "GARCH(3,1)"
names(EvaluationHO)[5] <- "GARCH(1,3)"
names(EvaluationHO)[6] <- "GARCH(2,3)"
names(EvaluationHO)[7] <- "GARCH(3,2)"
names(EvaluationHO)[8] <- "GARCH(3,3)"



#Forecasting with the GARCH(1,1) model
RollG11  <- ugarchroll(Garch_sged,
                              data = DAXts,
                              n.ahead = 1,
                              n.start = 2025,
                              refit.window =  "rolling",
                              refit.every = 5,
                              window.size = 260) #REchtfertigung : VaR Standards


#Extracting forecasted volatility
G11sigma <- (RollG11@forecast$density$Sigma)^2


#Forecasting with the GARCH(2,1) model
RollG21  <- ugarchroll(Garch21,
                       data = DAXts,
                       n.ahead = 1,
                       n.start = 2025,
                       refit.window =  "rolling",
                       refit.every = 5,
                       window.size = 260)

#Extracting forecasted volatility
G21sigma <- (RollG21@forecast$density$Sigma)^2


##Evaluation 
#Mincer Zarnowitz Regression
#Setting up the MZ regression (GARCH(1,1))
G11mz = lm(test_df$sqLogReturns ~ G11sigma)

#Linear hypothesis test for a biased forecast
linearHypothesis(G11mz, c("(Intercept) = 0", "G11sigma = 1")) 

#Summary in order to get R^2
summary(G11mz)

#Setting up the MZ regression (GARCH(2,1))
G21mz = lm(test_df$sqLogReturns ~ G21sigma)

#Linear hypothesis test for a biased forecast
linearHypothesis(G21mz, c("(Intercept) = 0", "G21sigma = 1"))

#Summary in order to get R^2
summary(G21mz)

#Calculating Mean squared error of the models
mean((test_df$sqLogReturns - G11sigma)^2)

mean((test_df$sqLogReturns - G21sigma)^2)


#######  PLOTS   ##########
#plot of the Logarithmic returns
plot(DAXclean$Date, DAXclean$LogReturns, 
     ylab = "Percent", 
     xlab = "Time",
     main = "Logarithmic Returns",
     type="l", 
     col = "steelblue", 
     lwd = 1)
abline(0, 0)

