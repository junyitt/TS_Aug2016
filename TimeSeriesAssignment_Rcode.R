library(fpp)

#Importing Data
dataset <- read.csv("exports.csv", header=T)
attach(dataset)
y <- ts(exports, start=c(1959,4), end=c(1995,3), frequency=4)

#Plotting the actual series
op <- par(mar=c(5, 6, 4, 2) + 0.1)
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  #need to add label x in month, y in units, title

#ACF Plot
acf(y)

#PACF Plot
pacf(y)

#Perform ADF test
adf.test(y)  

#Perform KPSS test
kpss.test(y) 

#Taking the first-order differencing
d.y <- diff(y,1)

#Plot the differenced series
tsdisplay(d.y)

#Perform ADF test
adf.test(d.y)

#Perform KPSS test
kpss.test(d.y)

#Taking the first-order seasonal differencing
ds.y <- diff(d.y, 4)

#Plot the seasonally differenced series
tsdisplay(ds.y)

#Perform ADF test
adf.test(d.y)

#Perform KPSS test
kpss.test(d.y)

# Suggest plausible models
# Model 1: ARIMA(0,1,1)(0,1,1)[4] 
fit1 <- Arima(y, order = c(0,1,1), seasonal = c(0,1,1))

# Model 2: ARIMA(1,1,0)(2,1,0)[4] 
fit2 <- Arima(y, order = c(1,1,0), seasonal = c(2,1,0)) #5 parameters

# Model 3: ARIMA(1,1,1)(2,0,0)[4] with drift
fit3 <- auto.arima(y) #5 parameters


#Fitting the model and estimating the coefficients
summary(fit1) 
summary(fit2)
summary(fit3)

#plot fitted values for Model 1 against actual values
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  

lines(fitted(fit1),col= "red", lty = 2, lwd=2.5)

legend(1983,7000, c("Actual", "Fitted (Model 1)"),
       lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))

#plot fitted values for Model 2 against actual values
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  

lines(fitted(fit2),col= "red", lty = 2, lwd=2.5)

legend(1983,7000, c("Actual", "Fitted (Model 2)"),
       lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))


#plot fitted values for Model 3 against actual values
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  

lines(fitted(fit3),col= "red", lty = 2, lwd=2.5)

legend(1983,7000, c("Actual", "Fitted (Model 3)"),
       lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))


#Examining the residuals for Model 1
fit1_resid <- residuals(fit1)

#Plot the residuals for Model 1
tsdisplay(fit1_resid, main = "Residuals for Model 1")

#Perform Ljung-Box test
Box.test(fit1_resid, lag = 12, type = "Ljung-Box")

#Examining the residuals for Model 2
fit2_resid <- residuals(fit2)

#Plot the residuals for Model 2
tsdisplay(fit2_resid, main = "Residuals for Model 2")

#Perform Ljung-Box test
Box.test(fit2_resid, lag = 12, type = "Ljung-Box")

#Examining the residuals for Model 3
fit3_resid <- residuals(fit3)

#Plot the residuals for Model 3
tsdisplay(fit3_resid, main = "Residuals for Model 3") 

#Perform Ljung-Box test
Box.test(fit3_resid, lag = 12, type = "Ljung-Box")

#Comparing the 3 Models
summary(fit1) # AICc=2083.71, p-value = 0.6351, RMSE 412.2105 
summary(fit2) # AICc=2100.47, p-value = 0.006502, RMSE 437.5281
summary(fit3) # AICc=2160, p-value = 0.5896, RMSE 436.0327

#Forecasting 10 periods into the future
forecast(fit1, h = 10)

#Plot the forecasted values
plot(forecast(fit1, h = 10), main = "Quarterly Australian National Accounts Exports Forecast \nfrom 1959 Q4 to 1998 Q1 \n(Model 1)",
     xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  


