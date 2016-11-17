library(fpp)
setwd("C:/Users/User/Google Drive/__Project_Degree 2015-2017/05_MAT3014 (Time Series)/Dataset")

### Exports ###
dataset <- read.csv("exports.csv", header=T)
attach(dataset)
y <- ts(exports, start=c(1959,4), end=c(1995,3), frequency=4)


#1) Brief introduciton
### talk about data, objective, forecast


# 2. Model Specification
# a) Examine time plot of the time series.
op <- par(mar=c(5, 6, 4, 2) + 0.1)
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  #need to add label x in month, y in units, title

###Trend, seasonal, cyclical, irregular

# b) Perform correlogram...
acf(y)

###Comment - dies down slowly, not stationary

pacf(y)

### cutoff at lag 1, so "come in pair with acf" -> implies not stationary


# c) perform test
adf.test(y)  
### p-value  > 0.05, do not reject h0, not stationary
kpss.test(y) 
### p-value < 0.05, reject H0, not stationary

### not stationary, therefore requires transformation and differencing
### perform log transformation because the series is trending

tsdisplay(y) 
###acf and pacf indicates not stationary
adf.test(y); kpss.test(y)
### adf.test and kpss.test shows not stationary

###we take first differencing

d.y <- diff(y,1)
tsdisplay(d.y)
### acf spikes are large and does not decay, hence not stationary
adf.test(d.y); kpss.test(d.y)
### adf test and kpss test shows not stationary
### because we are using quarterly data, ??seasonalities (which month?), so we take seasonal differencing

ds.y <- diff(d.y, 4)
tsdisplay(ds.y)
### most of the acf and "pacf??" spikes are within the band and decays, appears covariance stationary
adf.test(ds.y); kpss.test(ds.y)
### adf test and kpss test shows stationary


# d) suggest plausible models

# ### give reasoning on selection of model
# ### ARIMA(0,1,0)(0,1,1)4 #assume pacf dies down, look at acf, significant spike at lag 4 which is the first seasonal spike P = 1 #no significant spike at lag 1 to lag 3 -> p = 0
fit1 <- Arima(y, order = c(0,1,1), seasonal = c(0,1,1)) #4 parameters #MA
# 
# ### ARIMA(0,1,0)(2,1,0)4 #assume acf dies down, look at pacf, significant spike at lag 4 and 8, which is the first and 2nd seasonal spikes...
fit2 <- Arima(y, order = c(1,1,0), seasonal = c(2,1,0)) #5 parameters
# 
# ###autoarima > ARIMA(1,1,1)(2,0,0)[4] with drift
fit3 <- auto.arima(y) #5 parameters


# 3) a) estimate model, examine coefficient
summary(fit1) ### comment on coefficients #coefficients significant or not, |coeff/s.e.| > 2
summary(fit2)
summary(fit3)

# b) examine fitted values against actual value

###FIT 1###
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  

lines(fitted(fit1),col= "red", lty = 2, lwd=2.5)

legend(1983,7000, c("Actual", "Fitted (Model 1)"),
       lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))

###FIT 2###
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  

lines(fitted(fit2),col= "red", lty = 2, lwd=2.5)

legend(1983,7000, c("Actual", "Fitted (Model 2)"),
       lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))


###FIT 3###
plot.ts(y, main = "Quarterly Australian National Accounts Exports \nfrom 1959 Q4 to 1995 Q3",
        xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  

lines(fitted(fit3),col= "red", lty = 2, lwd=2.5)

legend(1983,7000, c("Actual", "Fitted (Model 3)"),
       lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))

# ### The fitted line follows the observed series closely.

##############Q4###########################
# 4) a) 
fit1_resid <- residuals(fit1)
tsdisplay(fit1_resid, main = "Residuals for Model 1") ### a few significant spikes, may not be white noise
m <- sqrt(144) ### parameter m approximately sqrt(T), T = 144
Box.test(fit1_resid, lag = 12, type = "Ljung-Box")
### p-value > 0.05, H0 not rejected, not white noise #p-value = 0.6351

fit2_resid <- residuals(fit2)
tsdisplay(fit2_resid, main = "Residuals for Model 2") ### most acf spikes within standard error band, may not be white noise
m <- sqrt(144) ### parameter m approximately sqrt(T), T = 144
Box.test(fit2_resid, lag = 12, type = "Ljung-Box")
### p-value > 0.05, H0 not rejected, white noise #p-value = 0.006502

fit3_resid <- residuals(fit3)
tsdisplay(fit3_resid, main = "Residuals for Model 3") ### all acf spikes within standard error band, may not be white noise
m <- sqrt(144) ### parameter m approximately sqrt(T), T = 144
Box.test(fit3_resid, lag = 12, type = "Ljung-Box")
### p-value > 0.05, H0 not rejected, follows white noise up to lag 12  #p-value = 0.5896


# 5) final model
summary(fit1) ### AICc=2083.71, p-value = 0.6351, RMSE 412.2105 coefficients significant?
summary(fit2) ### AICc=2100., p-value = 0.006502, RMSE 437.5281
summary(fit3) ### AICc=2160, p-value = 0.5896, RMSE 436.0327

###choose first model, because lowest AICc, and highest p-value for Ljung box test, ...follows white noise up to lag12 
# ma1     sma1
# -0.2284  -0.8013
# s.e.   0.0943   0.0536

# Arima(0,1,1)(0,1,1)[4]
# write model with coefficient values
# (1-L)(1-L^4)yt = (1+qL)(1+QL4)et


# 6) forecast
forecast(fit1, h = 10)
plot(forecast(fit1, h = 10), main = "Quarterly Australian National Accounts Exports Forecast \nfrom 1959 Q4 to 1998 Q1 \n(Model 1)",
     xlab = "Time (quarter)", ylab = "Exports (in millions of dollars \nat 1989/90 prices)")  


#looks reasonable, follows the pattern of the data


# 7) Discussion/Conclusion

