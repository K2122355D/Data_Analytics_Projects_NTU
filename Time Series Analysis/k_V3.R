install.packages("forecast")
install.packages("tseries")
library(tseries)
library(forecast)
jj <- scan("C:/Users/chuny/Downloads/JJ.dat",skip=1)
jj
#time plot
ts.plot(jj)

#null hypothesis:non-stationary
adf.test(jj)  

#plotting ACF and PACF
acf(jj)  #does not cut off
pacf(jj) #cuts off at lag 1

#Since the variance is not constant
#we do log transformation
log_jj <- log(jj)
length(log_jj)
log_jj

#time-plot
ts.plot(log_jj)

#plotting ACF and PACF
acf(log_jj) #does not cut-off
pacf(log_jj)

#first order differencing
z1 <- diff(log_jj)

plot(z1,type='o')

#acf and pacf of the difference data
acf(z1) 
#from the ACF of the first order differencing we can
#observe that there are lags at 4,8,12,16...
#Hence, and the acf dies down gradually

pacf(z1) #cuts off at lag 4

#seasonal differencing
s_z1 = diff(log_jj,lag=4)

plot(s_z1,type='o')

#from the adf test we see that the time series is not stationary.
adf.test(s_z1)

#acf and pacf of the difference data
acf(s_z1) #cuts off at lag (significant lag at 7)

#acf dies down quickly
pacf(s_z1) #cuts off at lag 4

ma <- ma(s_z1, order = 12)
plot(s_z1, type = "l", xlab = "Time", ylab = "Value", main = "Time Series Data with 12-Month Moving Average")
lines(ma, col = "red")

#Non-seasonal Differencing
k <- diff(s_z1)
acf(k)
pacf(k)
adf.test(k)
#Models to check:
#From ACF
#Model 1: (p=0,d=1,q=1)x(P=0,D=1,Q=1)
#From PACF 
#Model 2: (p=1,d=1,q=0)x(P=1,D=1,Q=0)


fit1 = arima(log_jj, order =c(0,1,1), seasonal = list(order =c(0,1,1), period = 4))
tsdiag(fit1) 

plot(1:84, log_jj, xlim = c(0, 84), ylim=c(-1,3))
lines(1:84, log_jj, type="l")
lines(1:84, log_jj-fit1$residuals, type="l", col="red")

fit2 = arima(log_jj, order =c(1,1,0), seasonal = list(order =c(1,1,0), period =4))
tsdiag(fit2) 

plot(1:84, log_jj, xlim = c(0, 84), ylim=c(-1,3))
lines(1:84, log_jj, type="l")
lines(1:84, log_jj-fit2$residuals, type="l", col="red")

fit3 = arima(log_jj, order =c(0,1,1), seasonal = list(order =c(1,1,0), period =4))
tsdiag(fit3) 
plot(1:84, log_jj, xlim = c(0, 84), ylim=c(-1,3))
lines(1:84, log_jj, type="l")
lines(1:84, log_jj-fit3$residuals, type="l", col="red")

fit4 = arima(log_jj, order =c(1,1,0), seasonal = list(order =c(0,1,1), period =4))
tsdiag(fit4)  
plot(1:84, log_jj, xlim = c(0, 84), ylim=c(-1,3))
lines(1:84, log_jj, type="l")
lines(1:84, log_jj-fit4$residuals, type="l", col="red")



#get the model with the best AIC
fit1
fit2
fit3
fit4

#according to aic the best model is fit 3

#Using the auto.arima function
auto.arima(log_jj)


#use fit 1 to forecast ahead
forecast = predict(fit1, n.ahead=4)
forecast
plot(1:84, log_jj, xlim = c(0, 90), ylim=c(-1,3))
lines(85:88, forecast$pred, type="l", col="red")
lines(85:88, forecast$pred-1.96*forecast$se, col="blue")
lines(85:88, forecast$pred+1.96*forecast$se, col="blue")

#use fit 4 to forecast ahead
forecast4 = predict(fit4, n.ahead=4)
forecast4
plot(1:84, log_jj, xlim = c(0, 90), ylim=c(-1,3))
lines(85:88, forecast4$pred, type="l", col="red")
lines(85:88, forecast4$pred-1.96*forecast$se, col="blue")
lines(85:88, forecast4$pred+1.96*forecast$se, col="blue")
