install.packages("tseries")


library(tseries)

jj <- scan("C:/Users/AN000/OneDrive/Desktop/Masters Program/Trimester 2 (13 Nov - 24 Feb 2024)/MH6241 - Time Series Analysis (Mon) [1.5 AU]/Group Project (due 16 Feb)/JJ.dat",skip=1)

par(mar = c(0.5, 1, 0.5, 1))
#time plot
ts.plot(jj)

#non-stationary

#plotting ACF and PACF
acf(jj)  #does not cut off
pacf(jj) #cuts off at lag 1 

#Since the variance is not constant
#we do log transformation

log_jj <- log(jj)
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
acf(z1) #cuts off at lag 16
#a peak at every 4,8,12,16 lag, 
#which shows there is seasonality

pacf(z1) 
#cuts off at lag 4
#does not show seasonality

#seasonal differencing
s_z1 = diff(z1,lag=4)

plot(s_z1,type='o')

#acf and pacf of the difference data
acf(s_z1) #cuts off at lag (significant lag at 7)

pacf(s_z1) #cuts off at lag 4

#Models to check:
#From PACF
#Model 1: (P=1,Q=0,D=1),(p=1,q=0,d=1)
#From ACF 
#Model 2: (P=0,Q=1,D=1),(p=0,q=1,d=1)

fit1 = arima(log_jj, order =c(1,0,1), seasonal = list(order =c(1,0,1), period =4))
tsdiag(fit1) 

fit2 = arima(log_jj, order =c(1,0,1), seasonal = list(order =c(0,1,1), period =4))
tsdiag(fit2) 

fit3 = arima(log_jj, order =c(0,1,1), seasonal = list(order =c(1,0,1), period =4))
tsdiag(fit3) 

fit4 = arima(log_jj, order =c(0,1,1), seasonal = list(order =c(0,1,1), period =4))
tsdiag(fit4)