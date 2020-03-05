#read the following libraries
library(forecast)
library(lmtest)
library(tseries)
library(portes)

#read data
data = read.csv("C:\\Users\\tejan\\Downloads\\time-series-practice-datasets\\quarterlybeerproductioninaus.csv", header = TRUE)
head(data)
tail(data)

#changing column names
colnames(data) = c("Date", "Produce")

#converting to timeseries object by inspecting the head and tail of data
data_ts = ts(data$Produce, frequency =4, start = c(1965,1), end = c(1994,2))


#plot the data to visually inspect the trend, seasonality etc.
plot(data_ts, ylab = "Beer Produce per Quarters")
plot(aggregate(data_ts))
boxplot(data_ts ~ cycle(data_ts))

#since there is a clear trend and seasonality, 
#moving average model to smoothen and approximate the trend
ma = ma(data_ts, order = 4)
plot(data_ts, ylab = "Beer Produce per Quarters", col = "black")
lines(ma, col = 'blue')

#exponential smoothing  model to estimate trend
esm = HoltWinters(data_ts, gamma = FALSE)
plot(esm)


#check forecast accuracy fro moving average and esm models
fa_ma = forecast(ma, h = 10)
fa_esm = forecast(esm, h = 10)
plot(fa_ma)
plot(fa_esm)

accuracy(fa_esm)
accuracy(fa_esm)

#building same models, with seasonality removed
data_trend = data_ts - data_ts_dc$seasonal
ma2 = ma(data_trend, order = 4)
plot(data_ts, ylab = "Beer Produce per Quarters", col = "black")
lines(ma2, col = 'blue')

#esm odel to estimate trend
esm2 = HoltWinters(data_trend, gamma = FALSE)
plot(esm2)

#check forecast accuracy fro ma and esm models
fa_ma_2 = forecast(na.omit(ma2), h = 10)
fa_esm_2 = forecast(na.omit(esm2), h = 10)
plot(fa_ma_2)
plot(fa_esm_2)

accuracy(fa_esm_2)
accuracy(fa_esm_2)

#decompose
data_ts_dc = decompose(data_ts)
plot(data_ts_dc)

#plotting trend removed data and moving average trend removed data - both plots looks alike  
layout(1:2)
plot(data_ts - data_ts_dc$trend)
plot(data_ts - ma)

#checking for stationarity - adf test, also checking acf, pacf
acf(na.omit(data_ts - data_ts_dc$seasonal - data_ts_dc$trend))
pacf(na.omit(data_ts - data_ts_dc$seasonal - data_ts_dc$trend))
adf.test(na.omit(data_ts - data_ts_dc$seasonal - data_ts_dc$trend), k = 20, alternative =  "stationary")

#since there is some stationarity even after removing trend and seasonality, 
#difference the data
data_trend = data_ts  - data_ts_dc$seasonal
data_d1 = diff(data_trend, differences = 3)
plot(data_d1)

acf(data_d1)
pacf(data_d1)
adf.test(data_d1,k = 20, alternative =  "stationary" )

#now that the data is stationary, build ARMA models, Choose the AR and MA terms from ACf and PACF plots
arima_custom = arima(data_d1, order = c(3,0,2), method = "ML")
arima_custom
coeftest(arima_custom)

#forecast with this model for 20 periods
arima_forecast = forecast(arima_custom, h = 20)
plot(arima_forecast)
accuracy(arima_forecast)

#now, check if the residual are white noise using djung-box test and acf, pacf,
#also check the normality and homoscedasticity of the residuals
plot(arima_forecast$residuals)
lines(c(1965, 1995), c(0,0), col = "blue")
hist(arima_forecast$residuals)

Box.test(arima_forecast$residuals, lag = 10, type = "Ljung-Box", fitdf = 5)



