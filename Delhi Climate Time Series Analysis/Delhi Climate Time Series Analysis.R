library(readxl)
library(ggplot2)
library(lubridate)
library(stats)
library(forecast)

# Read/Import the data from the  file
data <- Delhi_Climate  # Replace with your file path
head(data)

data$date <- as.Date(data$date, format="%Y-%m-%d")

#Time Plot
ggplot(data, aes(x=date, y=meantemp)) + 
  geom_line() + 
  ggtitle("Time Plot of Mean Temperature") + 
  xlab("Date") + 
  ylab("Mean Temperature")

acf(data$meantemp, main="ACF of Mean Temperature")

# Create an ACF plot
acf_plot <- forecast::Acf(data$meantemp, lag.max = 30, plot = TRUE)

ts_data <- ts(data$meantemp, frequency=7)  # assuming weekly seasonality due to lack of info
decomposed_data <- stl(ts_data, s.window="periodic")
plot(decomposed_data)


lambda <- BoxCox.lambda(ts_data)
ts_data_transformed <- BoxCox(ts_data, lambda)
print(ts_data_transformed)

#ARIMA
fit_arima <- auto.arima(ts_data_transformed)
forecast_arima <- forecast(fit_arima, h=7)  # forecast for the next 7 days
plot(forecast_arima)

#ETS Model Forecast
fit_ets <- ets(ts_data_transformed)
forecast_ets <- forecast(fit_ets, h=7)
plot(forecast_ets)

#Residual Analysis
# ARIMA residuals
residuals_arima <- residuals(fit_arima)
plot(residuals_arima, main="ARIMA Residuals")
acf(residuals_arima, main="ACF of ARIMA Residuals")

# ETS residuals
residuals_ets <- residuals(fit_ets)
plot(residuals_ets, main="ETS Residuals")
acf(residuals_ets, main="ACF of ETS Residuals")
