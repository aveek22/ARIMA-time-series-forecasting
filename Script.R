# ARIMA Timeseries forecasting using R
# Source URL: https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

# Import Library and Data
library(ggplot2)
library(forecast)
library(tseries)

data <- read.csv('Dataset/Bike-Sharing-Dataset/day.csv',header = T, stringsAsFactors = F)


# Add Date column with changed data type
data$Date <- as.Date(data$dteday)

# Plot the data
ggplot(data,aes(Date,cnt))+ geom_line()+ scale_x_date('Month')+ylab("Daily Bike Checkouts")+xlab("")

# Create a time series object to pass to tsclean():
count_ts <- ts(data[,c('cnt')])

# Clean time-series data for outliers
data$clean_cnt <- tsclean(count_ts)
ggplot(data,aes(Date,clean_cnt))+ geom_line()+ scale_x_date('Month')+ylab(" Cleaned Daily Bike Checkouts")+xlab("")


# Calculate moving avegrages
# Weekly MA
data$week_ma <- ma(data$clean_cnt, order = 7)


# Monthly MA
data$month_ma <- ma(data$clean_cnt, order = 30)


# Plot all the graphs for comparison
ggplot() +
  geom_line(data = data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = data, aes(x = Date, y = week_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = data, aes(x = Date, y = month_ma, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

# Decomposing the time series data
# Omit missing values and generate time-series with frequency=30
count_ma <- ts(na.omit(data$week_ma), frequency = 30)
decomp <- stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# ADF Test
adf.test(count_ma, alternative = "stationary")

# Plot ACF and PACF (Auto-correlation function and Partial ACF)
acf(count_ma, main="")
pacf(count_ma, main="")

# Create and plot the differenced series
count_ma_diff <- diff(deseasonal_cnt, differences = 1)
plot(count_ma_diff)

# ADF Test
adf.test(count_ma_diff, alternative = "stationary")

# Plot ACF and PACF (Auto-correlation function and Partial ACF) on differenced series
acf(count_ma_diff, main="ACF for differenced series")
pacf(count_ma_diff, main="PACF for differenced series")

# Generate ARIMA model
auto.arima(deseasonal_cnt, seasonal = F)

# Fit ARIMA model
fit <- auto.arima(deseasonal_cnt, seasonal = F)
tsdisplay(residuals(fit), lag.max = 45, main = "1-1-1 Model Residuals")

# Fit ARIMA model for (1,1,7)
fit2 <- arima(deseasonal_cnt, order = c(1,1,7))
fit2
tsdisplay(residuals(fit2), lag.max = 15, main = "Seasonal Model Residuals")

# Forecast using the fit model
fcast <- forecast(fit2, h=30)
plot(fcast)

# Test the model on the data itself
hold <- window(ts(deseasonal_cnt), start=700)
fit_no_holdout <- arima(ts(deseasonal_cnt[-c(700:725)]), order = c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout, h=25)

# Plot the No holdout fit
plot(fcast_no_holdout, main = "")
lines(ts(deseasonal_cnt))

# Fit with seasonality
fit_w_seasonality <- auto.arima(deseasonal_cnt, seasonal = T)
fit_w_seasonality

seasonal_fcast <- forecast(fit_w_seasonality, h=30)
plot(seasonal_fcast)









