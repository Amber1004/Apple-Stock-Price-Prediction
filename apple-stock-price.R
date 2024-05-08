library(zoo)
library(quantmod)
library(forecast)
library(TSA)
library(rugarch)
library(fgarch)

setwd("C:/Users/PREDATOR/OneDrive - Nanyang Technological University/MSAI-Sem-2/AI6123-Time-Series-Analysis/Assignment3")

symbol <- 'AAPL'
start_date <- as.Date('2002-02-01')
end_date <- as.Date('2017-02-01')

data <- getSymbols(symbol, from=start_date, to=end_date, src='yahoo', auto.assign=FALSE)
class(data)
tail(data)
adj_close_data <- data[,6]
class(adj_close_data)
head(adj_close_data, 5)
tail(adj_close_data,5)
ncol(adj_close_data)
nrow(adj_close_data)

max(adj_close_data) # 29.86383
min(adj_close_data) # 0.198346
mean(adj_close_data) # 9.459981

# Plot time series data
plot.zoo(adj_close_data)

# Augmented Dicker-Fuller Test
adf.test(adj_close_data) #0.4848

# ADC/PACF Plot
acf(adj_close_data)
pacf(adj_close_data)

# Seasonal Decomposition for Adjusted Close Price
monthly_data <- to.monthly(data)
adj_monthly <- Ad(monthly_data)
ts_adj <- ts(adj_monthly, frequency=12)
fit.stl <- stl(ts_adj[,1], s.window = "period")
autoplot(fit.stl, main="STL Decomposition")

# Log Transformation
hist(adj_close_data)
log_adj_close_data <- log(adj_close_data)

# Trend Differencing
diff_log_adj_close_data <- diff(log_adj_close_data)
scaled_diff_log_adj_close_data <- diff_log_adj_close_data
plot.zoo(scaled_diff_log_adj_close_data, main="Log Transformed Adjusted Close Price")

# Remove missing value
clean_adj_close_data <- na.omit(scaled_diff_log_adj_close_data)
head(clean_adj_close_data,5)

adf.test(clean_adj_close_data) #0.01

## Time series plot/ACF/PACF
plot.zoo(clean_adj_close_data)
acf(clean_adj_close_data)
pacf(clean_adj_close_data)

hist(clean_adj_close_data)
qqnorm(clean_adj_close_data)
qqline(clean_adj_close_data)
skewness(clean_adj_close_data) #-0.1942065
kurtosis(clean_adj_close_data) #5.445903

plot.zoo(abs(clean_adj_close_data))
acf(abs(clean_adj_close_data))
pacf(abs(clean_adj_close_data))

plot.zoo(clean_adj_close_data^2)
acf(clean_adj_close_data^2)
pacf(clean_adj_close_data^2)

# EACF: get the parameters for the ARMA(p,q) models
eacf(clean_adj_close_data)
eacf(abs(clean_adj_close_data))
eacf(clean_adj_close_data^2)

# Split dataset
total <- length(clean_adj_close_data) #3775
train_data <- head(clean_adj_close_data,total-30) #3745
test_data <- tail(clean_adj_close_data,30) #30

# GARCH Model
garch40<-garch(clean_adj_close_data,order=c(4,0)) 
summary(garch40)
AIC(garch40) #-17926.97

garch11<-garch(clean_adj_close_data,order = c(1,1))
summary(garch11)
AIC(garch11) # -18556.44

garch22<-garch(clean_adj_close_data,order = c(2,2))
summary(garch22)
AIC(garch22) # -18556.44

garch33<-garch(clean_adj_close_data,order = c(3,3))
summary(garch33)
AIC(garch33) # -18556.44



# GARCH(1,1) Diagnostic Check
checkresiduals(garch11)
qqnorm(residuals(garch11))
qqline(residuals(garch11))
ggtsdisplay(abs(residuals(garch11)))
ggtsdisplay(residuals(garch11)^2)    
gBox(garch11,method = 'squared')

# Forecast
fgarch_11 <- garchFit(~ garch(1, 1), data = train_data, trace = FALSE, cond.dist = "std")
plot(fgarch_11, which=13) # qq-plot
plot(fgarch_11, which=3)
fgarch_11.pred <- predict(fgarch_11,n.ahead = 30, plot=TRUE)
accuracy(fgarch_11.pred$meanForecast, test_data) #0.005045741
summary(fgarch_11)

