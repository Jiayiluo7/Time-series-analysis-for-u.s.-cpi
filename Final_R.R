install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("readr")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(forecast)
library(zoo)

# Set Working Directory 
setwd("C:/Users/lktho/OneDrive/Desktop/MSBA CSUEB/Spring 2025/BAN 673-01 Time Series Analytics/Group")


##--------------------------------------------------------------------------------------------------------
## Data Pre-Processing:
##  1. Formatted US labor and unemployment data to time series format .csv
##  2. Load data into time-series function ts()
##--------------------------------------------------------------------------------------------------------

# 1. Formatted US labor and unemployment data to time series format
format_data <- function(file_name, output_csv, sheet_name = "BLS Data Series", data_range, data_label) {

  # Load data from the specified Excel file, sheet, and range
  raw <- read_excel(file_name, sheet = sheet_name, range = data_range, col_names = FALSE)

  # Rename columns: First column as "Year", others as month abbreviations (Jan, Feb, ..., Dec)
  colnames(raw) <- c("Year", month.abb)

  # Convert Year column to character to handle missing values properly
  raw$Year <- as.character(raw$Year)

  # Reshape data: Convert month columns into rows
  raw.long <- pivot_longer(raw, cols = -Year, names_to = "Month", values_to = data_label) %>%
    drop_na()

  # Convert Month names to numeric values
  raw.long$Month <- match(raw.long$Month, month.abb)

  # Create a proper date column
  raw.long <- raw.long %>%
    mutate(Year = as.numeric(Year),
           Date = as.Date(paste(Year, Month, "01", sep = "-")))

  # Select and rename final columns
  data <- raw.long %>%
    select(Year = Date, !!data_label := !!sym(data_label))

  # Save to CSV
  write_csv(data, output_csv)

  # Print message
  message("CSV file saved: ", output_csv)
}

#Process CPI Data
cpi_name <- "SeriesReport-CPI-U.xlsx"
cpi_u_csv <- "Formatted_CPI-U.csv"
cpi_data_range <- "A13:M23"
cpi_label <- "CPI"

format_data(cpi_name, cpi_u_csv, sheet_name = "BLS Data Series", cpi_data_range, cpi_label)

#Process Unemployment Data
unemp_name <- "SeriesReport-Unemployement.xlsx"
unemp_csv <- "Formatted_Unemployment.csv"
unemp_data_range <- "A13:M24"
unemp_label <- "Unemployment Rate"

format_data(unemp_name, unemp_csv, sheet_name = "BLS Data Series", unemp_data_range, unemp_label)


# 2. Load data into time-series ts()
# CPI
cpi.data <- read.csv("Formatted_CPI-U.csv")
head(cpi.data)
tail(cpi.data)

cpi.ts <- ts(cpi.data$CPI,
             start = c(2015,1), end = c(2025,1), freq = 12)
cpi.ts

# Unemployment rate
unemp.data <- read.csv("Formatted_Unemployment.csv")
unemp.data <- unemp.data[-c(1:12), ] # remove year 2014
head(unemp.data)
tail(unemp.data)

unemp.ts <- ts(unemp.data$Unemployment.Rate,
                 start = c(2015,1), end = c(2025,1), freq = 12)
unemp.ts

##--------------------------------------------------------------------------------------------------------
## EAD for CPI and unemployment rate data:
##  1. CPI data using stl() and acf() 
##  2. Unemployment rate data using stl() and acf()
##  3. CPI data predictability
##  4. Correlation between CPI and Unemployment
##--------------------------------------------------------------------------------------------------------

# 1. CPI data
# Use stl() function to plot times series components of the original data. 
cpi.stl <- stl(cpi.ts, s.window = "periodic")
autoplot(cpi.stl, main = "CPI Time Series Components")

# Use plot() to plot time series data  
plot(cpi.ts, 
     xlab = "Time", ylab = "CPI", 
     ylim = c(200, 350), xaxt = 'n',
     main = "CPI")

# USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION
autocor <- Acf(cpi.ts, lag.max = 12, 
               main = "Autocorrelation for CPI")

# 2. Unemployment rate data
unemp.stl <- stl(unemp.ts, s.window = "periodic")
autoplot(unemp.stl, main = "Unermployment Time Series Components")

# Use plot() to plot time series data  
plot(unemp.ts, 
     xlab = "Time", ylab = "Unemployment Rate", 
     ylim = c(0, 20), xaxt = 'n',
     main = "CPI")

# USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION
autocor <- Acf(unemp.ts, lag.max = 12, 
               main = "Autocorrelation for Unemployment Rate")

# 3. CPI data predictability
# Statistical test
cpi.ar1<- Arima(cpi.ts, order = c(1,0,0))
summary(cpi.ar1)

ar1 <- cpi.ar1$coef["ar1"]
s.e. <- sqrt(diag(vcov(cpi.ar1)))["ar1"]
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Fail to reject null hypothesis"}

# Order 1 differencing   
diff.cpi.ts <- diff(cpi.ts, lag = 1)
diff.cpi.ts
  
Acf(diff.cpi.ts, lag.max = 12, 
    main = "Autocorrelation for Order 1 Differencing CPI data")

# 4. Correlation between CPI and Unemployment
corr <- cor.test(cpi.data$CPI, unemp.data$Unemployment.Rate, 
                   method = "pearson")
corr


##--------------------------------------------------------------------------------------------------------
## PARTITION DATA : Train-validate : 70% (7 years) - 30% (3 years + 1 month)
##--------------------------------------------------------------------------------------------------------


# CPI data
nValid <- 37
nTrain <- length(cpi.ts) - nValid
train.ts <- window(cpi.ts, start = c(2015,1), end = c(2015, nTrain))
valid.ts <- window(cpi.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))

tail(train.ts)
tail(valid.ts)

# Unemployment Rate
train.unemp.ts <- window(unemp.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.unemp.ts <- window(unemp.ts, start = c(2015, nTrain + 1), end = c(2015, nTrain + nValid))

tail(train.unemp.ts)
tail(valid.unemp.ts)


##--------------------------------------------------------------------------------------------------------  
## ## Forecasting Methods : 3 models explored 
## # 1. Two-level mode: regression with linear trend and seasonality + trailing MA for residuals: 
## #   1.b No external- Cheryl
## #   1.a With external- Darcy, auto arima for external
## #
## # 2. Automated Holt-Winter’s Model-sab
## #
## # 3. Arima:
## #   3.a Season Arima-jiayi 
## #   3.b Auto Arima without external factor -Cheryl 
## #   3.c Auto Arima with externalfactor - Cheryl, auto arima for external
##--------------------------------------------------------------------------------------------------------  


#############################
## ## Model 1. Two-level model: regression with linear trend and seasonality + trailing MA for residuals 
## ## 1.a No external- Cheryl
#############################

# CPI : Train-validate set of 
# Level 1 : Fit a regression model with linear trend and seasonality for training partition. 
cpi.ln.trend.seas <- tslm(train.ts ~ trend  + season)
summary(cpi.ln.trend.seas)

cpi.ln.trend.seas.pred <- forecast(cpi.ln.trend.seas, h = nValid, level = 0)
cpi.ln.trend.seas.pred

cpi.ln.trend.seas.res <- cpi.ln.trend.seas$residuals

# Level 2 : Apply trailing MA for residuals with window width k = 3,6,9 and 12 
ma.trail3.res <- rollmean(cpi.ln.trend.seas.res, k = 3, align = "right")
ma.trail6.res <- rollmean(cpi.ln.trend.seas.res, k = 6, align = "right")
ma.trail9.res <- rollmean(cpi.ln.trend.seas.res, k = 9, align = "right")
ma.trail12.res <- rollmean(cpi.ln.trend.seas.res, k = 12, align = "right")

# Create residuals forecast for validation period.
ma.trail3.res.pred <- forecast(ma.trail3.res, h = nValid, level = 0)
ma.trail6.res.pred <- forecast(ma.trail6.res, h = nValid, level = 0)
ma.trail9.res.pred <- forecast(ma.trail9.res, h = nValid, level = 0)
ma.trail12.res.pred <- forecast(ma.trail12.res, h = nValid, level = 0)

# Combine : Develop two-level forecast for validation period
# regression forecast and trailing MA forecast for residuals.
trail3.2level <- cpi.ln.trend.seas.pred$mean + ma.trail3.res.pred$mean
trail6.2level <- cpi.ln.trend.seas.pred$mean + ma.trail6.res.pred$mean
trail9.2level <- cpi.ln.trend.seas.pred$mean + ma.trail9.res.pred$mean
trail12.2level <- cpi.ln.trend.seas.pred$mean + ma.trail12.res.pred$mean

# Measure validation accuracy : chose k=6
round(accuracy(trail3.2level , valid.ts), 3)
round(accuracy(trail6.2level , valid.ts), 3)
round(accuracy(trail9.2level , valid.ts), 3)
round(accuracy(trail12.2level , valid.ts), 3)

# CPI : Entire set and next 23 months
entire.ln.trend.seas <- tslm(cpi.ts ~ trend  + season)
summary(entire.ln.trend.seas)

# Create regression forecast for future 23 periods.
entire.ln.trend.seas.pred <- forecast(entire.ln.trend.seas, h = 23, level = 0)
entire.ln.trend.seas.pred

# Identify and display regression residuals for entire data set.
entire.ln.trend.seas.res <- entire.ln.trend.seas$residuals
entire.ln.trend.seas.res

# Use trailing MA to forecast residuals for entire data set, k=6
entire.ma.trail6.res <- rollmean(entire.ln.trend.seas.res, k = 6, align = "right")
entire.ma.trail6.res

# Create forecast for trailing MA residuals for future 23 periods.
entire.ma.trail6.res.pred <- forecast(entire.ma.trail6.res, h = 23, level = 0)
entire.ma.trail6.res.pred

# Entire data : Develop 2-level forecast for future 23 periods by combining 
entire.trail6.2level <- entire.ln.trend.seas.pred$mean + entire.ma.trail6.res.pred$mean
entire.trail6.2level

round(accuracy(entire.ln.trend.seas.pred$fitted + entire.ma.trail6.res, cpi.ts), 3)


#############################
## ## Model 1. Two-level model: regression with linear trend and seasonality + trailing MA for residuals
## ## 1.b With external- Darcy, choose auto arima for external factor
#############################


# Forecast External factor: auto arima vs auto SES
# Auto arima w/ seasonality
sarima.unemp <- auto.arima(unemp.ts, seasonal = TRUE)
summary(sarima.unemp)

sarima.unemp.pred <- forecast(sarima.unemp, h = 23)

# auto SIMPLE EXPONENTIAL SMOOTHING 
ses.unemp <- ets(unemp.ts, model = "ZZZ")
ses.unemp

ses.unemp.pred <- forecast(ses.unemp, h = 23)

round(accuracy(sarima.unemp.pred$fitted, unemp.ts), 3)
round(accuracy(ses.unemp.pred$fitted, unemp.ts), 3) 

# Prepare new external variable data for forecasting CPI
train.forecast_param <- data.frame(trend = seq(nTrain + 1, nTrain + nValid),
                                    train.unemp.ts = valid.unemp.ts)

train.forecast_param

forecast_param <- data.frame(trend = (length(cpi.ts) + 1):(length(cpi.ts) + 23), 
                             unemp.ts = sarima.unemp.pred$mean)
forecast_param



# CPI forecast model with unemployment rate using auto ariam as external factor  
# Level 1 : Fit a regression model with linear trend and seasonality for training partition. 
ext.cpi.ln.trend.seas <- tslm(train.ts ~ trend  + season + train.unemp.ts)
summary(ext.cpi.ln.trend.seas)

ext.cpi.ln.trend.seas.pred <- forecast(ext.cpi.ln.trend.seas,newdata = train.forecast_param, h = nValid, level = 0)
ext.cpi.ln.trend.seas.pred

ext.cpi.ln.trend.seas.res <- ext.cpi.ln.trend.seas$residuals

# Level 2 : Apply trailing MA for residuals with window width k = 3,6,9 and 12 
ext.ma.trail3.res <- rollmean(ext.cpi.ln.trend.seas.res, k = 3, align = "right")
ext.ma.trail6.res <- rollmean(ext.cpi.ln.trend.seas.res, k = 6, align = "right")
ext.ma.trail9.res <- rollmean(ext.cpi.ln.trend.seas.res, k = 9, align = "right")
ext.ma.trail12.res <- rollmean(ext.cpi.ln.trend.seas.res, k = 12, align = "right")

# Create residuals forecast for validation period.
ext.ma.trail3.res.pred <- forecast(ext.ma.trail3.res, h = nValid, level = 0)
ext.ma.trail6.res.pred <- forecast(ext.ma.trail6.res, h = nValid, level = 0)
ext.ma.trail9.res.pred <- forecast(ext.ma.trail9.res, h = nValid, level = 0)
ext.ma.trail12.res.pred <- forecast(ext.ma.trail12.res, h = nValid, level = 0)

# Combine : Develop two-level forecast for validation period
# regression forecast and trailing MA forecast for residuals.
ext.trail3.2level <- ext.cpi.ln.trend.seas.pred$mean + ext.ma.trail3.res.pred$mean
ext.trail6.2level <- ext.cpi.ln.trend.seas.pred$mean + ext.ma.trail6.res.pred$mean
ext.trail9.2level <- ext.cpi.ln.trend.seas.pred$mean + ext.ma.trail9.res.pred$mean
ext.trail12.2level <- ext.cpi.ln.trend.seas.pred$mean + ext.ma.trail12.res.pred$mean

# Measure validation accuracy : chose k=6
round(accuracy(ext.trail3.2level , valid.ts), 3)
round(accuracy(ext.trail6.2level , valid.ts), 3)
round(accuracy(ext.trail9.2level , valid.ts), 3)
round(accuracy(ext.trail12.2level , valid.ts), 3) 
  
# CPI : Entire set and next 23 months
entire.ext.ln.trend.seas <- tslm(cpi.ts ~ trend  + season + unemp.ts)
summary(entire.ext.ln.trend.seas)

# Create regression forecast for future 23 periods.
entire.ext.ln.trend.seas.pred <- forecast(entire.ext.ln.trend.seas,newdata = forecast_param, h = 23, level = 0)
entire.ext.ln.trend.seas.pred

# Identify and display regression residuals for entire data set.
entire.ext.ln.trend.seas.res <- entire.ext.ln.trend.seas$residuals
entire.ext.ln.trend.seas.res

# Use trailing MA to forecast residuals for entire data set, k=6
entire.ext.ma.trail6.res <- rollmean(entire.ext.ln.trend.seas.res, k = 6, align = "right")
entire.ext.ma.trail6.res

# Create forecast for trailing MA residuals for future 23 periods.
entire.ext.ma.trail6.res.pred <- forecast(entire.ext.ma.trail6.res, h = 23, level = 0)
entire.ext.ma.trail6.res.pred

# Entire data : Develop 2-level forecast for future 11 periods by combining 
entire.ext.trail6.2level <- entire.ext.ln.trend.seas.pred$mean + entire.ext.ma.trail6.res.pred$mean
entire.ext.trail6.2level

round(accuracy(entire.ext.ln.trend.seas.pred$fitted + entire.ext.ma.trail6.res, cpi.ts), 3)
round(accuracy(entire.ln.trend.seas.pred$fitted + entire.ma.trail6.res, cpi.ts), 3)



#############################
## ## Model 2: Holt-Winter's Exponential Smoothing, ZZZ option- sabrina
#############################


# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Use accuracy() function to identify common accuracy measures.
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

# Create Holt-Winter's (HW) exponential smoothing for full data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
entire.hw.ZZZ <- ets(cpi.ts, model = "ZZZ")
entire.hw.ZZZ

# Use forecast() function to make predictions 23 months
entire.hw.ZZZ.pred <- forecast(entire.hw.ZZZ, h = 23 , level = 0)
entire.hw.ZZZ.pred


round(accuracy(entire.ext.ln.trend.seas.pred$fitted + entire.ext.ma.trail6.res, cpi.ts), 3)
round(accuracy(entire.ln.trend.seas.pred$fitted + entire.ma.trail6.res, cpi.ts), 3)
round(accuracy(entire.hw.ZZZ.pred$fitted, cpi.ts), 3)



#############################
## ## Model 3 : Arima
## #   3.a Season Arima-jiayi 
#############################

## FIT ARIMA(2,1,2)(1,1,2) MODEL.

# Use Arima() function to fit ARIMA(2,1,2)(1,1,2) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(2,1,2), 
                          seasonal = c(1,1,2),
                          method = "ML") 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(2,1,2)(1,1,2) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2)(1,1,2) Model Residuals")

## FIT SEASONAL ARIMA ENTIRE DATA SET. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use arima() function to fit seasonal ARIMA(2,1,2)(1,1,2) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
entire.arima.seas <- Arima(cpi.ts, order = c(2,1,2), 
                    seasonal = c(1,1,2)) 
summary(entire.arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 23 periods. 
entire.arima.seas.pred <- forecast(entire.arima.seas, h = 23, level = 0)
entire.arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals.
Acf(entire.arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA (2,1,2)(1,1,2) Model Residuals")


round(accuracy(entire.ext.ln.trend.seas.pred$fitted + entire.ext.ma.trail6.res, cpi.ts), 3)
round(accuracy(entire.ln.trend.seas.pred$fitted + entire.ma.trail6.res, cpi.ts), 3)
round(accuracy(entire.hw.ZZZ.pred$fitted, cpi.ts), 3)
round(accuracy(entire.arima.seas$fitted, cpi.ts), 3)


#############################
## ## Model 3 : Arima
## ##   3.b Auto Arima without external factor -Cheryl 
#############################

## FIT AUTO ARIMA MODEL.
# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, auto ARIMA fitted values, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "CPI-U", 
     bty = "l", 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend("topleft", legend = c("CPI-U Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 5), lwd = c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
# Get y-axis limits for placing text and arrows
y_max <- max(cpi.ts) * 1.1
y_arrow <- y_max * 0.95
y_text <- y_max * 0.98

# Find the exact time points for partition
train_end_time <- time(train.ts)[length(train.ts)]
valid_end_time <- time(valid.ts)[length(valid.ts)]
start_time <- time(cpi.ts)[1]
future_end <- valid_end_time + 1

# Add vertical lines and text
abline(v = train_end_time, lty = 2)
abline(v = valid_end_time, lty = 2)
text(mean(c(start_time, train_end_time)), y_text, "Training")
text(mean(c(train_end_time, valid_end_time)), y_text, "Validation")
text(mean(c(valid_end_time, future_end)), y_text, "Future")

# Add arrows
arrows(start_time, y_arrow, train_end_time, y_arrow, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(train_end_time, y_arrow, valid_end_time, y_arrow, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(valid_end_time, y_arrow, future_end, y_arrow, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures 
# for validation period forecast
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

## FIT AUTO ARIMA MODEL FOR ENTIRE DATA SET
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
entire.auto.arima <- auto.arima(cpi.ts)
summary(entire.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 23 periods. 
entire.auto.arima.pred <- forecast(entire.auto.arima, h = 23, level = 0)
entire.auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(entire.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals (Entire Dataset)")

round(accuracy(entire.ext.ln.trend.seas.pred$fitted + entire.ext.ma.trail6.res, cpi.ts), 3)
round(accuracy(entire.ln.trend.seas.pred$fitted + entire.ma.trail6.res, cpi.ts), 3)
round(accuracy(entire.hw.ZZZ.pred$fitted, cpi.ts), 3)
round(accuracy(entire.arima.seas$fitted, cpi.ts), 3)
round(accuracy(entire.auto.arima$fitted, cpi.ts), 3)


#############################
## ## Model 3 : Arima
## ##   3.c Auto Arima with external factor -Cheryl 
#############################

## FIT ARIMAX MODEL ON TRAINING DATA
# Use auto.arima() with xreg parameter for external regressor
train.arimax <- auto.arima(train.ts, xreg = train.unemp.ts)
summary(train.arimax)

# Apply forecast() function to make predictions for validation period
# Need to provide the unemployment values for validation period as xreg
train.arimax.pred <- forecast(train.arimax, h = nValid, 
                              xreg = valid.unemp.ts, level = 0)
train.arimax.pred

# Using Acf() function, create autocorrelation chart of ARIMAX model residuals
Acf(train.arimax$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMAX Model Residuals")

# Plot ts data, ARIMAX fitted values, and predictions for validation period
# Get y-axis limits for placing text and arrows
y_max <- max(cpi.ts) * 1.1
y_arrow <- y_max * 0.95
y_text <- y_max * 0.98

# Find the exact time points for partition
train_end_time <- time(train.ts)[length(train.ts)]
valid_end_time <- time(valid.ts)[length(valid.ts)]
start_time <- time(cpi.ts)[1]
future_end <- valid_end_time + 1

plot(train.arimax.pred, 
     xlab = "Time", ylab = "CPI-U", 
     bty = "l", 
     main = "ARIMAX Model with Unemployment Rate", lwd = 2, flty = 5,
     ylim = c(min(cpi.ts) * 0.95, y_max)) 
lines(train.arimax.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend("topleft", legend = c("CPI-U Time Series", 
                             "ARIMAX Forecast for Training Period",
                             "ARIMAX Forecast for Validation Period"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 5), lwd = c(2, 2, 2), bty = "n")

# Add vertical lines and text
abline(v = train_end_time, lty = 2)
abline(v = valid_end_time, lty = 2)
text(mean(c(start_time, train_end_time)), y_text, "Training")
text(mean(c(train_end_time, valid_end_time)), y_text, "Validation")
text(mean(c(valid_end_time, future_end)), y_text, "Future")

# Add arrows
arrows(start_time, y_arrow, train_end_time, y_arrow, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(train_end_time, y_arrow, valid_end_time, y_arrow, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(valid_end_time, y_arrow, future_end, y_arrow, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures for validation period forecast
round(accuracy(train.arimax.pred$mean, valid.ts), 3)


## FIT ARIMAX MODEL FOR ENTIRE DATA SET
# Use auto.arima() function with xreg to fit ARIMAX model for entire data set
entire.arimax <- auto.arima(cpi.ts, xreg = unemp.ts)
summary(entire.arimax)

# For 23-month forecasts, we need 23 future values of unemployment
# First, forecast unemployment for the next 23 months using auto.arima
unemployment_model <- auto.arima(unemp.ts)
unemployment_forecast <- forecast(unemployment_model, h = 23)

# Display and plot the unemployment forecast
cat("\nUnemployment Rate Forecast for next 23 months:\n")
print(unemployment_forecast)

# Plot the unemployment forecasts
plot(unemployment_forecast, 
     main = "Unemployment Rate Forecast for Next 23 Months",
     xlab = "Time", ylab = "Unemployment Rate")

# Now use these forecasted unemployment values in the ARIMAX model
entire.arimax.pred <- forecast(entire.arimax, h = 23, 
                        xreg = unemployment_forecast$mean, level = 0)
entire.arimax.pred

# Use Acf() function to create autocorrelation chart of ARIMAX model residuals
Acf(entire.arimax$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMAX Model Residuals (Entire Dataset)")



##--------------------------------------------------------------------------------------------------------  
## ## Accuracy Comparisons
##--------------------------------------------------------------------------------------------------------  

# round(accuracy(entire.ext.ln.trend.seas.pred$fitted + entire.ext.ma.trail6.res, cpi.ts), 3)
# round(accuracy(entire.ln.trend.seas.pred$fitted + entire.ma.trail6.res, cpi.ts), 3)
# round(accuracy(entire.hw.ZZZ.pred$fitted, cpi.ts), 3)
# round(accuracy(entire.arima.seas$fitted, cpi.ts), 3)
# round(accuracy(entire.auto.arima$fitted, cpi.ts), 3)
# round(accuracy(entire.arimax.pred$fitted, cpi.ts), 3)

# Compute accuracy metrics in the given order
acc_2level <- round(accuracy(entire.ln.trend.seas.pred$fitted + entire.ma.trail6.res, cpi.ts), 3)
acc_ext_2level <- round(accuracy(entire.ext.ln.trend.seas.pred$fitted + entire.ext.ma.trail6.res, cpi.ts), 3)
acc_hw_zzz <- round(accuracy(entire.hw.ZZZ.pred$fitted, cpi.ts), 3)
acc_arima_seas <- round(accuracy(entire.arima.seas$fitted, cpi.ts), 3)
acc_auto_arima <- round(accuracy(entire.auto.arima$fitted, cpi.ts), 3)
acc_arimax <- round(accuracy(entire.arimax.pred$fitted, cpi.ts), 3)

# Create a comparison dataframe
accuracy_comparison <- data.frame(
  Model = c("2-Level Model (No Ext. Factor)", "2-Level Model (With Ext. Factor)", 
            "Holt-Winters (ZZZ) (No Ext. Factor)", "Seasonal ARIMA(2,1,2)(1,1,2) (No Ext. Factor)"
            , "Auto ARIMA(2,2,2)(0,0,2)[12] (No Ext. Factor)", "Auto ARIMA(0,1,1)(1,0,0)[12] (With Ext. Factor)"),
  ME = c(acc_2level[1], acc_ext_2level[1], acc_hw_zzz[1], acc_arima_seas[1], acc_auto_arima[1], acc_arimax[1]),
  RMSE = c(acc_2level[2], acc_ext_2level[2], acc_hw_zzz[2], acc_arima_seas[2], acc_auto_arima[2], acc_arimax[2]),
  MAE = c(acc_2level[3], acc_ext_2level[3], acc_hw_zzz[3], acc_arima_seas[3], acc_auto_arima[3], acc_arimax[3]),
  MPE = c(acc_2level[4], acc_ext_2level[4], acc_hw_zzz[4], acc_arima_seas[4], acc_auto_arima[4], acc_arimax[4]),
  MAPE = c(acc_2level[5], acc_ext_2level[5], acc_hw_zzz[5], acc_arima_seas[5], acc_auto_arima[5], acc_arimax[5]),
  ACF1 = c(acc_2level[6], acc_ext_2level[6], acc_hw_zzz[6], acc_arima_seas[6], acc_auto_arima[6], acc_arimax[6]),
  Theils_U = c(acc_2level[7], acc_ext_2level[7], acc_hw_zzz[7], acc_arima_seas[7], acc_auto_arima[7], acc_arimax[7])
)

# Print the comparison table
print(accuracy_comparison, row.names = FALSE)


# -----------------------------------------------------------------------
# Plotting Top 3 models
# -----------------------------------------------------------------------
plot_forecast <- function(ts_data, fitted_values, forecast_values, title) {
  
  # Extract start and end years dynamically
  start_year <- start(ts_data)[1]
  end_year <- end(ts_data)[1]
  future_periods <- length(forecast_values)  # Number of forecast periods
  forecast_end_year <- end_year + (future_periods / 12)  # Adjusted x-axis limit
  
  # Get dynamic Y-axis range
  y_min <- min(c(ts_data, fitted_values, forecast_values), na.rm = TRUE)-25
  y_max <- max(c(ts_data, fitted_values, forecast_values), na.rm = TRUE)+25
  
  # Plot the forecast
  plot(forecast_values, 
       xlab = "Time", ylab = "CPI", 
       ylim = c(y_min, y_max), 
       bty = "l", xlim = c(start_year, forecast_end_year), xaxt = "n",
       main = title, lty = 2, col = "blue", lwd = 2)
  
  # Adjust x-axis labels dynamically
  axis(1, at = seq(start_year, forecast_end_year+1, 1), labels = format(seq(start_year, forecast_end_year+1, 1)))
  
  # Add fitted values and actual data
  lines(fitted_values, col = "blue", lwd = 2)
  lines(ts_data, col = "black", lwd = 1)
  
  # Add legend
  legend(start_year + 1, y_max * 0.85, 
         legend = c("Actual CPI", "Model Fitted", "Model Forecast"), 
         col = c("black", "blue", "blue"), 
         lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = "n")
  
  # Add vertical line to separate past and forecasted values
  lines(c(end_year, end_year), c(y_min, y_max), lty = 2)
  
  # Add annotations for "Data Set" and "Future"
  text((start_year + end_year) / 2, y_max * 0.97, "Data Set")
  text(forecast_end_year-1, y_max * 0.97, "Future")
  
  # Add arrows indicating time ranges
  arrows(start_year, y_max*0.96 , end_year - 0.5, y_max*0.96 , 
         code = 3, length = 0.1, lwd = 1, angle = 30)
  arrows(end_year + 0.1, y_max*0.96 , forecast_end_year, 
         code = 3, length = 0.1, lwd = 1, angle = 30)
}

# model plotting
plot_forecast(cpi.ts, entire.arima.seas$fitted, entire.arima.seas.pred$mean, 
              title = "[Top 1] Seasonal ARIMA(2,1,2)(1,1,2) (No Ext. Factor) for CPI Feb 2025 - Dec 2026")

plot_forecast(cpi.ts, entire.hw.ZZZ$fitted, entire.hw.ZZZ.pred$mean , 
              title = "[Top 2] Holt-Winters (ZZZ) (No Ext. Factor) for CPI Feb 2025 - Dec 2026")

plot_forecast(cpi.ts, entire.auto.arima$fitted, entire.auto.arima.pred$mean, 
              title = "[Top 3] Auto ARIMA(2,2,2)(0,0,2)[12] (No Ext. Factor) for CPI Feb 2025 - Dec 2026")



# -----------------------------------------------------------------------
# Future forecast 23 months Feb 2025 - Dec 2026 of the Top 3 model
# -----------------------------------------------------------------------

forecast.top3 <- data.frame(
  Month = format(seq(as.Date("2025-02-01"), by = "month", length.out = 23), "%b %Y"),
  Seasonal_ARIMA = entire.arima.seas.pred$mean,
  Holt_Winters_ZZZ = entire.hw.ZZZ.pred$mean,
  Auto_ARIMA = entire.auto.arima.pred$mean
)

forecast.top3


