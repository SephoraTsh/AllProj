# Data
install.packages("fGarch")
library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(fGarch)
library(caret)
library(dplyr)

prod <- read_csv("Individual Assignment/archive/global_electricity_production_data.csv")

head(prod)

## EDA Analysis ## 
glimpse(prod)  # To see the structure and data types
summary(prod)  # To get summary statistics


prod$date <- as.Date(prod$date, format = "%m/%d/%Y")

unique(prod$date)

prod$country_name <- as.factor(prod$country_name)
prod$parameter <- as.factor(prod$parameter)
prod$product <- as.factor(prod$product)
prod$unit <- as.factor(prod$unit)


# Check for missing values
sum(is.na(prod))  # Count of missing values



# U.S Line plot of electricity production over time for US
ggplot(data = prod %>% filter(country_name == "United States", product == "Electricity"), 
       aes(x = date, y = value, group = 1)) +
  geom_area(fill = "steelblue") +
  labs(title = "Electricity Production in the United States", 
       x = "Date", y = "Production (Value)")




# Bar chart of electricity production by country
ggplot(data = prod %>% group_by(country_name) %>% summarise(total_prod = sum(value)), 
       aes(x = reorder(country_name, -total_prod), y = total_prod)) +
  geom_col() +
  labs(title = "Total Electricity Production by Country",
       x = "Country", y = "Total Production") +
  coord_flip()


# Assuming your data is in a data frame called 'prod'
top_10_countries <- prod %>%
  group_by(country_name) %>%
  summarise(total_prod = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_prod)) %>%
  slice(1:10) %>%
  pull(country_name)

filtered_data <- prod %>%
  filter(country_name %in% top_10_countries)

## GGPlot for top 10 ##
ggplot(filtered_data %>% group_by(country_name) %>% summarise(total_prod = sum(value, na.rm = TRUE)), 
       aes(x = reorder(country_name, -total_prod), y = total_prod)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Country", y = "Top 10 Electricity Production") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)  # Add comma separators to y-axis labels

## Decided to choose US and China ince they're the top performing production of electricity                                        



###  United States Analysis 1 ### 


#Subset the data for the United States
us_data <- subset(prod, country_name == "United States")


#Prepare the data for time series analysis
us_data$date <- as.Date(us_data$date, format = "%m/%d/%Y")  # Replace with the appropriate format

#Create a time series object from the data
us_ts <- ts(us_data$value, start = c(date(min(us_data$date)), 1), frequency = 12)


## Time Series of 2023 ##
frequency(us_ts)

forecast_start <- c(14880, 1)  # January 2023
forecast_end <- c(14891, 12)   # December 2023


# Determine the split point
split_point <- c(14850, 12)  # December 2021 (assuming monthly data)

# Split the data into training and testing sets
train_data <- window(us_ts, end = split_point)
test_data <- window(us_ts, start = split_point + c(0, 1))



# Train the models
arma_model <- auto.arima(train_data)
arch_model <- garch(train_data, order = c(1, 1))
garch_model <- garch(train_data, order = c(1, 1), type = "garch")


# Make predictions on the testing set
arma_forecast <- forecast(arma_model, h = length(test_data))

arch_forecast <- predict(arch_model, n.ahead = length(test_data), trace = FALSE)

garch_forecast <- predict(garch_model, n.ahead = length(test_data), trace = FALSE)

# Extract the forecasted values
arma_forecast <- arma_forecast$mean
# arch_forecast and garch_forecast are already vectors of forecasted values


# Evaluate the models
accuracy(arma_forecast, test_data)

# Align arch_forecast and test_data
arch_forecast_aligned <- window(arch_forecast, start = c(14610, 1), end = c(14850, 12))
print(start(arch_forecast_aligned))
print(end(arch_forecast_aligned))

# Convert test_data to a time series object
test_data_ts <- ts(test_data, start = c(14610, 1), end = c(14850, 12), frequency = 12)

# Align test_data with arch_forecast
test_data_aligned <- window(test_data_ts, start = start(arch_forecast_aligned), end = end(arch_forecast_aligned))



## Garch Forecast
garch_forecast <- predict(garch_model, n.ahead = length(test_data), trace = FALSE)

# Print start and end indices of garch_forecast
print(start(garch_forecast))
print(end(garch_forecast))


# Convert test_data to a time series object
test_data_ts <- ts(test_data, frequency = 12, start = start(garch_forecast))

# Align test_data with garch_forecast
garch_time_index <- merge(yearmon(start(garch_forecast)), yearmon(end(garch_forecast)))
# Evaluate the model
accuracy(garch_forecast, test_data_aligned)





                            # United States Analysis 2#
us_data <- prod %>%
  filter(country_name == "United States", product == "Electricity")

# Create a time series object for the United States
us_ts <- ts(us_data$value, start = c(year(min(us_data$date)), month(min(us_data$date))), frequency = 12)

# ARMA Forecast for the United States
fit_arma_us <- Arima(us_ts, order = c(1, 0, 1))
forecast_arma_us <- forecast(fit_arma_us, h = 12)
plot(forecast_arma_us, main = "ARMA Forecast for US Electricity Production")


# ARIMA Forecast for the United States
fit_arima_us <- auto.arima(us_ts)
forecast_arima_us <- forecast(fit_arima_us, h = 12)
plot(forecast_arima_us, main = "ARIMA Forecast for US Electricity Production")

# ARIMA Forecast for the United States (2023)
fit_arima_us <- auto.arima(us_ts)
forecast_arima_us <- forecast(fit_arima_us, h = 12)
plot(forecast_arima_us, xlim = c(2023, 2024), main = "ARIMA Forecast for US Electricity Production (2023)")



    ## Linear Regression Model for US ##

# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(us_data$value, p = 0.8, list = FALSE, times = 1)
us_train <- us_data[trainIndex, ]
us_test <- us_data[-trainIndex, ]

# Prepare data for modeling
us_train_x <- us_train$date
us_train_y <- us_train$value
us_test_x <- us_test$date
us_test_y <- us_test$value

# Convert us_train_x to a data frame
us_train_x <- data.frame(time = us_train_x)

# Build the linear regression model
lm_us <- train(us_train_x, us_train_y, method = "lm", preProcess = c("center", "scale"))

# Make predictions and evaluate the model
us_predictions <- predict(lm_us, data.frame(time = us_test_x))
us_rmse <- RMSE(us_predictions, us_test_y)
us_r_squared <- R2(us_predictions, us_test_y)

cat("United States:\nRMSE:", us_rmse, "\nR²:", us_r_squared, "\n\n")

##United States:
RMSE: 156904.8  
R²: 0.0003032044


# ACF plot for United States
acf_plot_us <- ggAcf(us_ts, main = "ACF Plot for US Electricity Production")
print(acf_plot_us)






                          ### China Aanlysis 1###
ggplot(data = prod %>% filter(country_name == "China", product == "Electricity"), 
       aes(x = date, y = value, group = 1)) +
  geom_line() +
  labs(title = "Electricity Production in the China", 
       x = "Date", y = "Production (Value)")

# Subset the data for China
china_data <- subset(prod, country_name == "China")

# Prepare the data for time series analysis
china_data$date <- as.Date(china_data$date, format = "%m/%d/%Y")  # Replace with the appropriate format

# Create a time series object from the data
china_ts <- ts(china_data$value, start = c(year(min(china_data$date)), 1), frequency = 12)

# Determine the split point (assuming monthly data)
split_point <- c(year(max(china_data$date)), 12)  # December of the last year in the data

# Split the data into training and testing sets
train_data <- window(china_ts, end = split_point)
test_data <- window(china_ts, start = split_point + c(0, 1))

# Train the models
arma_model <- auto.arima(train_data)
arch_model <- garch(train_data, order = c(2, 0))
garch_model <- garch(train_data, order = c(2, 1), type = "garch")

# Forecast for 2023
forecast_start <- c(year(max(china_data$date)) + 1, 1)  # January of the following year
forecast_end <- c(year(max(china_data$date)) + 1, 12)   # December of the following year

# Make predictions for 2023
arma_forecast <- forecast(arma_model, h = 12)
arch_forecast <- predict(arch_model, n.ahead = 12, trace = FALSE)
garch_forecast <- predict(garch_model, n.ahead = 12, trace = FALSE)

# Extract the forecasted values
arma_forecast <- arma_forecast$mean

# Evaluate the models (if you have actual values for 2023)
accuracy(arma_forecast, actual_values_2023)
accuracy(arch_forecast, actual_values_2023)
accuracy(garch_forecast, actual_values_2023)

# If you don't have actual values for 2023, you can plot the forecasts
plot(arma_forecast, main = "ARMA Forecast for China 2023")
plot(arch_forecast, main = "ARCH Forecast for China 2023")
plot(garch_forecast, main = "GARCH Forecast for China 2023")

# Make predictions on the testing set
arma_forecast <- forecast(arma_model, h = length(test_data))
arch_forecast <- predict(arch_model, n.ahead = length(test_data), trace = FALSE)
garch_forecast <- predict(garch_model, n.ahead = length(test_data), trace = FALSE)

# Extract the forecasted values
arma_forecast <- arma_forecast$mean

# Evaluate the models
accuracy(arma_forecast, test_data)



                                          # China Analysis 2 #
china_data <- prod %>%
  filter(country_name == "China", product == "Electricity")

# Create a time series object for China
china_ts <- ts(china_data$value, start = c(year(min(china_data$date)), month(min(china_data$date))), frequency = 12)

# ARMA Forecast for China
fit_arma_china <- Arima(china_ts, order = c(1, 0, 1))
forecast_arma_china <- forecast(fit_arma_china, h = 12)
plot(forecast_arma_china, main = "ARMA Forecast for China Electricity Production")

# ARIMA Forecast for China
fit_arima_china <- auto.arima(china_ts)
forecast_arima_china <- forecast(fit_arima_china, h = 12)
plot(forecast_arima_china, main = "ARIMA Forecast for China Electricity Production")



      ## Linear Regression For China ##
china_data <- prod %>%
  filter(country_name == "China", product == "Electricity")

# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(china_data$value, p = 0.8, list = FALSE, times = 1)
china_train <- china_data[trainIndex, ]
china_test <- china_data[-trainIndex, ]

# Prepare data for modeling
china_train_x <- china_train$date
china_train_y <- china_train$value
china_test_x <- china_test$date
china_test_y <- china_test$value

# Convert china_train_x to a data frame
china_train_x <- data.frame(time = china_train_x)

# Build the linear regression model
lm_china <- train(china_train_x, china_train_y, method = "lm", preProcess = c("center", "scale"))

# Make predictions and evaluate the model
china_predictions <- predict(lm_china, data.frame(time = china_test_x))
china_rmse <- RMSE(china_predictions, china_test_y)
china_r_squared <- R2(china_predictions, china_test_y)

cat("China:\nRMSE:", china_rmse, "\nR²:", china_r_squared, "\n")

##China:
RMSE: 46265.14
R²: 0.8476346


# ACF plot for China
acf_plot_china <- ggAcf(china_ts, main = "ACF Plot for China Electricity Production")
print(acf_plot_china)
