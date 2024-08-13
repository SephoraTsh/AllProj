#Data
library(readr)
library(readr)   # For reading data
library(dplyr)   # For data manipulation
library(ggplot2) # For data visualization
library(scales)  # For formatting y-axis labels
library(lubridate)
library(forecast)
library(tseries)
library(zoo)
library(caret)

prod <- read_csv("Individual Assignment/archive/global_electricity_production_data.csv")

head(prod)

                    ## EDA Analysis ## 
glimpse(prod)  # To see the structure and data types
summary(prod)  # To get summary statistics

## Class and Mode are showing as character, therefore need to convert them. 

##


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


# Assuming your data is in a data frame called 'electricity_prod'
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


## Knowing now that United Stated has the most electricity production.




          ###  United States only ### 


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




                            ### China ###
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











