##########################################################
# part 1:Download Data sets and load them 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(prophet)) install.packages("prophet", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(tseries)) install.packages("tseries", repos = "http://cran.us.r-project.org")

# Load necessary packages
library(tidyverse)
library(tidyr)
library(caret)
library(ggplot2)
library(dplyr)
library(lubridate)
library(glmnet)
library(forecast)
library(prophet)
library(zoo)
library(tseries)

# Download and load salesdaily dataset 

# 1. Define URL
url_salesdaily <- "https://raw.githubusercontent.com/ahmedm3laa/pharma-sales-ml-project/main/raw_data/salesdaily.csv"

# 2. Create local data folder if it doesn't exist
dest_dir <- "data"
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# 3. Download file
destfile <- file.path(dest_dir, "salesdaily.csv")
download.file(url_salesdaily, destfile, mode = "wb")

# 4. Load dataset
salesdaily <- read.csv(destfile)

# 5. Quick check
head(salesdaily)


###############################################################################
#part 2 :Preparing the Data 
############################################################################### 

#Step 1 - Remove Wrong Columns
# Remove incorrect Kaggle-generated columns

cols_to_remove <- c("Month", "Hour", "Weekday.Name")
salesdaily <- salesdaily %>% select(-any_of(cols_to_remove))

# Create proper date column
salesdaily <- salesdaily %>%
  mutate(
    Date = as.Date(datum, format = "%m/%d/%Y"),
    Year = year(Date),
    Month = month(Date),
    Weekday = weekdays(Date),
    t_sales = M01AB + M01AE + N02BA + N02BE + N05B + N05C + R03 + R06
  )

# Check last rows
tail(salesdaily)


head(salesdaily)


#step 2 :Create model data 
model_data <- salesdaily %>%
  select(
    Date,
    Month,
    Weekday,
    t_sales
  )

model_data$Weekday <- as.factor(model_data$Weekday)

#Treat zeros as closed days
model_data <- model_data %>%
  mutate(is_closed = ifelse(t_sales == 0, 1, 0))

###############################################################################
#part 3 :Exploratory Data Analysis (EDA) 
############################################################################### 
#daily category lines almost always overlap and hide the pattern.
#so I will Aggregate sales to MONTHLY level for better visualization 
monthly_sales <- salesdaily %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarize(
    M01AB = sum(M01AB, na.rm = TRUE),
    M01AE = sum(M01AE, na.rm = TRUE),
    N02BA = sum(N02BA, na.rm = TRUE),
    N02BE = sum(N02BE, na.rm = TRUE),
    N05B  = sum(N05B,  na.rm = TRUE),
    N05C  = sum(N05C,  na.rm = TRUE),
    R03   = sum(R03,   na.rm = TRUE),
    R06   = sum(R06,   na.rm = TRUE),
    t_sales = sum(
      M01AB + M01AE + N02BA + N02BE +
        N05B  + N05C  + R03   + R06,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


#1.Monthly Sales by Drug Category 

#Are the categories structurally similar or very different?

# Convert drug categories to long format
monthly_long <- monthly_sales %>%
  pivot_longer(
    cols = c(M01AB, M01AE, N02BA, N02BE, N05B, N05C, R03, R06,t_sales),
    names_to = "Drug",
    values_to = "Sales"
  )

#plot 
ggplot(monthly_long, aes(x = YearMonth, y = Sales, color = Drug)) +
  geom_line(linewidth = 0.8) +   # Drug categories (thin colored lines)
  scale_color_manual(values = c(
    "M01AB" = "orange", #Anti-inflammatory(Acetic acid derivatives)
    "M01AE" = "red",  #Anti-inflammatory(Propionic acid derivatives)
    "N02BA" = "blue",  #Other analgesics and antipyretics, Salicylic acid and derivatives
    "N02BE" = "green", #Other analgesics and antipyretics, Pyrazolones and Anilides
    "N05B"  = "brown", #Psycholeptics drugs, (Anxiolytic drugs)
    "N05C"  = "purple",#Psycholeptics drugs, (Hypnotics and sedatives drugs)  
    "R03"   = "yellow", #Drugs for obstructive airway diseases 
    "R06"   = "grey" , #Antihistamines for systemic use
    "t_sales"="black"   # Total sales
  )) +
  labs(
    title = "Monthly Sales by Drug Category & Total Sales",
    x = "Date",
    y = "Sales",
    color = "Legend"
  ) +
  theme_minimal()


#Figure 1 presents the evolution of Monthly sales across the eight drug categories
#alongside total sales. While individual categories exhibit distinct variability patterns,
#their aggregated behavior motivates the use of total sales for the primary forecasting analysis.

############################################################################### 
#2. Daily Total Pharmaceutical Sales

ggplot(salesdaily, aes(Date, t_sales)) +
  geom_line() +
  labs(title = "Daily Total Pharmaceutical Sales",
       x="Date",
       y="Total Daily Sales in $")
#It looks stationary no trends but seasonality but the daily sales are to sticky 
#in the plot so i will aggregate to monthly level for better visualization 

############################################################################### 
#3. Monthly Total Pharmaceutical Sales

ggplot(monthly_sales, aes(YearMonth, t_sales)) +
  geom_line() +
  labs(title = "Monthly Total Pharmaceutical Sales",
       x="Date",
       y="Total Monthly Sales in $")

#There is a difference in monthly sales across the year . 
#monthly sales drop in June , July & August , and rise again in October, December & January 

############################################################################### 
#4. Distribution of daily sales

ggplot(model_data, aes(t_sales)) +
  geom_histogram(bins = 40)

#Daily sales are right skewed with the mean 60.5 $ 

############################################################################### 
#5. Weekly seasonality

ggplot(model_data, aes(Weekday, t_sales)) +
  geom_boxplot()
#Daily Sales vary a little bit across the weekdays .    
#Sunday Sundays showed lower median sales with higher variability
#may be because the "Weekend effect" likely reflecting reduced operating hours 
#or demand patterns
############################################################################### 
#6. Monthly pattern

ggplot(salesdaily, aes(factor(Month), t_sales)) +
  geom_boxplot()

#Monthly Sales Median drops from January till the least in July then rise again

############################################################################### 
#7. yearly pattern
ggplot(salesdaily, aes(factor(Year), t_sales)) +
  geom_boxplot()

#Yearly Sales has the Lowest mean in 2017 

############################################################################### 
#8. Rolling mean & volatility (KEY for lag models)

model_data %>%
  mutate(roll_30 = zoo::rollmean(t_sales, 30, fill = NA)) %>%
  ggplot(aes(Date)) +
  geom_line(aes(y = t_sales), alpha = 0.4) +
  geom_line(aes(y = roll_30), color = "blue")

acf(model_data$t_sales, na.action = na.pass)
pacf(model_data$t_sales, na.action = na.pass)

#Spikes at regular lags (Seasonality) 7,14,21,28 --- Weekly Pattern 
#sales depend on last week

############################################################################### 
#9. Zero-sales days analysis

sum(model_data$t_sales == 0)

ggplot(model_data, aes(Date, t_sales == 0)) +
  geom_point(alpha = 0.3)

salesdaily[(salesdaily$t_sales == 0),][c("Date","t_sales")]
#The Pharmacy is closed at specific days in each Year 



###############################################################################
#Daily sale looks Stationary from plot 2 But before using ARIMA model 
#I will confirm this by formal stationarity tests


#1-ADF test (Augmented Dickey-Fuller)
adf.test(model_data$t_sales, alternative = "stationary")

#p-value =0.01
#p-value < 0.05 ---reject H0 --- stationary

#2-KPSS test
kpss.test(model_data$t_sales)

#p-value = 0.06151
#p-value ??? 0.05 ---- fail to reject ---- stationary

#These results jointly indicate that the series is stationary, 
#allowing it to be modeled without differencing.

###############################################################################
#part 4 :Machine learning models  
############################################################################### 

#model 1 (Elastic Net model+Lag)

#step1 :  Add lag features correctly 

enet_model <- model_data %>%
  arrange(Date) %>%
  mutate(
    lag_1  = lag(t_sales, 1),
    lag_7  = lag(t_sales, 7),
    lag_14 = lag(t_sales, 14),
    lag_21 = lag(t_sales, 21),
    lag_30 = lag(t_sales, 30),
    lag_60 = lag(t_sales, 60),
    roll_7  = zoo::rollmean(t_sales, 7, fill = NA, align = "right"),
    roll_30 = zoo::rollmean(t_sales, 30, fill = NA, align = "right")
  )


#step 2 : Re-split and clean
train <- enet_model %>% filter(year(Date) >= 2014 & year(Date) <= 2018)
test  <- enet_model %>% filter(year(Date) == 2019)

train_x <- train %>% 
  select(
    Date,
    t_sales,
    Weekday,
    Month,
    is_closed,
    starts_with("lag_"),
    starts_with("roll_")
  )%>%na.omit()

test_x <- test %>% 
  select(
    Date,
    t_sales,
    Weekday,
    Month,
    is_closed,
    starts_with("lag_"),
    starts_with("roll_")
  )%>%na.omit()

tail(test_x)

#step 3 : train Elastic Net
set.seed(1)

x_train <- model.matrix(t_sales ~ ., train_x)[, -1]
y_train <- train_x$t_sales

x_test <- model.matrix(t_sales ~ ., test_x)[, -1]
y_test <- test_x$t_sales

cv_fit <- cv.glmnet(
  x_train,
  y_train,
  alpha = 0.5   # Elastic Net
)

#step 4 : predict
pred1 <- as.numeric(predict(cv_fit, x_test, s = "lambda.min"))
postResample(pred1, y_test)

#step 5 : Make Elastic Net predictions as table 
ElasticNet_res <- tibble(Date=test_x$Date,
                  Actual= y_test,
                  ElasticNet=as.numeric(pred1))


#step 6 : Plotting 
#Plot Elastic Net predictions VS Actual data

ggplot(ElasticNet_res, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "grey", size = 0.8) +
  geom_line(aes(y = ElasticNet), color = "orange") +
  labs(title = "Elastic Net: Actual vs Predicted",
       y = "Sales in $", x = "Date (2019)") +
  theme_minimal()



#Plot Elastic Net predictions Residuals

residuals_enet <- ElasticNet_res$Actual - ElasticNet_res$ElasticNet

#1-Line plot (Residuals over time)

ggplot(ElasticNet_res, aes(x = Date, y = residuals_enet)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Elastic Net Residuals", y = "Residual", x = "Date") +
  theme_minimal()

#2-Histogram (Residual distribution)

ggplot(ElasticNet_res, aes(x = Actual - ElasticNet)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Elastic Net Residuals Distribution", x = "Residual", y = "Count") +
  theme_minimal()

############################################################################### 
#model 2 (ARIMA)

#step 1:  Train on raw t_sales only
ts_train <- ts(train$t_sales, frequency = 7)
fit_arima <- auto.arima(ts_train, seasonal = TRUE)

#step 2 : Forecast test period

h <- nrow(test)
pred_arima <- forecast(fit_arima, h = h)$mean
postResample(pred_arima, test$t_sales)

#step 3 : Make ARIMA predictions as table 
Arima_res <- tibble(Date=test$Date,
                    ARIMA=as.numeric(pred_arima))

#step 4 : plotting 
#Plot ARIMA predictions VS Actual data

ggplot(Arima_res, aes(x = Date)) +
  geom_line(aes(y = test$t_sales), color = "grey", linewidth = 0.8) +
  geom_line(aes(y = ARIMA), color = "purple") +
  labs(title = "ARIMA: Actual vs Predicted",
       y = "Sales in $", x = "Date (2019)") +
  theme_minimal()

#ARIMA alone without regressors Shows bad prediction of Sales 

#Plot ARIMA predictions Residuals

residuals_arima <- test$t_sales - Arima_res$ARIMA

#1-Line plot (Residuals over time)

ggplot(Arima_res, aes(x = Date, y = residuals_arima)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Plain ARIMA Residuals", y = "Residual", x = "Date") +
  theme_minimal()

#2-Histogram (Residual distribution)

ggplot(Arima_res, aes(x = residuals_arima)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Plain ARIMA Residuals Distribution", x = "Residual", y = "Count") +
  theme_minimal()


############################################################################### 
#model 3 (ARIMAX)

#Step 1: Prepare ARIMAX regressors

# Filter necessary columns
train_arimax <- train %>% select(t_sales, Weekday, Month, is_closed)
test_arimax  <- test  %>% select(t_sales, Weekday, Month, is_closed)

# Create model matrices for ARIMAX
xreg_train <- model.matrix(~ Weekday + Month + is_closed, train_arimax)[, -1]
xreg_test  <- model.matrix(~ Weekday + Month + is_closed, test_arimax)[, -1]

#Step 2: Fit ARIMAX

fit_arimax <- auto.arima(
  ts(train_arimax$t_sales, frequency = 7),  # Weekly frequency
  xreg = xreg_train,
  seasonal = TRUE
)

#Step 3: Predict on test / future data
pred_arimax <- forecast(fit_arimax, xreg = xreg_test)$mean

postResample(pred_arimax, test$t_sales)


#step 4 : Make ARIMAx predictions as table 
Arimax_res <- tibble(Date=test$Date,
                     ARIMAX=as.numeric(pred_arimax))
#step 5 :plotting 

#Plot ARIMAX predictions VS Actual data

ggplot(Arimax_res, aes(x = Date)) +
  geom_line(aes(y = test$t_sales), color = "grey", size = 0.8) +
  geom_line(aes(y = ARIMAX), color = "red") +
  labs(title = "ARIMAX: Actual vs Predicted",
       y = "Sales in $", x = "Date (2019)") +
  theme_minimal()
#adding regressors improve the ARIMA Model sales prediction

#Plot ARIMAX predictions Residuals

residuals_arimax <- test$t_sales - Arimax_res$ARIMAX

#1-Line plot (Residuals over time)

ggplot(Arimax_res, aes(x = Date, y = residuals_arimax)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "ARIMAX Residuals", y = "Residual", x = "Date") +
  theme_minimal()

#2-Histogram (Residual distribution)

ggplot(Arimax_res, aes(x = residuals_arimax)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "ARIMAX Residuals Distribution", x = "Residual", y = "Count") +
  theme_minimal()

############################################################################### 
#model 4 (Prophet)

#step 1 : Build prophet_train including is_closed

prophet_train <- data.frame(
  ds = train$Date,
  y  = train$t_sales,
  is_closed = train$is_closed
)

#step 2 : fit the model 
m <- prophet(
  daily.seasonality  = FALSE,
  weekly.seasonality = TRUE,
  yearly.seasonality = TRUE
)

m <- add_regressor(
  m,
  name = "is_closed",
  standardize = FALSE
)

m <- fit.prophet(m, prophet_train)


future <- data.frame(ds = test$Date,
                     is_closed = test$is_closed)

#step 3 : forecast on test set  

pred_prophet <- predict(m, future)$yhat
postResample(pred_prophet, test$t_sales)

#step 4 : Make prophet predictions as table 
prophet_res <- tibble(Date=test$Date,
                      Prophet=as.numeric(pred_prophet))

#step 5 :plotting 

#Plot Prophet predictions VS Actual data

ggplot(prophet_res, aes(x = Date)) +
  geom_line(aes(y = test$t_sales), color = "grey", size = 0.8) +
  geom_line(aes(y = Prophet), color = "blue") +
  labs(title = "Prophet: Actual vs Predicted",
       y = "Sales in $", x = "Date (2019)") +
  theme_minimal()

#Plot Prophet predictions Residuals

residuals_prophet <- test$t_sales - prophet_res$Prophet

#1-Line plot (Residuals over time)

ggplot(prophet_res, aes(x = Date, y = residuals_prophet)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Prophet Residuals", y = "Residual", x = "Date") +
  theme_minimal()

#2-Histogram (Residual distribution)

ggplot(prophet_res, aes(x = residuals_prophet)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Prophet Residuals Distribution", x = "Residual", y = "Count") +
  theme_minimal()


###############################################################################
#part 4 :Results
############################################################################### 

#Step 1 

#Make a Final results Table for the predictions of all models 

final_results <- ElasticNet_res %>%
  inner_join(Arima_res,  by = "Date") %>%
  inner_join(Arimax_res, by = "Date") %>%
  inner_join(prophet_res, by = "Date")

#make ensemble column for the mean of ARIMAX & Prophet data 
final_results$Ensemble <- (final_results$ARIMAX+final_results$Prophet)/2


# Print results table
head(final_results)

############################################################################### 
#step 2 : Plotting 

#Plot all predictions vs actual on one chart

# Convert to long format for ggplot
final_long <- final_results %>%
  pivot_longer(
    cols = c(Actual, ElasticNet, ARIMA, ARIMAX, Prophet,Ensemble),
    names_to = "Model",
    values_to = "Sales"
  )

# Plot with legend

ggplot(final_long, aes(x = Date, y = Sales, color = Model)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c(
    "Actual" = "grey",
    "ElasticNet" = "orange",
    "ARIMA" = "purple",
    "ARIMAX" = "red",
    "Prophet" = "blue",
    "Ensemble" = "green"
  )) +
  labs(
    title = "Actual vs Predictions: All Models",
    x = "Date",
    y = "Sales",
    color = "Legend"
  ) +
  theme_minimal()

############################################################################### 
#Step 3 : Create Metrics Table to Compare The Accuracy of All Models 

#Functions to detect the accuracy 

#1-RMSE (Root Mean Square Error)
RMSE <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

#2-MAE (Mean Absolute Error)

MAE <- function(actual, pred) {
  mean(abs(actual - pred))
}


#3-MAPE (Mean Absolute Percentage Error)

MAPE <- function(actual, pred) {
  idx <- actual != 0
  mean(abs((actual[idx] - pred[idx]) / actual[idx])) * 100
}



#Compute metrics for all models

metrics <- tibble(
  Model = c("Elastic Net", "ARIMA", "ARIMAX", "Prophet","Ensemble"),
  
  RMSE = c(
    RMSE(final_results$Actual, final_results$ElasticNet),
    RMSE(final_results$Actual, final_results$ARIMA),
    RMSE(final_results$Actual, final_results$ARIMAX),
    RMSE(final_results$Actual, final_results$Prophet),
    RMSE(final_results$Actual, final_results$Ensemble)
  ),
  
  MAE = c(
    MAE(final_results$Actual, final_results$ElasticNet),
    MAE(final_results$Actual, final_results$ARIMA),
    MAE(final_results$Actual, final_results$ARIMAX),
    MAE(final_results$Actual, final_results$Prophet),
    MAE(final_results$Actual, final_results$Ensemble)
  ),
  
  MAPE = c(
    MAPE(final_results$Actual, final_results$ElasticNet),
    MAPE(final_results$Actual, final_results$ARIMA),
    MAPE(final_results$Actual, final_results$ARIMAX),
    MAPE(final_results$Actual, final_results$Prophet),
    MAPE(final_results$Actual, final_results$Ensemble)
  )
)

#Show the Matrics

knitr::kable(metrics)

#conclusion

#1-Elastic Net + lags + calendar features ------- best accuracy, shorter horizon

#2-Prophet + is_closed -------- robust, handles holidays well

#3-Ensemble ----------Decent, but worse than Prophet

#4-ARIMAX (Month + lags + is_closed) ------ Not helping much (exogenous vars weak)

#5-Plain ARIMA ------- weakest (no external info)


###############################################################################
#part 5 :Forecast Nov-Dec (2019) + Full (2020)
############################################################################### 

#first of all we want to forecast the 2020 year sales the models made for Long-Term forecasting 
#here are 
#1-ARIMAX
#2-prophet 

#but they need is_closure column in the dataset which based on the official holidays 
#to find the closure days in 2020 we need to analyze the closure in the previous data 

#step 1 :Create a clear holiday category structure

previous_closure <- salesdaily[(salesdaily$t_sales==0),c("Date","Weekday","Month")]

#after searching for the official holidays in Serbia (where the pharmacy data collected)

#Matched 21 / 26 zero-sales days to official Serbian holidays


previous_closure$Holiday <- c("Christmas Day", "Easter Day", "Labor holiday", "Saint Nicholas Day",
                              "Western New Year's Day", "Christmas Day", "Easter Day", "Saint Nicholas Day",
                              "Western New Year's Day", "Christmas Day", "Labor holiday", "Saint Nicholas Day",
                              "", "Easter Day", "", "Saint Nicholas Day", "Western New Year's Day",
                              "Christmas Day", "Easter Day", "", "", "Saint Nicholas Day", "Western New Year's Day",
                              "Christmas Day", "", "Easter Day")



#there are 5 days the pharmacy closed with unexplained closures 

previous_closure$Holiday[previous_closure$Holiday == ""] <- "unexplained closures"


############################################################################### 
#step 2 : build the 2020 future dataset with the needed columns to run the ARIMAX & prophet models 


#1- build holidays_2020 dataframe for the is_closed column

holidays_2020 <- data.frame(
  Date = as.Date(c("2019-12-19","2020-01-01", "2020-01-07", "2020-04-19", "2020-05-01",
                   "2020-12-19")),
  Holiday = c("Saint Nicholas Day","Western New Year's Day","Christmas Day", "Easter Day", "Labor holiday", "Saint Nicholas Day")
)



#2- Build 2020 future dataset
future_2020 <- data.frame(
  Date = seq.Date(from = as.Date("2019-11-01"), 
                  to   = as.Date("2020-12-31"), 
                  by   = "day")
)

#3- Add additional columns needed for ARIMAX/Prophet
future_2020 <- future_2020 %>%
  mutate(
    Weekday = factor(weekdays(Date)),
    Month   = as.numeric(format(Date, "%m")),
    is_closed = ifelse(Date %in% as.Date(holidays_2020$Date), 1, 0)
  )

head(future_2020)

############################################################################### 
#step 3 : Run the Models 

#1- Run ARIMAX Model 
xreg_2020  <- model.matrix(~ Weekday + Month + is_closed, future_2020)[, -1]
pred_arimax_2020 <- forecast(fit_arimax, xreg = xreg_2020)$mean



#2-Run Prophet Model 

p_2020 <- data.frame(ds = future_2020$Date,
                     is_closed = future_2020$is_closed)


pred_prophet_2020 <- predict(m, p_2020)$yhat


############################################################################### 
#step 4 : add model predictions to future 2020 dataframe 

future_2020$f_arimax <- pred_arimax_2020

future_2020$f_prophet <- pred_prophet_2020

future_2020$ensemble <- (pred_arimax_2020+pred_prophet_2020)/2

#Equal-weight ensemble chosen due to similar validation performance and lack of prior preference.

head(future_2020)

############################################################################### 
#step 5 :plot all 2020 predictions 

# Convert future_2020 to long format for ggplot
future_long <- future_2020 %>%
  pivot_longer(
    cols = c(f_prophet, f_arimax, ensemble),
    names_to = "Model",
    values_to = "Sales"
  )

# Convert 2019 actual sales to long format
actual_2019_long <- test %>%
  select(Date, t_sales) %>%
  mutate(Model = "Actual_2019", Sales = t_sales) %>%
  select(Date, Model, Sales)

# Combine with 2020 predictions
plot_data <- bind_rows(future_long, actual_2019_long)

#Plot  the Forecasted Daily Sales 
ggplot(plot_data, aes(x = Date, y = Sales, color = Model)) +
  geom_line(size = 0.8,alpha = 0.6) +
  scale_color_manual(values = c(
    "ensemble" = "green",
    "f_arimax" = "red",
    "f_prophet" = "blue",
    "Actual_2019" = "grey"
  )) +
   geom_vline(
    xintercept = as.Date("2019-11-01"),
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    title = "Forecasted 2020 Sales with 2019 Actuals",
    x = "Date",
    y = "Sales",
    color = "Legend"
  ) +
  annotate(
  "text",
  x = as.Date("2019-11-01"),
  y = max(plot_data$Sales, na.rm = TRUE),
  label = "Forecast start",
  vjust = -0.5,
  hjust = 0,
  size = 3
 )+
  theme_minimal()


############################################################################### 
#step 6 : Aggregate the forecasted sales data in Weekly and Monthly level to help in Purchasing Planning

#1-Create The Monthly_forecast Dataset
monthly_forecast <- future_2020 %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarize(
    ARIMAX = sum(f_arimax),
    Prophet = sum(f_prophet),
    Ensemble = sum(ensemble),
    .groups = "drop") %>%
  tibble()


#2-Round values 

monthly_forecast <- monthly_forecast %>%
  mutate(
    across(
      c(ARIMAX, Prophet, Ensemble),
      \(x) round(x, 0)
    )
  )


#3-Add Month name (more readable for business users)

monthly_forecast <- monthly_forecast %>%
  mutate(
    Month_Name = month(
      as.Date(paste(Year, Month, 1, sep = "-")),
      label = TRUE,
      abbr = TRUE
    )
  ) %>%
  select(Year, Month, Month_Name, everything())

monthly_forecast

#4-Add Weekly aggregation 

weekly_forecast <- future_2020 %>%
  mutate(
    Week_Start = floor_date(Date, unit = "week", week_start = 1)
  ) %>%
  group_by(Week_Start) %>%
  summarize(
    ARIMAX = sum(f_arimax),
    Prophet = sum(f_prophet),
    Ensemble = sum(ensemble),
    .groups = "drop"
  )

#Daily forecasts were aggregated to monthly and weekly levels to support inventory
#planning and procurement decisions. An ensemble forecast was included to reduce
#model-specific bias and improve robustness.


############################################################################### 
#step 7 : save the final forecasted sales into .csv files 

#Make sure the folder exists
if (!dir.exists("output")) dir.create("output")

#save Daily Forecast 
Daily_forecast_export <- future_2020 %>%
  select(Date,ensemble)%>%
  rename(Planned_Sales = ensemble)%>%
  arrange(Date)

write.csv(
  Daily_forecast_export,
  "output/Daily_forecast_2020.csv",
  row.names = FALSE
)
#save Weekly Forecast
weekly_forecast_export <- weekly_forecast %>%
  select(Week_Start,Ensemble)%>%
  rename(Planned_Sales = Ensemble)%>%
  arrange(Week_Start)

write.csv(
  weekly_forecast_export,
  "output/weekly_forecast_2020.csv",
  row.names = FALSE
)

#save Monthly Forecast
monthly_forecast_export <- monthly_forecast %>%
  select(Year,Month_Name,Ensemble)%>%
  rename(Planned_Sales = Ensemble,Month=Month_Name)%>%
  arrange(Year, Month)

write.csv(
  monthly_forecast_export,
  "output/monthly_forecast_2020.csv",
  row.names = FALSE
)
