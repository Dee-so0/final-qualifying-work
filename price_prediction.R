# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(randomForest)

require(caTools)
require(smooth)
require(Mcomp)

# Read data
df <- read.csv("~/RProjects/Dima_diploma/new_data.csv")

df$date <- ymd(df$date)

# Calculate non-corporate sales
df$non_corporate_sales_value <- (df$sales_value - df$sales_corporate_value)

# Smooth sales
df$sales_sma <- as.numeric(sma(df$non_corporate_sales_value, h=22, silent=FALSE)$fitted)
df$sales_ets <- as.numeric(es(df$non_corporate_sales_value, h=22, silent=FALSE)$fitted)

# Plot smoothed sales
p <- df %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=sales_sma, color="SMA")) +
  geom_line(aes(y=sales_ets, color="ETS")) +
  scale_color_manual(name = "Method", 
                     values = c("SMA" = "#69b3a2", "ETS" = "#b3697a")) +
  xlab("") +
  ylab("Sales") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Read prices
prices <- read.csv("~/RProjects/Dima_diploma/purchases.csv")
prices$date <- ymd(prices$date)

# Filter prices and calculate mean
filtered_prices <- prices %>% 
                          filter(cost >= 200) %>%
                          group_by(date) %>%
                          summarise(price = mean(cost))

# Plot filtered prices
p <- filtered_prices %>%
  ggplot(aes(x=date, y=price)) +
  geom_line(color="#69b3a2") +
  xlab("") +
  ylab("Mean_Price") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Merge prices and sales
df <- merge(df, filtered_prices, by.x = "date", by.y = "date", all.x = TRUE, all.y = FALSE)

# Variables normalization
df$sales_value <- (df$sales_value - mean(df$sales_value))/sd(df$sales_value)
df$views_value <- (df$views_value - mean(df$views_value))/sd(df$views_value)
df$shopping_cart_value <- (df$shopping_cart_value - mean(df$shopping_cart_value))/sd(df$shopping_cart_value)
df$shopping_cart_count <- (df$shopping_cart_count - mean(df$shopping_cart_count))/sd(df$shopping_cart_count)
df$views_count <- (df$views_count - mean(df$views_count))/sd(df$views_count)
df$favorite_count <- (df$favorite_count - mean(df$favorite_count))/sd(df$favorite_count)
df$favorite_value <- (df$favorite_value - mean(df$favorite_value))/sd(df$favorite_value)
df$non_corporate_sales_value <- (df$non_corporate_sales_value - mean(df$non_corporate_sales_value))/sd(df$non_corporate_sales_value)
df$search_queries_count <- (df$search_queries_count - mean(df$search_queries_count))/sd(df$search_queries_count)
df$sales_sma <- (df$sales_sma - mean(df$sales_sma))/sd(df$sales_sma)
df$sales_ets <- (df$sales_ets - mean(df$sales_ets))/sd(df$sales_ets)

# Remove NA values
df_filtered <- df %>%
                  filter(!is.na(price))

# Filter insignificant variables
df_filtered <- subset(df_filtered, select = -c(sales_corporate_value, sales_corporate_count, sales_sma, non_corporate_sales_value, sales_value, sales_count, online_requests_count))

summary(df_filtered)

# Random forest price prediction
price_rf <- randomForest(
  price ~ .,
  data=df_filtered,
  ntree=500,
  mtry=2
)

price_rf

df$price <- predict(price_rf, newdata = df)

# Plot predicted prices
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=price), color="#69b3a2") +
  xlab("") +
  ylab("Price_predicted") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Smooth price
df$price_sma <- as.numeric(sma(df$price, h=104, silent=FALSE)$fitted)
df$price_ets <- as.numeric(es(df$price, model="ANN", h=104, silent=FALSE)$fitted)

# Plotting smoothed price
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=price_sma, color="SMA")) +
  geom_line(aes(y=price_ets, color="ETS")) +
  scale_color_manual(name = "Method", 
                     values = c("SMA" = "#69b3a2", "ETS" = "#b3697a")) +
  xlab("") +
  ylab("Prices") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


p

p <- ggplotly(p)

p

# Normalize predicted prices
df$price <- (df$price - mean(df$price))/sd(df$price)
df$price_sma <- (df$price_sma - mean(df$price_sma))/sd(df$price_sma)
df$price_ets <- (df$price_ets - mean(df$price_ets))/sd(df$price_ets)

# Filter complements prices and calculate mean
filtered_complements_prices <- prices %>%
  filter(cost < 200) %>%
  group_by(date) %>%
  summarise(complements_price = mean(cost))

# Plot filtered complements prices
p <- filtered_complements_prices %>%
  ggplot(aes(x=date, y=complements_price)) +
  geom_line(color="#69b3a2") +
  xlab("") +
  ylab("Mean_Complements_Price") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Merge complements prices and sales
df <- merge(df, filtered_complements_prices, by.x = "date", by.y = "date", all.x = TRUE, all.y = FALSE)

# Remove NA values
df_filtered <- df %>%
  filter(!is.na(complements_price))

# Filter insignificant variables
df_filtered <- subset(df_filtered, select = -c(sales_corporate_value, sales_corporate_count, sales_sma, non_corporate_sales_value, sales_value, sales_count, online_requests_count, price, price_sma))

summary(df_filtered)

# Random forest price prediction
complements_price_rf <- randomForest(
  complements_price ~ .,
  data=df_filtered,
  ntree=500,
  mtry=2
)

complements_price_rf

df$complements_price <- predict(complements_price_rf, newdata = df)

# Plot predicted complements prices
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=complements_price), color="#69b3a2") +
  xlab("") +
  ylab("Complements_Price_predicted") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Smooth complements price
df$complements_price_sma <- as.numeric(sma(df$complements_price, h=69, silent=FALSE)$fitted)
df$complements_price_ets <- as.numeric(es(df$complements_price, model="ANN", h=69, silent=FALSE)$fitted)

# Plotting smoothed complements price
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=complements_price_sma, color="SMA")) +
  geom_line(aes(y=complements_price_ets, color="ETS")) +
  scale_color_manual(name = "Method",
                     values = c("SMA" = "#69b3a2", "ETS" = "#b3697a")) +
  xlab("") +
  ylab("Complements Prices") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


p

p <- ggplotly(p)

p

# Normalize predicted complements prices
df$complements_price <- (df$complements_price - mean(df$complements_price))/sd(df$complements_price)
df$complements_price_sma <- (df$complements_price_sma - mean(df$complements_price_sma))/sd(df$complements_price_sma)
df$complements_price_ets <- (df$complements_price_ets - mean(df$complements_price_ets))/sd(df$complements_price_ets)

# Read USD/UZS course
usd <- read.csv("~/RProjects/Dima_diploma/USD.csv")

usd$date <- ymd(usd$date)

# Merge USD/UZS and sales
df <- merge(df, usd, by.x = "date", by.y = "date", all.x = TRUE, all.y = FALSE)

# Remove NA values
df_filtered <- df %>%
  filter(!is.na(course))

# Filter insignificant variables
df_filtered <- subset(df_filtered, select = -c(sales_corporate_value, sales_corporate_count, sales_sma, non_corporate_sales_value, sales_value, sales_count, online_requests_count, price, price_sma, complements_price, complements_price_sma))

summary(df_filtered)

# Random forest price prediction
usd_rf <- randomForest(
  course ~ .,
  data=df_filtered,
  ntree=500,
  mtry=2
)

usd_rf

df$course <- predict(usd_rf, newdata = df)

# Plot predicted USD/UZS course
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=course), color="#69b3a2") +
  xlab("") +
  ylab("USD/UZS predicted") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Normalize USD/UZS course value
df$course <- (df$course - mean(df$course))/sd(df$course)


# Read credit rate
credit <- read.csv("~/RProjects/Dima_diploma/credit.csv")

credit$date <- ymd(credit$date)

# Merge credit rate and sales
df <- merge(df, credit, by.x = "date", by.y = "date", all.x = TRUE, all.y = FALSE)

# Remove NA values
df_filtered <- df %>%
  filter(!is.na(credit_rate))

# Filter insignificant variables
df_filtered <- subset(df_filtered, select = -c(sales_corporate_value, sales_corporate_count, sales_sma, non_corporate_sales_value, sales_value, sales_count, online_requests_count, price, price_sma, complements_price, complements_price_sma))

summary(df_filtered)

# Random forest price prediction
credit_rf <- randomForest(
  credit_rate ~ .,
  data=df_filtered,
  ntree=500,
  mtry=3
)

credit_rf

df$credit_rate <- predict(credit_rf, newdata = df)

# Plot predicted credit_rate course
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=credit_rate), color="#69b3a2") +
  xlab("") +
  ylab("Credit rate predicted") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Smooth credit rate
df$credit_rate_sma <- as.numeric(sma(df$credit_rate, h=3, silent=FALSE)$fitted)
df$credit_rate_ets <- as.numeric(es(df$credit_rate, model="ANN", h=3, silent=FALSE)$fitted)

# Plotting smoothed credit_rate
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=credit_rate_sma, color="SMA")) +
  geom_line(aes(y=credit_rate_ets, color="ETS")) +
  scale_color_manual(name = "Method",
                     values = c("SMA" = "#69b3a2", "ETS" = "#b3697a")) +
  xlab("") +
  ylab("Smoothed credit rate") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


p

p <- ggplotly(p)

p

# Normalize credit rate course value
df$credit_rate <- (df$credit_rate - mean(df$credit_rate))/sd(df$credit_rate)
df$credit_rate_sma <- (df$credit_rate_sma - mean(df$credit_rate_sma))/sd(df$credit_rate_sma)
df$credit_rate_ets <- (df$credit_rate_ets - mean(df$credit_rate_ets))/sd(df$credit_rate_ets)

# Read inflation
inflation <- read.csv("~/RProjects/Dima_diploma/inflation.csv")

inflation$date <- ymd(inflation$date)

# Merge inflation and sales
df <- merge(df, inflation, by.x = "date", by.y = "date", all.x = TRUE, all.y = FALSE)

# Remove NA values
df_filtered <- df %>%
  filter(!is.na(previous_month))

# Filter insignificant variables
df_filtered <- subset(df_filtered, select = -c(sales_corporate_value, sales_corporate_count, sales_sma, non_corporate_sales_value, sales_value, sales_count, online_requests_count, price, price_sma, complements_price, complements_price_sma, credit_rate, credit_rate_sma, previous_year))

summary(df_filtered)

# Random forest price prediction
inflation_rf <- randomForest(
  previous_month ~ .,
  data=df_filtered,
  ntree=500,
  mtry=3
)

inflation_rf

df$previous_month <- predict(inflation_rf, newdata = df)

# Plot predicted inflation
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=previous_month), color="#69b3a2") +
  xlab("") +
  ylab("Inlfation predicted") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

# Smooth inflation
df$inflation_sma <- as.numeric(sma(df$previous_month, h=3, silent=FALSE)$fitted)
df$inflation_ets <- as.numeric(es(df$previous_month, model="ANN", h=3, silent=FALSE)$fitted)

# Plotting smoothed inflation
p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=inflation_sma, color="SMA")) +
  geom_line(aes(y=inflation_ets, color="ETS")) +
  scale_color_manual(name = "Method",
                     values = c("SMA" = "#69b3a2", "ETS" = "#b3697a")) +
  xlab("") +
  ylab("Smoothed inflation") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


p

p <- ggplotly(p)

p

# Normalize inflation value
df$previous_month <- (df$previous_month - mean(df$previous_month))/sd(df$previous_month)
df$inflation_sma <- (df$inflation_sma - mean(df$inflation_sma))/sd(df$inflation_sma)
df$inflation_ets <- (df$inflation_ets - mean(df$inflation_ets))/sd(df$inflation_ets)

# Remove insignificant variables
df <- subset(df, select = -c(sales_corporate_value, sales_corporate_count, sales_value, sales_count, online_requests_count, sales_sma, price, price_sma, complements_price, complements_price_sma, credit_rate, credit_rate_sma, previous_year, previous_month, inflation_sma))

# Random forest sales prediction model
sales_rf <- randomForest(
  sales_ets ~ .,
  data=df,
  ntree=1000,
  mtry=4
)

sales_rf