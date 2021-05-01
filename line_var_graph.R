# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(randomForest)

require(smooth)
require(Mcomp)
require(caTools)

df <- read.csv("~/RProjects/Dima_diploma/new_data.csv")

df$date <- ymd(df$date)
df$non_corporate_sales_value <- (df$sales_value - df$sales_corporate_value)

df$sales_sma <- as.numeric(sma(df$non_corporate_sales_value, h=22, silent=FALSE)$fitted)
df$sales_ets <- as.numeric(es(df$non_corporate_sales_value, h=22, silent=FALSE)$fitted)

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

prices <- read.csv("~/RProjects/Dima_diploma/purchases.csv")
prices$date <- ymd(prices$date)

filtered_prices <- prices %>% 
  filter(cost > 200) %>%
  group_by(date) %>%
  summarise(price = mean(cost))

filtered_prices$price_sma <- as.numeric(sma(filtered_prices$price, h=104, silent=FALSE)$fitted)
filtered_prices$price_ets <- as.numeric(es(filtered_prices$price, model="ANN", h=104, silent=FALSE)$fitted)

p <- filtered_prices %>%
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