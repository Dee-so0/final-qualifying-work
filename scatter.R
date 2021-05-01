# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(randomForest)

require(caTools)

df <- read.csv("~/RProjects/Dima_diploma/data.csv")

df$date <- ymd(df$date)

df$sales_value <- (df$sales_value - mean(df$sales_value))/sd(df$sales_value)
df$views_value <- (df$views_value - mean(df$views_value))/sd(df$views_value)
df$shopping_cart_value <- (df$shopping_cart_value - mean(df$shopping_cart_value))/sd(df$shopping_cart_value)
df$shopping_cart_count <- (df$shopping_cart_count - mean(df$shopping_cart_count))/sd(df$shopping_cart_count)
df$views_count <- (df$views_count - mean(df$views_count))/sd(df$views_count)
df$favorite_count <- (df$favorite_count - mean(df$favorite_count))/sd(df$favorite_count)
df$favorite_value <- (df$favorite_value - mean(df$favorite_value))/sd(df$favorite_value)

p <- df %>%
  ggplot(aes(x=favorite_value, y=sales_value)) + 
  geom_point() +
  xlab("Стоимость добавленного в избранное") +
  ylab("Продажи")

p

p <- ggplotly(p)

p