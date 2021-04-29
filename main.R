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

p <- df %>%
  ggplot(aes(x=date, y=sales_value)) +
  geom_line(color="#69b3a2") +
  xlab("") +
  ylab("Sales") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

rf <- randomForest(
  sales_value ~ .,
  data=df,
  ntree=500,
  mtry=2
)

rf

tb <- data.frame(df$sales_value, rf$predicted)