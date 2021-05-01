# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(plotly)
library(randomForest)

require(caTools)

df <- read.csv("~/RProjects/Dima_diploma/train.csv")

df$date <- ymd(df$date)

df_new <- df %>%
          group_by(date) %>%
          summarise(sales = sum(sales))

df_new$sales <- (df_new$sales - mean(df_new$sales))/sd(df_new$sales)

p <- df_new %>%
  ggplot(aes(x=date, y=sales)) +
  geom_line(color="#69b3a2") +
  xlab("") +
  ylab("Sales") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

rf <- randomForest(
  sales ~ .,
  data=df_new,
  ntree=500,
  mtry=1
)

rf

rf$predicted[is.na(rf$predicted)] = mean(df_new$sales, na.rm=TRUE)

tb <- data.frame(df_new$sales, rf$predicted)