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

df <- read.csv("~/RProjects/Dima_diploma/inflation.csv")

df$date <- ymd(df$date)

p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=previous_month), color="#69b3a2") +
  xlab("") +
  ylab("inflation") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

df <- read.csv("~/RProjects/Dima_diploma/credit.csv")

df$date <- ymd(df$date)

p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=credit_rate), color="#69b3a2") +
  xlab("") +
  ylab("credit rate") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p