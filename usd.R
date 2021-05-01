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

df <- read.csv("~/RProjects/Dima_diploma/USD.csv")

df$date <- ymd(df$date)

p <- df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=course), color="#69b3a2") +
  xlab("") +
  ylab("USD/UZS") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

p

p <- ggplotly(p)

p

df$course_sma <- as.numeric(sma(df$course, h=1, silent=FALSE)$fitted)
df$course_ets <- as.numeric(es(df$course, model="ANN", h=1, silent=FALSE)$fitted)

p <- df %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=course_sma, color="SMA")) +
  geom_line(aes(y=course_ets, color="ETS")) +
  scale_color_manual(name = "Method", 
                     values = c("SMA" = "#69b3a2", "ETS" = "#b3697a")) +
  xlab("") +
  ylab("USD/UZS") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


p

p <- ggplotly(p)

p