install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
library(readr)
library(tidyverse)
library(dplyr)


read.csv("Homework3/data_clean.csv")

data_clean %>%
  summarize(
    count_int = n(),
    mean = mean(interest_correct, na.rm = TRUE),
    median = median(interest_correct, na.rm = TRUE),
    min= min(interest_correct, na.rm = TRUE),
    max = max(interest_correct, na.rm = TRUE),
    sd = sd(interest_correct, na.rm = TRUE)
  )

data_clean %>%
  summarize(
    count = n(),
    mean = mean(inf_loss_correct, na.rm = TRUE),
    median = median(inf_loss_correct, na.rm = TRUE),
    min = min(inf_loss_correct, na.rm = TRUE),
    max = max(inf_loss_correct, na.rm = TRUE),
    sd = sd(inf_loss_correct, na.rm = TRUE)
  )


data_clean %>%
  summarize(
    count = n(),
    mean = mean(stock_correct, na.rm = TRUE),
    median = median(stock_correct, na.rm = TRUE),
    min = min(stock_correct, na.rm = TRUE),
    max = max(stock_correct, na.rm = TRUE),
    sd = sd(stock_correct, na.rm = TRUE)
  )


data_clean %>%
  summarize(
    count = n(),
    mean = mean(fin_literacy, na.rm = TRUE),
    median = median(fin_literacy, na.rm = TRUE),
    min = min(fin_literacy, na.rm = TRUE),
    max = max(fin_literacy, na.rm = TRUE),
    sd = sd(fin_literacy, na.rm = TRUE)
  )

#############
## Calculating the summary statistics based on different age group

datafinal<-data_clean %>%
  group_by(agegroup) %>%
  summarize(
    count_int = n(),
    mean_int = mean(interest_correct, na.rm = TRUE),
    median_int = median(interest_correct, na.rm = TRUE),
    min_int = min(interest_correct, na.rm = TRUE),
    max_int = max(interest_correct, na.rm = TRUE),
    sd_int = sd(interest_correct, na.rm = TRUE)
  )

datafinal<-data_clean %>%
  group_by(agegroup) %>%
  summarize(
    count_inf = n(),
    mean_inf = mean(inf_loss_correct, na.rm = TRUE),
    median_inf = median(inf_loss_correct, na.rm = TRUE),
    min_inf = min(inf_loss_correct, na.rm = TRUE),
    max_inf = max(inf_loss_correct, na.rm = TRUE),
    sd_inf = sd(inf_loss_correct, na.rm = TRUE)
  )

datafinal<-data_clean %>%
  group_by(agegroup) %>%
  summarize(
    count_stock = n(),
    mean_stock = mean(stock_correct, na.rm = TRUE),
    median_stock = median(stock_correct, na.rm = TRUE),
    min_stock = min(stock_correct, na.rm = TRUE),
    max_stock = max(stock_correct, na.rm = TRUE),
    sd_stock = sd(stock_correct, na.rm = TRUE)
  )

datafinal<-data_clean %>%
  group_by(agegroup) %>%
  summarize(
    count_fin = n(),
    mean_fin = mean(fin_literacy, na.rm = TRUE),
    median_fin = median(fin_literacy, na.rm = TRUE),
    min_fin = min(fin_literacy, na.rm = TRUE),
    max_fin = max(fin_literacy, na.rm = TRUE),
    sd_fin = sd(fin_literacy, na.rm = TRUE)
  )
################################################

#############
## Calculating the summary statistics based on different age group

datafinal<-data_clean %>%
  group_by(agegroup) %>%
  summarize(
    count_int = n(),
    mean_int = mean(interest_correct, na.rm = TRUE),
    median_int = median(interest_correct, na.rm = TRUE),
    min_int = min(interest_correct, na.rm = TRUE),
    max_int = max(interest_correct, na.rm = TRUE),
    sd_int = sd(interest_correct, na.rm = TRUE),
    count_inf = n(),
    mean_inf = mean(inf_loss_correct, na.rm = TRUE),
    median_inf = median(inf_loss_correct, na.rm = TRUE),
    min_inf = min(inf_loss_correct, na.rm = TRUE),
    max_inf = max(inf_loss_correct, na.rm = TRUE),
    sd_inf = sd(inf_loss_correct, na.rm = TRUE),
    count_stock = n(),
    mean_stock = mean(stock_correct, na.rm = TRUE),
    median_stock = median(stock_correct, na.rm = TRUE),
    min_stock = min(stock_correct, na.rm = TRUE),
    max_stock = max(stock_correct, na.rm = TRUE),
    sd_stock = sd(stock_correct, na.rm = TRUE),
    count_fin = n(),
    mean_fin = mean(fin_literacy, na.rm = TRUE),
    median_fin = median(fin_literacy, na.rm = TRUE),
    min_fin = min(fin_literacy, na.rm = TRUE),
    max_fin = max(fin_literacy, na.rm = TRUE),
    sd_fin = sd(fin_literacy, na.rm = TRUE)
  )
View(datafinal)
################################################
