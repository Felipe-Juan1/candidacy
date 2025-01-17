install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
library(readr)
library(tidyverse)
library(dplyr)

## 3. Import and Transform the data
## 3.1 The R file name is renamed to "import.R"

## 3.2 Importing the file
dataframe<-read_csv(file="Homework3/NFCS 2009 State Data 220712 v2.csv")

## 3.3

## A

## A3Ar_w is the name of the variable given in dataset, I created a new variable
## and named it 'agegroup' and the value are replaced by labels.
## The pdf used was  NFCS 2009 State Data File Info 220712 v2.pdf

dataframe<-dataframe %>%
  mutate(agegroup = case_when(A3Ar_w == 1 ~ "18-24",
                             A3Ar_w == 2 ~ "25-34",
                             A3Ar_w == 3 ~ "35-44",
                             A3Ar_w == 4 ~ "45-54",
                             A3Ar_w == 5 ~ "55-64",
                             A3Ar_w == 6 ~ "65+"
                             ))
## 3.3.B

## Method 1
## Giving the value 1 for the correct answer and 0 for all other remaining answer
## including "prefer not to say"

# Interest earnings question
# dataframe <- dataframe %>%
#  mutate ( interest_earnings_correct = if_else(M6 %in% c(1), 1, 0))

# Inflation losses question
# dataframe <- dataframe %>%
#  mutate ( inflation_loses_correct = if_else(M7 %in% c(3), 1, 0 ))

# Stock market risk question
# dataframe <- dataframe %>%
#  mutate ( stockrisk_correct = if_else (M10 %in% c(2), 1, 0))

# C: Created new variable financial literacy score
# dataframe <- dataframe %>%
#  mutate (fin_literacy =((interest_earnings_correct + inflation_loses_correct + stockrisk_correct)/ 3))

# 4
# dataclean<-select(.data=dataframe,agegroup, interest_earnings_correct, inflation_loses_correct, stockrisk_correct, fin_literacy, NFCSID )
# write.csv(data_clean,"Homework3/dataclean.csv")

######### Another way:
## 1 More than $102       [1]
## 2 Exactly $102         [0]
## 3 Less than $102       [0]
## 98 Don't know          [0]
## 99 Prefer not to say   [NA]

# 3.3 B [Another way]
dataframe <- dataframe %>%
  mutate(
    interest_correct = case_when(
      M6 == 1 ~ 1,
      M6 %in% c(2,3,98) ~ 0,
      M6 == 99 ~ NA_real_,
      TRUE ~ NA_real_                      
    ))

table(dataframe$interest_correct)

########

## 1 More than today      [0]
## 2 Exactly the same     [0]
## 3 Less than today      [1]
## 98 Don't know          [0]
## 99 Prefer not to say   [NA]

dataframe <- dataframe %>%
  mutate(
    inf_loss_correct = case_when(
      M7 %in% c(1,2,98) ~ 0,
      M7 == 3 ~ 1,
      M7 == 99 ~ NA_real_,
      TRUE ~ NA_real_ ))

table(dataframe$inf_loss_correct)

########

## 1 True                 [0]
## 2 False                [1]
## 98 Don't know          [0]
## 99 Prefer not to say   [NA]


dataframe <- dataframe %>%
  mutate(
    stock_correct = case_when(
      M10 %in% c(1,98) ~ 0,
      M10 == 2 ~ 1,
      M10 == 99 ~ NA_real_,
      TRUE ~ NA_real_ ))

table(dataframe$stock_correct)

########
## 3.3 C
## Creating the variable with combined financial literacy score
dataframe<-dataframe %>%
 mutate(fin_literacy=(interest_correct+inf_loss_correct+stock_correct)/3)

## 4
## Exporting the clean data to new csv file named "data_clean.csv"
## Subset data to 6 variables
data_clean<-select(.data=dataframe,agegroup, interest_correct, inf_loss_correct, stock_correct, fin_literacy, NFCSID )

write.csv(data_clean,"Homework3/data_clean.csv")

#######
## The variables created by second method 
## (giving the value 1 to correct answer, 0 to incorrect answers and NA to answer choice 'prefer not to say') 
## will be used in further questions (question 4 and 5)

#######


