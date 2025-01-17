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

## 
## Giving the value 1 for the correct answer and 0 for incorrect answers

# Interest earnings question
dataframe <- dataframe %>%
  mutate ( interest_earnings_correct = if_else(M6 %in% c(1), 1, 0))

# Inflation losses question
dataframe <- dataframe %>%
  mutate ( inflation_loses_correct = if_else(M7 %in% c(3), 1, 0 ))

# Stock market risk question
dataframe <- dataframe %>%
  mutate ( stockrisk_correct = if_else (M10 %in% c(2), 1, 0))


######### Another way of assigning values 
# Giving value 1 to correct answer, value 0 to incorrect answers and NA to 'prefer not to say' 
## 1 More than $102       [1]
## 2 Exactly $102         [0]
## 3 Less than $102       [0]
## 98 Don't know          [0]
## 99 Prefer not to say   [NA]

# 3.3 B [Another way]
#dataframe <- dataframe %>%
#  mutate(
#    interest_correct = case_when(
#      M6 == 1 ~ 1,
#      M6 %in% c(2,3,98) ~ 0,
#      M6 == 99 ~ NA_real_,
#      TRUE ~ NA_real_                      
#    ))


## 1 More than today      [0]
## 2 Exactly the same     [0]
## 3 Less than today      [1]
## 98 Don't know          [0]
## 99 Prefer not to say   [NA]

#dataframe <- dataframe %>%
#  mutate(
#    inf_loss_correct = case_when(
#      M7 %in% c(1,2,98) ~ 0,
#      M7 == 3 ~ 1,
#      M7 == 99 ~ NA_real_,
#      TRUE ~ NA_real_ ))


## 1 True                 [0]
## 2 False                [1]
## 98 Don't know          [0]
## 99 Prefer not to say   [NA]


#dataframe <- dataframe %>%
#  mutate(
#    stock_correct = case_when(
#      M10 %in% c(1,98) ~ 0,
#      M10 == 2 ~ 1,
#      M10 == 99 ~ NA_real_,
#      TRUE ~ NA_real_ ))


# C: Created new variable financial literacy score
dataframe <- dataframe %>%
  mutate (fin_literacy =((interest_earnings_correct + inflation_loses_correct + stockrisk_correct)/ 3))

# 4
# Subset data to 6 variables
dataclean<-select(.data=dataframe,agegroup, interest_earnings_correct, inflation_loses_correct, stockrisk_correct, fin_literacy, NFCSID )

# Exporting the clean data to new csv file named "data_clean.csv"
write.csv(dataclean,"Homework3/dataclean.csv")




################################################################################
################################################################################


########################       Assignment 5     ################################


################################################################################
################################################################################

## 1.1 and 1.2 [Gender(A3) and Level of financial satisfaction (J1) were imported
## from the dataset and was proceed to further calculation

## Creating variable gender based on A3
dataframe <- dataframe %>%
  mutate(gender = case_when(A3 == 1 ~ "Male",
                            A3 == 2 ~ "Female"))


table(dataframe$gender)

## Creating variable levelofsatisfaction based on J1
dataframe <- dataframe %>%
  mutate(levelofsatisfaction = case_when(J1 == 1 ~ 1,
                                         J1 == 2 ~ 2,
                                         J1 == 3 ~ 3,
                                         J1 == 4 ~ 4,
                                         J1 == 5 ~ 5,
                                         J1 == 6 ~ 6,
                                         J1 == 7 ~ 7,
                                         J1 == 8 ~ 8,
                                         J1 == 9 ~ 9,
                                         J1 == 10 ~ 10,
                                         J1 == 98 ~ 11,
                                         J1 == 99 ~ 12),
         levelofsatisfaction = factor (levelofsatisfaction,
                                       levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                       labels = c("Extremely dissatisfied",
                                                  "Very dissatisfied",
                                                  "Dissatisfied",
                                                  "Somewhat dissatisfied",
                                                  "Neutral",
                                                  "Somewhat satisfied",
                                                  "Satisfied",
                                                  "Very satisfied", 
                                                  "Absolutely satisfied",
                                                  "Extremely satisfied", 
                                                  "Don't know",
                                                  "Prefer not to say"),
                                       ordered = TRUE))

## Tabulating the dataset 
table(dataframe$levelofsatisfaction)

#### 1.3
## Creating cleaned data for further analysis
data_clean1<-select(.data=dataframe,agegroup, interest_earnings_correct, inflation_loses_correct, stockrisk_correct, fin_literacy, NFCSID, gender, levelofsatisfaction )

## Exporting cleaned data to new csv file
write.csv(data_clean1,"Homework3/data_clean1.csv", row.names = FALSE)

###############################################################################