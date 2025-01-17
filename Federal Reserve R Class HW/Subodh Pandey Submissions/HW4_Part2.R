
## 2.1 A new script for this analysis was created

## 2.2 Cleaned csv file was imported

datafinal1<-read_csv("Homework3/data_clean1.csv")

## 2.3
## Summary statistics for combined literacy score

Summary_stat_ungroup <- datafinal1 %>%
  summarize(
    n = n(),
    mean = mean(fin_literacy, na.rm=TRUE),
    median = median(fin_literacy, na.rm=TRUE),
    min = min(fin_literacy, na.rm=TRUE),
    max = max(fin_literacy, na.rm=TRUE),
    sd = sd(fin_literacy, na.rm=TRUE),
    per25= quantile(fin_literacy,p=0.25, na.rm=TRUE),
    per75 = quantile(fin_literacy,p=0.75, na.rm=TRUE),
  )

View(Summary_stat_ungroup)

################################################################################

## 2.4
## Summary statistics for combined literacy score grouped by gender ######

Summary_stat_gender <- datafinal1 %>%
  group_by(gender) %>%
  summarize(
    count= n(),
    mean = mean(fin_literacy, na.rm=TRUE),
    median = median(fin_literacy, na.rm=TRUE),
    min = min(fin_literacy, na.rm=TRUE),
    max = max(fin_literacy, na.rm=TRUE),
    sd = sd(fin_literacy, na.rm=TRUE),
    per25 = quantile(fin_literacy,p=0.25, na.rm=TRUE),
    per75 = quantile(fin_literacy,p=0.75, na.rm=TRUE),
  )

View(Summary_stat_gender)

#############################################################

## 2.5
## Summary statistics for combined literacy score by level of financial satisf-
## action ###

Summary_satisfaction <- datafinal1 %>%
  group_by(levelofsatisfaction) %>%
  summarize(
    count = n(),
    mean = mean(fin_literacy, na.rm=TRUE),
    median = median(fin_literacy, na.rm=TRUE),
    min = min(fin_literacy, na.rm=TRUE),
    max = max(fin_literacy, na.rm=TRUE),
    sd = sd(fin_literacy, na.rm=TRUE),
    per25 = quantile(fin_literacy,p=0.25, na.rm=TRUE),
    per75 = quantile(fin_literacy,p=0.75, na.rm=TRUE),
  )

View(Summary_satisfaction)


################################################################################
## 2.6
## Average financial literacy score grouped by both gender and level of financi-
## al satisfaction
Summary_both <- datafinal1 %>%
  group_by(gender, levelofsatisfaction) %>%
  summarize(
    mean = mean(fin_literacy, na.rm=TRUE)
  )

View(Summary_both)


## 2.7 

################ Pivot wider ###################################################

Summarize_wider<-datafinal1 %>%
  group_by(gender, levelofsatisfaction) %>%
  summarize(
    mean = mean(fin_literacy, na.rm=TRUE)
  ) %>%
  pivot_wider(names_from = gender,
              values_from = mean,
              values_fill = 0)

View(Summarize_wider)
################################################################################
