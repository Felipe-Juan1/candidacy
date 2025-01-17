# Description ----


# 1. I logged in the Posit cloud and selected ...., then created the
#     new R studio project called hw2

# 2. Directory HW2 was created(cloud/project/HW2) and then data 
#     file was uploaded.

# 3. install.packages("tidyverse")

# 4. Template was downloaded and uploaded to HW2 directory  and renamed to
#   ....

# 5.
# Assignment: Homework 2
# Student Name: 
# Date: 


# 6. Load packages ----

library(tidyverse)



# 7. Import ----

starwars_raw <- read_csv(file="HW2/starwars_raw.csv")



# 8. View Data & Documentation ----

glimpse(starwars_raw)
glimpse(starwars)

?starwars

head(starwars_raw)  #For first few rows of starwars_raw data
head(starwars)      #For first few rows of starwars data

##The number of rows and columns were determined using glimpse command
## Number of rows: 87  Number of columns: 8     (In starwars_raw data)
## Number of rows: 87  Number of columns: 14    (In starwars data)  
## Birth year: Year born (BBY = Before Battle of Yavin)

# 9. Transform ----

### Subsetting the data by certain columns without storing the output
starwars_raw |>
  select(name, height, mass, homeworld, species)
 
### Displaying where mass or height are unknown without storing the output
starwars_raw |>
  filter(is.na(mass)|is.na(height))

### Displaying where mass and height are known
starwars_raw |>
  select(name, height, mass, homeworld, species) |>
  filter(!is.na(mass) & !is.na(height))

# 10. Functions ----

## Creating a function (to_pounds) to convert kg to pounds
## However in general (kg=0.453592*pounds),
## I continue to follow the HW guidelines (pounds=0.453592*kg)

to_pounds <- function(kg) {
  pounds = 0.453592*kg
  return(pounds)
}

## Creating a function (to_inches) to convert centimeters to inches

to_inches <- function(cm) {
  inches=(cm/2.54)
  return(inches)
}
  
## Creating the function to_feet to convert inches to feet

to_feet <- function(inches) {
  feet=(inches/12)
  return(feet)
}

## Creating the function compute_bmi to compute 
## body mass index (mass in kg & height in cm)

compute_bmi <- function(kg,cm) {
  bmi=(kg/(cm^2))
  return(bmi)
}

# 11. Transform raw data & Create new data sset ----

## Output will be different if we use starwars_raw instead of starwars data. To 
## become consistent with the output in HW, I choose to use starwars data

## Output shown in the HW1 question consists of 11 columns however we are only
## selecting 5 columns and computed the 3 other columns which adds to 8

###### Without using pipe operators
starwars_clean <- select (starwars, name, height, mass, homeworld, species) #1
starwars_clean <- filter(starwars_clean, !is.na(height) & !is.na(mass))   #2
starwars_clean <- rename(starwars_clean, height_cm = height, mass_kg = mass) #3 & 4
starwars_clean$bmi <- compute_bmi(starwars_clean$mass_kg,starwars_clean$height_cm) #5
starwars_clean$height_ft <- to_feet(to_inches(starwars_clean$height_cm))  #6
starwars_clean$mass_lbs <- to_pounds(starwars_clean$mass_kg)  #7

print(starwars_clean)

### OR
###### Using pipe operators

starwars_clean <- starwars |>
  select(name, height, mass, homeworld, species) |>       # 11.1
  filter (!is.na(mass) & !is.na(height)) |>               # 11.2
  rename (height_cm=height) |>                            # 11.3
  rename (mass_kg=mass) |>                                # 11.4
  mutate(bmi=compute_bmi(mass_kg, height_cm)) |>          # 11.5
  mutate(height_ft=to_feet(to_inches(height_cm))) |>      # 11.6
  mutate(mass_lbs=to_pounds(mass_kg))                     # 11.7

print(starwars_clean)  

### Bonus Question Answer

#   Without using any dplyr verbs twice. dplyr function used are select, filter,
#   rename and transmute. None of them are repeated. I used the starwars_clean1 
#   for bonus question to compare and match the result calculated above.

### OR
starwars_clean1 <- starwars_raw |>
  select(name, height, mass, homeworld, species) |>
  filter (!is.na(mass) & !is.na(height)) |>           
  rename (height_cm=height, mass_kg=mass) |>          
  transmute(name,
            height_cm,
            mass_kg,
            homeworld,
            species,
            bmi = compute_bmi(mass_kg, height_cm),
            height_ft = to_feet(to_inches(height_cm)),
            mass_lbs = to_pounds(mass_kg)
  )

print (starwars_clean1)

# Analyze ----

# 12. Compute Summary Statistics ----

starwars_clean |>
    summarize(
      n=n(),
      mean=mean(bmi),
      median=median(bmi),
      min=min(bmi),
      max=max(bmi),
      sd=sd(bmi)
    )


# 13. Compute Grouped Summary Statistics ----

starwars_clean |>
  filter(homeworld %in% c("Naboo","Tatooine")) |>
  group_by(homeworld) |>
  summarize(
    n=n(),
    mean=mean(bmi),
    median=median(bmi),
    min=min(bmi),
    max=max(bmi),
    sd=sd(bmi)
  )

# The output for bmi of Naboo in QN 13 exactly matches if we construct new data 
# set (starwars_clean) from starwars_raw but it will be slightly different if we
# use dataset from starwars. Other outputs are indifferent.



# 14. Interpret & Communicate ----


# 1. Which home world has a lower mean for the analysis variable, Naboo or Tatooine?
# Ans:  Naboo has a lower body mass index (analysis variable) compared with 
#       Tatooine measured in kilogram per squared centimeter.

# 2. Does this seem like a meaningful difference or not, and why?
#Ans:   They have the difference of (0.00067) which seems insignificant to me.
#       Further analysis can be done (regression or t-test) can be done to verify
#       it.

# 3. What might explain the difference or similarity?
#Ans:   We have other variables like birth_year, sex and gender which may affects 
#       the bmi. BMI increases as age increase. Also, male and masculine have more
#       bmi compared to female and feminine.

# 4. What other comparisons might help learn about how the characters from these two planets compare?
# Ans:  Difference in space composition and the environmental conditions may help
#       to compare the characteristics of this planet.
