ibrary(rvest)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(tidyverse)
library(IPEDS)
####BUFFALO####
# Base URL of the ESPN page
base_url <- "https://www.espn.com/college-football/team/schedule/_/id/2084/season/"

# Initialize an empty data frame to store the combined data
combined_schedule_df_Buffalo <- data.frame()

# Loop through the years 2005 to 2023
for (year in 2005:2023) {
  # Construct the full URL for the given year
  url <- paste0(base_url, year)
  
  # Read the HTML content from the URL
  webpage <- read_html(url)
  
  # Extract the schedule table
  schedule_table_Buffalo <- webpage %>%
    html_nodes("table") %>%
    .[1] %>% 
    html_table(fill = TRUE)
  
  # Convert the extracted table to a data frame
  schedule_df_Buffalo <- as.data.frame(schedule_table_Buffalo)
  
  # Add a column for the year
  schedule_df_Buffalo$Year <- year
  
  # Append the data frame to the combined data frame
  combined_schedule_df_Buffalo <- bind_rows(combined_schedule_df_Buffalo, schedule_df_Buffalo)
}


# Filter out rows that do not start with 'vs' or '@' in the 'X2' column
cleaned_schedule_df_BUFFALO <- combined_schedule_df_Buffalo %>%
  filter(str_starts(X2, "vs") | str_starts(X2, "@"))

# Remove numbers from strings in the 'X2' column
cleaned_schedule_df_BUFFALO$X2 <- str_replace_all(cleaned_schedule_df_BUFFALO$X2, "\\d+", "")

# Create a new column 'Opponent' by removing special characters and 'vs' and '@' from 'X2'
cleaned_schedule_df_BUFFALO$Opponent <- cleaned_schedule_df_BUFFALO$X2 %>%
  str_replace_all("[^a-zA-Z ]", "") %>%
  str_replace_all("^(vs|@)\\s*", "") %>% 
trimws()

# Create a 'Home_Dummy' column: 1 if 'X2' starts with 'vs', 0 if it starts with '@'
cleaned_schedule_df_BUFFALO$Away_Dummy<- ifelse(str_starts(cleaned_schedule_df_BUFFALO$X2, "vs"), 0, 1)

# Create a 'Win_Loss_Dummy' column: 1 if 'X3' starts with 'W', 0 if it starts with 'L'
cleaned_schedule_df_BUFFALO$Win_Loss_Dummy <- ifelse(str_starts(cleaned_schedule_df_BUFFALO$X3, "W"), 1, 0)




# Print the cleaned data frame with IPEDS IDs
cleaned_schedule_df_BUFFALO<-cleaned_schedule_df_BUFFALO %>% 
  filter(str_starts(X3, "W") | str_starts(X3, "L")) %>% 
select(-c(X4:X8))

cleaned_schedule_df_BUFFALO$TEAM<-"BUFFALO"



####Rice University####





base_url <- "https://www.espn.com/college-football/team/schedule/_/id/242/season/"

# Initialize an empty data frame to store the combined data
combined_schedule_df_Rice <- data.frame()

# Loop through the years 2005 to 2023
for (year in 2005:2023) {
  # Construct the full URL for the given year
  url <- paste0(base_url, year)
  
  # Read the HTML content from the URL
  webpage <- read_html(url)
  
  # Extract the schedule table
  schedule_table_Rice <- webpage %>%
    html_nodes("table") %>%
    .[1] %>% 
    html_table(fill = TRUE)
  
  # Convert the extracted table to a data frame
  schedule_df_Rice <- as.data.frame(schedule_table_Rice)
  
  # Add a column for the year
  schedule_df_Rice$Year <- year
  
  # Append the data frame to the combined data frame
  combined_schedule_df_Rice <- bind_rows(combined_schedule_df_Rice, schedule_df_Rice)
}


# Filter out rows that do not start with 'vs' or '@' in the 'X2' column
cleaned_schedule_df_Rice <- combined_schedule_df_Rice %>%
  filter(str_starts(X2, "vs") | str_starts(X2, "@"))

# Remove numbers from strings in the 'X2' column
cleaned_schedule_df_Rice$X2 <- str_replace_all(cleaned_schedule_df_Rice$X2, "\\d+", "")

# Create a new column 'Opponent' by removing special characters and 'vs' and '@' from 'X2'
cleaned_schedule_df_Rice$Opponent <- cleaned_schedule_df_Rice$X2 %>%
  str_replace_all("[^a-zA-Z ]", "") %>%
  str_replace_all("^(vs|@)\\s*", "") %>% 
  trimws()

# Create a 'Home_Dummy' column: 1 if 'X2' starts with 'vs', 0 if it starts with '@'
cleaned_schedule_df_Rice$Away_Dummy<- ifelse(str_starts(cleaned_schedule_df_Rice$X2, "vs"), 0, 1)

# Create a 'Win_Loss_Dummy' column: 1 if 'X3' starts with 'W', 0 if it starts with 'L'
cleaned_schedule_df_Rice$Win_Loss_Dummy <- ifelse(str_starts(cleaned_schedule_df_Rice$X3, "W"), 1, 0)




# Print the cleaned data frame with IPEDS IDs
cleaned_schedule_df_Rice<-cleaned_schedule_df_Rice %>% 
  filter(str_starts(X3, "W") | str_starts(X3, "L")) %>% 
  select(-c(X4:X8))

cleaned_schedule_df_Rice$TEAM<-"Rice"


#### USC####

base_url <- "https://www.espn.com/college-football/team/schedule/_/id/30/season/"

# Initialize an empty data frame to store the combined data
combined_schedule_df_USC <- data.frame()

# Loop through the years 2005 to 2023
for (year in 2005:2023) {
  # Construct the full URL for the given year
  url <- paste0(base_url, year)
  
  # Read the HTML content from the URL
  webpage <- read_html(url)
  
  # Extract the schedule table
  schedule_table_USC <- webpage %>%
    html_nodes("table") %>%
    .[1] %>% 
    html_table(fill = TRUE)
  
  # Convert the extracted table to a data frame
  schedule_df_USC <- as.data.frame(schedule_table_USC)
  
  # Add a column for the year
  schedule_df_USC$Year <- year
  
  # Append the data frame to the combined data frame
  combined_schedule_df_USC <- bind_rows(combined_schedule_df_USC, schedule_df_USC)
}


# Filter out rows that do not start with 'vs' or '@' in the 'X2' column
cleaned_schedule_df_USC <- combined_schedule_df_USC %>%
  filter(str_starts(X2, "vs") | str_starts(X2, "@"))

# Remove numbers from strings in the 'X2' column
cleaned_schedule_df_USC$X2 <- str_replace_all(cleaned_schedule_df_USC$X2, "\\d+", "")

# Create a new column 'Opponent' by removing special characters and 'vs' and '@' from 'X2'
cleaned_schedule_df_USC$Opponent <- cleaned_schedule_df_USC$X2 %>%
  str_replace_all("[^a-zA-Z ]", "") %>%
  str_replace_all("^(vs|@)\\s*", "") %>% 
  trimws()

# Create a 'Home_Dummy' column: 1 if 'X2' starts with 'vs', 0 if it starts with '@'
cleaned_schedule_df_USC$Away_Dummy<- ifelse(str_starts(cleaned_schedule_df_USC$X2, "vs"), 0, 1)

# Create a 'Win_Loss_Dummy' column: 1 if 'X3' starts with 'W', 0 if it starts with 'L'
cleaned_schedule_df_USC$Win_Loss_Dummy <- ifelse(str_starts(cleaned_schedule_df_USC$X3, "W"), 1, 0)




# Print the cleaned data frame with IPEDS IDs
cleaned_schedule_df_USC<-cleaned_schedule_df_USC %>% 
  filter(str_starts(X3, "W") | str_starts(X3, "L")) %>% 
  select(-c(X4:X8))

cleaned_schedule_df_USC$TEAM<-"USC"




####
#####





Winnings_Master_Sheet <- read_excel("Your")
test<-left_join(Winnings_Master_Sheet, cleaned_schedule_df_BUFFALO, by = c("YEAR" = "Year", "TEAM"))
test<-left_join(test, cleaned_schedule_df_Rice, by = c("YEAR" = "Year", "TEAM"))
test<-left_join(test, cleaned_schedule_df_USC, by = c("YEAR" = "Year", "TEAM"))

Winnings_Master_Sheet_06282024 <- read_excel("Your-path-here") %>% 
group_by(Opponent.x) %>%
  fill(`IPEDS OPPONENT`, .direction = "downup") %>%
  ungroup()


Matrix_and_cleaningkoch <- read_excel("Your-path-here") %>% 
  select(c(school,))

left_join(Winnings_Master_Sheet)

writexl::write_xlsx(Winnings_Master_Sheet_06282024, "Winnings_Master_Sheet_07012024.xlsx")



IPEDS<-school_preferences()
