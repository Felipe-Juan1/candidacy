library(hoopR)
library(dplyr)

#creating list for data
standings_list <- list()

# Loop through each year from 2004 to 2024
for (year in 2004:2024) {
  # Fetch the data for each year
  mbb_data <- hoopR::espn_mbb_standings(year = year)
  
  # Add a 'year' column to the data frame
  mbb_data <- mbb_data %>% mutate(year = year)
  
  #Creating each list
  standings_list[[as.character(year)]] <- mbb_data
}

# Combine all the data frames in the list into one data frame
combined_mbb_standings <- bind_rows(standings_list)


#write data into excel
writexl::write_xlsx(combined_mbb_standings, "combined_mbb_standings.xlsx")
