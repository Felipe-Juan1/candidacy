
install.packages("ggmap")

install.packages("remotes")
remotes::install_github("chris-prener/censusxy")
install.packages("sf")
# for increased speed processing
install.packages("furrr")
install.packages("future")

library(censusxy)

library(ggplot2)
library(dplyr)
library(readxl)
library(purrr)
library(ggmap)

library(future)
library(furrr)
library(dplyr)
library(sf)
library(writexl)


# get your own api on google using https://console.cloud.google.com/google/maps-apis/api-list?project=focus-beacon-441714-t4

register_google(key = "YOUR-API-KEY")  # Replace with your actual API key

# Group 1
# Geocode each address to obtain latitude and longitude

paddresses <- read_excel("R work/urban3_c_split.xlsx", sheet = "Group1")
View(paddresses)

paddresses <- paddresses %>%
  mutate(
    geocode_result = map(PROPERTY_ADDRESS, ~ geocode(.x, output = "latlona")),
    latitude = map_dbl(geocode_result, ~ .x$lat),
    longitude = map_dbl(geocode_result, ~ .x$lon)
  )

# Export paddresses to an Excel file
write_xlsx(paddresses, "paddresses.xlsx")

# Display the updated dataframe with latitude and longitude
print(paddresses)
View(paddresses)

# Count the number of missing values in each column
missing_counts <- colSums(is.na(paddresses))

# Display the result
print(missing_counts)


# Group 2

paddresses2 <- read_excel("R work/urban3_c_split.xlsx", sheet = "Group2")
View(paddresses2)

# Geocode each address to obtain latitude and longitude
paddresses2 <- paddresses2 %>%
  mutate(
    geocode_result = map(PROPERTY_ADDRESS, ~ geocode(.x, output = "latlona")),
    latitude = map_dbl(geocode_result, ~ .x$lat),
    longitude = map_dbl(geocode_result, ~ .x$lon)
  )

# Display the updated dataframe with latitude and longitude
print(paddresses2)
View(paddresses2)

# Count the number of missing values in each column
missing_counts2 <- colSums(is.na(paddresses2))
print(missing_counts2)

write_xlsx(paddresses2, "paddresses2.xlsx")


# Group 3
paddresses3 <- read_excel("R work/urban3_c_split.xlsx", sheet = "Group3")
View(paddresses3)

# Geocode each address to obtain latitude and longitude
paddresses3 <- paddresses3 %>%
  mutate(
    geocode_result = map(PROPERTY_ADDRESS, ~ geocode(.x, output = "latlona")),
    latitude = map_dbl(geocode_result, ~ .x$lat),
    longitude = map_dbl(geocode_result, ~ .x$lon)
  )

# Display the updated dataframe with latitude and longitude
print(paddresses3)
View(paddresses3)

# Count the number of missing values in each column
missing_counts3 <- colSums(is.na(paddresses3))

# Display the result
print(missing_counts3)

write_xlsx(paddresses3, "paddresses3.xlsx")


##### Combine all the dataframes
# Combine the three dataframes into one
paddresses4 <- rbind(paddresses, paddresses2, paddresses3)

print(paddresses4)

# Display the combined dataframe
View(paddresses4)

write_xlsx(paddresses4, "paddresses4.xlsx")


# Create the geometry column using map2
paddresses4 <- paddresses4 %>%
  mutate(geometry = map2(longitude, latitude, ~ st_point(c(.x, .y))))

# Convert the list-column 'geometry' into an sfc (simple feature column)
paddresses4$geometry <- st_sfc(paddresses4$geometry, crs = 4326)

# Convert the dataframe into an sf object
paddresses4 <- st_as_sf(paddresses4, sf_column_name = "geometry", crs = 4326)

# Display the updated dataframe
print(paddresses4)

# Export the final
write_xlsx(paddresses4, "paddresses4.xlsx")

# Count the number of missing values in each column
missing_counts4 <- colSums(is.na(paddresses4))

# Display the result
print(missing_counts4)


## Break ########
## Break ########
## Break ########


## getting pop black, hispanics and median income data

## link to register and get API https://api.census.gov/data/key_signup.html

install.packages("tidycensus")

library(tidycensus)

census_api_key("YOUR-API-KEY", install = TRUE)

readRenviron("~/.Renviron")

Sys.getenv("YOUR-API-KEY")


# Define the variables for African American and Hispanic populations and total population
variables <- c(
  total_pop = "B03002_001",          # Total population
  african_american = "B03002_004",    # African American population
  hispanic = "B03002_012",             # Hispanic population
  medIncome = "B19013_001"            # Median income
)

# Get data for Maryland (or change to your state of interest)
dc_data <- get_acs(
  geography = "tract",
  variables = variables,
  state = "DC",
  year = 2017,
  geometry = TRUE
)

View(dc_data)

# Export the final as shapefile
st_write(dc_data, "dc_data.shp", delete_layer = TRUE)

