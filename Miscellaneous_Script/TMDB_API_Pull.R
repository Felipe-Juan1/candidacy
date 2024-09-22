#Potato we love movies, I love data, this project is perfect

numCores <- parallel::detectCores()#Spliting workload across multiple cores this line is for Mac might be different for linux or Windows
results <- parallel::mclapply(1:100, function(x) {
  



  library(httr)
  library(jsonlite)
  library(dplyr)
  
  # Set up your API key @ https://developer.themoviedb.org/docs/getting-started
  api_key <- #"your-API-key"
  base_url <- "https://api.themoviedb.org/3/movie/"
  
  # Create an empty data frame to store movie details
  movies_data <- data.frame()
  
  # Loop through movie IDs...Loops the death of R 
  for (movie_id in 1:500) { #there are at least 2,140,000 at last check so you can change it from 500 to 2,140,000 but beware it'll take hours without a powerful computer
    
    # Create the URL for each movie
    url <- paste0(base_url, movie_id, "&append_to_response=credits")
    
    # Make the GET request
    response <- VERB("GET", url, query = list(language = "en-US", append_to_response = "credits"),
                     add_headers('Authorization' = paste('Bearer', api_key)),
                     content_type("application/octet-stream"),
                     accept("application/json"))
    
    # Parsing data
    movie_details <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Check if the movie exists (API returns a status code 34 if the movie ID does not exist)
    if (!is.null(movie_details$status_code) && movie_details$status_code == 34) {
      next  # Skip this iteration if the movie does not exist
    }
    director_name <- NA_character_
    director_popularity <- NA_real_
    
    # Check if crew is available and contains the necessary data
    if (!is.null(movie_details$credits$crew) && is.list(movie_details$credits$crew)) {
      
      # Convert crew to a tibble (if it's not already) and check if the tibble contains the job and popularity columns
      crew_df <- as_tibble(movie_details$credits$crew)
      
      if ("job" %in% colnames(crew_df) && "popularity" %in% colnames(crew_df)) {
        # Filter for the director's name and popularity
        director <- crew_df %>% 
          filter(job == "Director") %>%
          select(name, popularity)
        
        if (nrow(director) > 0) {
          director_name <- director$name[1]  # Assuming there's only one director
          director_popularity <- director$popularity[1]
        }
      }
    }
    genre1 <- ifelse(length(movie_details$genres) >= 1, movie_details$genres$name[1], NA_character_)
    genre2 <- ifelse(length(movie_details$genres) >= 2, movie_details$genres$name[2], NA_character_)
    genre3 <- ifelse(length(movie_details$genres) >= 3, movie_details$genres$name[3], NA_character_)
    prod_co_id1 <- ifelse(length(movie_details$production_companies) >= 1, movie_details$production_companies$id[1], NA)
    prod_co_id2 <- ifelse(length(movie_details$production_companies) >= 2, movie_details$production_companies$id[2], NA)
    prod_co_id3 <- ifelse(length(movie_details$production_companies) >= 3, movie_details$production_companies$id[3], NA)
    prod_co_name1 <- ifelse(length(movie_details$production_companies) >= 1, movie_details$production_companies$name[1], NA_character_)
    prod_co_name2 <- ifelse(length(movie_details$production_companies) >= 2, movie_details$production_companies$name[2], NA_character_)
    prod_co_name3 <- ifelse(length(movie_details$production_companies) >= 3, movie_details$production_companies$name[3], NA_character_)
    #####Actors#####
    Actor1 <- ifelse(length(movie_details$credits$cast) >= 1, movie_details$credits$cast$name[1], NA_character_)
    Actor2 <- ifelse(length(movie_details$credits$cast) >= 2, movie_details$credits$cast$name[2], NA_character_)
    Actor3 <- ifelse(length(movie_details$credits$cast) >= 3, movie_details$credits$cast$name[3], NA_character_)
    Actor4 <- ifelse(length(movie_details$credits$cast) >= 4, movie_details$credits$cast$name[4], NA_character_)
    Actor5 <- ifelse(length(movie_details$credits$cast) >= 5, movie_details$credits$cast$name[5], NA_character_)
    Actor6 <- ifelse(length(movie_details$credits$cast) >= 6, movie_details$credits$cast$name[6], NA_character_)
    Actor7 <- ifelse(length(movie_details$credits$cast) >= 7, movie_details$credits$cast$name[7], NA_character_)
    Actor8 <- ifelse(length(movie_details$credits$cast) >= 8, movie_details$credits$cast$name[8], NA_character_)
    Actor9 <- ifelse(length(movie_details$credits$cast) >= 9, movie_details$credits$cast$name[9], NA_character_)
    Actor10 <- ifelse(length(movie_details$credits$cast) >= 10, movie_details$credits$cast$name[10], NA_character_)
    Actor11 <- ifelse(length(movie_details$credits$cast) >= 11, movie_details$credits$cast$name[11], NA_character_)
    Actor12 <- ifelse(length(movie_details$credits$cast) >= 12, movie_details$credits$cast$name[12], NA_character_)
    Actor13 <- ifelse(length(movie_details$credits$cast) >= 13, movie_details$credits$cast$name[13], NA_character_)
    Actor14 <- ifelse(length(movie_details$credits$cast) >= 14, movie_details$credits$cast$name[14], NA_character_)
    Actor15 <- ifelse(length(movie_details$credits$cast) >= 15, movie_details$credits$cast$name[15], NA_character_)
    Actor16 <- ifelse(length(movie_details$credits$cast) >= 16, movie_details$credits$cast$name[16], NA_character_)
    Actor17 <- ifelse(length(movie_details$credits$cast) >= 17, movie_details$credits$cast$name[17], NA_character_)
    Actor18 <- ifelse(length(movie_details$credits$cast) >= 18, movie_details$credits$cast$name[18], NA_character_)
    Actor19 <- ifelse(length(movie_details$credits$cast) >= 19, movie_details$credits$cast$name[19], NA_character_)
    Actor20 <- ifelse(length(movie_details$credits$cast) >= 20, movie_details$credits$cast$name[20], NA_character_)
    
    
    
    Actorid1 <- ifelse(length(movie_details$credits$cast) >= 1, movie_details$credits$cast$id[1], NA_integer_)
    Actorid2 <- ifelse(length(movie_details$credits$cast) >= 2, movie_details$credits$cast$id[2], NA_integer_)
    Actorid3 <- ifelse(length(movie_details$credits$cast) >= 3, movie_details$credits$cast$id[3], NA_integer_)
    Actorid4 <- ifelse(length(movie_details$credits$cast) >= 4, movie_details$credits$cast$id[4], NA_integer_)
    Actorid5 <- ifelse(length(movie_details$credits$cast) >= 5, movie_details$credits$cast$id[5], NA_integer_)
    Actorid6 <- ifelse(length(movie_details$credits$cast) >= 6, movie_details$credits$cast$id[6], NA_integer_)
    Actorid7 <- ifelse(length(movie_details$credits$cast) >= 7, movie_details$credits$cast$id[7], NA_integer_)
    Actorid8 <- ifelse(length(movie_details$credits$cast) >= 8, movie_details$credits$cast$id[8], NA_integer_)
    Actorid9 <- ifelse(length(movie_details$credits$cast) >= 9, movie_details$credits$cast$id[9], NA_integer_)
    Actorid10 <- ifelse(length(movie_details$credits$cast) >= 10, movie_details$credits$cast$id[10], NA_integer_)
    Actorid11 <- ifelse(length(movie_details$credits$cast) >= 11, movie_details$credits$cast$id[11], NA_integer_)
    Actorid12 <- ifelse(length(movie_details$credits$cast) >= 12, movie_details$credits$cast$id[12], NA_integer_)
    Actorid13 <- ifelse(length(movie_details$credits$cast) >= 13, movie_details$credits$cast$id[13], NA_integer_)
    Actorid14 <- ifelse(length(movie_details$credits$cast) >= 14, movie_details$credits$cast$id[14], NA_integer_)
    Actorid15 <- ifelse(length(movie_details$credits$cast) >= 15, movie_details$credits$cast$id[15], NA_integer_)
    Actorid16 <- ifelse(length(movie_details$credits$cast) >= 16, movie_details$credits$cast$id[16], NA_integer_)
    Actorid17 <- ifelse(length(movie_details$credits$cast) >= 17, movie_details$credits$cast$id[17], NA_integer_)
    Actorid18 <- ifelse(length(movie_details$credits$cast) >= 18, movie_details$credits$cast$id[18], NA_integer_)
    Actorid19 <- ifelse(length(movie_details$credits$cast) >= 19, movie_details$credits$cast$id[19], NA_integer_)
    Actorid20 <- ifelse(length(movie_details$credits$cast) >= 20, movie_details$credits$cast$id[20], NA_integer_)
    
    
    Actorpop1 <- ifelse(length(movie_details$credits$cast) >= 1, movie_details$credits$cast$popularity[1], NA_integer_)
    Actorpop2 <- ifelse(length(movie_details$credits$cast) >= 2, movie_details$credits$cast$popularity[2], NA_integer_)
    Actorpop3 <- ifelse(length(movie_details$credits$cast) >= 3, movie_details$credits$cast$popularity[3], NA_integer_)
    Actorpop4 <- ifelse(length(movie_details$credits$cast) >= 4, movie_details$credits$cast$popularity[4], NA_integer_)
    Actorpop5 <- ifelse(length(movie_details$credits$cast) >= 5, movie_details$credits$cast$popularity[5], NA_integer_)
    Actorpop6 <- ifelse(length(movie_details$credits$cast) >= 6, movie_details$credits$cast$popularity[6], NA_integer_)
    Actorpop7 <- ifelse(length(movie_details$credits$cast) >= 7, movie_details$credits$cast$popularity[7], NA_integer_)
    Actorpop8 <- ifelse(length(movie_details$credits$cast) >= 8, movie_details$credits$cast$popularity[8], NA_integer_)
    Actorpop9 <- ifelse(length(movie_details$credits$cast) >= 9, movie_details$credits$cast$popularity[9], NA_integer_)
    Actorpop10 <- ifelse(length(movie_details$credits$cast) >= 10, movie_details$credits$cast$popularity[10], NA_integer_)
    Actorpop11 <- ifelse(length(movie_details$credits$cast) >= 11, movie_details$credits$cast$popularity[11], NA_integer_)
    Actorpop12 <- ifelse(length(movie_details$credits$cast) >= 12, movie_details$credits$cast$popularity[12], NA_integer_)
    Actorpop13 <- ifelse(length(movie_details$credits$cast) >= 13, movie_details$credits$cast$popularity[13], NA_integer_)
    Actorpop14 <- ifelse(length(movie_details$credits$cast) >= 14, movie_details$credits$cast$popularity[14], NA_integer_)
    Actorpop15 <- ifelse(length(movie_details$credits$cast) >= 15, movie_details$credits$cast$popularity[15], NA_integer_)
    Actorpop16 <- ifelse(length(movie_details$credits$cast) >= 16, movie_details$credits$cast$popularity[16], NA_integer_)
    Actorpop17 <- ifelse(length(movie_details$credits$cast) >= 17, movie_details$credits$cast$popularity[17], NA_integer_)
    Actorpop18 <- ifelse(length(movie_details$credits$cast) >= 18, movie_details$credits$cast$popularity[18], NA_integer_)
    Actorpop19 <- ifelse(length(movie_details$credits$cast) >= 19, movie_details$credits$cast$popularity[19], NA_integer_)
    Actorpop20 <- ifelse(length(movie_details$credits$cast) >= 20, movie_details$credits$cast$popularity[20], NA_integer_)
    
    
    ####Pull#####
    
    movie_row <- data.frame(
      movie_id = movie_details$id,
      title = movie_details$title,
      release_date = movie_details$release_date,
      budget = movie_details$budget,
      revenue = movie_details$revenue,
      vote_count = movie_details$vote_count,
      imdb_id = movie_details$imdb_id,
      rating = movie_details$vote_average,
      director_name = director_name,
      director_popularity = director_popularity,
      genre1 = genre1,
      genre2 = genre2,
      prod_co_id1 = prod_co_id1,
      prod_co_id2 = prod_co_id2,
      prod_co_id3 = prod_co_id3,
      prod_co_name1 = prod_co_name1,
      prod_co_name2 = prod_co_name2,
      prod_co_name3 = prod_co_name3,
      #####Actor Pull####
      Actor1 = Actor1,
      Actor2 = Actor2,
      Actor3 = Actor3,
      Actor4 = Actor4,
      Actor5 = Actor5,
      Actor6 = Actor6,
      Actor7 = Actor7,
      Actor8 = Actor8,
      Actor9 = Actor9,
      Actor10 = Actor10,
      Actor11 = Actor11,
      Actor12 = Actor12,
      Actor13 = Actor13,
      Actor14 = Actor14,
      Actor15 = Actor15,
      Actor16 = Actor16,
      Actor17 = Actor17,
      Actor18 = Actor18,
      Actor19 = Actor19,
      Actor20 = Actor20,
      
      Actorid1 = Actorid1,
      Actorid2 = Actorid2,
      Actorid3 = Actorid3,
      Actorid4 = Actorid4,
      Actorid5 = Actorid5,
      Actorid6 = Actorid6,
      Actorid7 = Actorid7,
      Actorid8 = Actorid8,
      Actorid9 = Actorid9,
      Actorid10 = Actorid10,
      Actorid11 = Actorid11,
      Actorid12 = Actorid12,
      Actorid13 = Actorid13,
      Actorid14 = Actorid14,
      Actorid15 = Actorid15,
      Actorid16 = Actorid16,
      Actorid17 = Actorid17,
      Actorid18 = Actorid18,
      Actorid19 = Actorid19,
      Actorid20 = Actorid20,
      
      Actorpop1 = Actorpop1,
      Actorpop2 = Actorpop2,
      Actorpop3 = Actorpop3,
      Actorpop4 = Actorpop4,
      Actorpop5 = Actorpop5,
      Actorpop6 = Actorpop6,
      Actorpop7 = Actorpop7,
      Actorpop8 = Actorpop8,
      Actorpop9 = Actorpop9,
      Actorpop10 = Actorpop10,
      Actorpop11 = Actorpop11,
      Actorpop12 = Actorpop12,
      Actorpop13 = Actorpop13,
      Actorpop14 = Actorpop14,
      Actorpop15 = Actorpop15,
      Actorpop16 = Actorpop16,
      Actorpop17 = Actorpop17,
      Actorpop18 = Actorpop18,
      Actorpop19 = Actorpop19,
      Actorpop20 = Actorpop20,
      
      
      
      
      stringsAsFactors = FALSE
    ) %>%filter(release_date>"1990-01-01" & vote_count>5500 )
    
    
    # Append to the movies_data dataframe
    movies_data <- bind_rows(movies_data, movie_row)
    
    # Print progress every 1000 movies
    if (movie_id %% 1000 == 0) {
      print(paste("Processed movie ID:", movie_id))
    }
  }
  
  
  # View the final data
  print(movies_data)
  
}, mc.cores = numCores)
