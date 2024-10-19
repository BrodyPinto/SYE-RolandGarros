library(tidyverse)
library(here)
library(pander)

## fetch_all_matches function - fetch and bind all matches given the following optional parameters: 
## player, year, round
fetch_all_matches <- function(player = "(.|\\s)*\\S(.|\\s)*", 
                              year = "(.|\\s)*\\S(.|\\s)*",
                              round = "(.|\\s)*\\S(.|\\s)*") {
  
  file_vec <- list.files(path = "data/")
  contains_year <- file_vec |>
    str_detect(pattern = as.character(year))
  contains_player <- file_vec |>
    str_detect(pattern = str_to_title(player))
  
  ## round should be F, SF, QF, R16, R32, R64, or R128
  contains_round <- file_vec |>
    str_detect(pattern = paste0("_", round, "_"))
  matches_of_interest <- file_vec[contains_year & contains_player & contains_round]
  
  ## add a match_id variable to all matches in matches_of_interest
  list_of_matches <- lapply(seq_along(matches_of_interest), function(i) {
    
    df <- read_csv(here::here(paste0("data/", matches_of_interest[i])))
    df <- df |> mutate(match_id = paste0(i)) |>
      relocate(match_id)
    
    return(df)
  })
  
  ## combine all data frames into a single data frame
  combined_df <- bind_rows(list_of_matches)
  
  return(combined_df)
}

federer_df <- fetch_all_matches(player = "Federer")
nadal_df <- fetch_all_matches(player = "Nadal")
djokovic_df <- fetch_all_matches(player = "Djokovic")

