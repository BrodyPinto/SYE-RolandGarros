#' Fetch All Matches
#'
#' This is a function that collects and binds all matches that match the player, year, and/or round of interest.
#'
#' @param player is a string of the player's name - first or last name (case insensitive)
#' @param year is a string of the year the match was played - between 2019 and 2023
#' @param round is a string of the round the match was played - F, SF, QF, R16, R32, R64, or R128
#' @return a data frame of all matches that match the player, year, and/or round of interest
#'
#' @examples
#' fetch_all_matches(player = "Nadal", year = "2022", round = "F") ## fetches the 2022 French Open final match that Nadal played
#' fetch_all_matches(player = "Federer") ## fetches all matches that Federer played
#'
#' @import tidyverse
#' @import here
#' @export

fetch_all_matches <- function(player = "(.|\\s)*\\S(.|\\s)*",
                              year = "(.|\\s)*\\S(.|\\s)*",
                              round = "(.|\\s)*\\S(.|\\s)*") {
  ## fetch_all_matches function - fetch and bind all matches given the following optional parameters:
  ## player, year, round

  file_vec <- list.files(path = "inst/data/")
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

    file_name <- matches_of_interest[i]
    df <- read_csv(here::here(paste0("inst/data/", file_name)))
    df <- df |> mutate(match_id = paste0(i)) |>
      mutate(match_info = file_name) |>
      mutate(match_id = as.numeric(match_id)) |>
      relocate(match_id, match_info)

    return(df)
  })

  # ## combine all data frames into a single data frame
  # combined_df <- bind_rows(list_of_matches)

  return(list_of_matches)
}

nadal_2022 = fetch_all_matches(player = "Nadal", year = 2022)
