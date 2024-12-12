#' Fetch Match Info Function
#'
#' This is a function that fetches all matches that match the player, year, and/or round of interest.
#'
#' @param player is a string of the player's name - first or last name (case insensitive)
#' @param year is a string of the year the match was played - between 2019 and 2023
#' @param round is a string of the round the match was played - F, SF, QF, R16, R32, R64, or R128
#' @return a list of strings with the info of all matches that match the player, year, and/or round of interest
#'
#' @examples
#' fetch_match_info(player = "Nadal", year = "2022", round = "F") ## fetches the 2022 French Open final match that Nadal played
#' fetch_match_info(player = "Federer") ## fetches all matches that Federer played
#'
#' @import tidyverse
#' @export

fetch_match_info <- function(year = "(.|\\s)*\\S(.|\\s)*",
                             player_last = "(.|\\s)*\\S(.|\\s)*",
                             round = "(.|\\s)*\\S(.|\\s)*") {
  ## list matches available: default gives all matches

  file_vec <- list.files(path = "inst/data/")
  contains_year <- file_vec |>
    str_detect(pattern = as.character(year))
  contains_player <- file_vec |>
    str_detect(pattern = str_to_title(player_last))

  ## round should be F, SF, QF, R16, R32, R64, or R128
  contains_round <- file_vec |>
    str_detect(pattern = paste0("_", round, "_"))
  matches_of_interest <- file_vec[contains_year & contains_player & contains_round]

  return(matches_of_interest)
}
