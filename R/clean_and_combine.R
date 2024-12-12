#' Clean and Combine Function
#'
#' This is a function that cleans the Court Vision data and combines all matches of interest into a data frame.
#' The clean_point_level() function is used to clean one match at a time.
#'
#' @param list_of_matches is a list of all matches of interest - from the fetch_all_matches() function
#' @param player_interest is a string of the player's name we want as player1 - first or last name (case insensitive)
#' @return returns a cleaned data frame with all matches of interest (point level of granularity)
#'
#' @examples
#' nadal_2022 <- fetch_all_matches(player = "Nadal", year = 2022)
#' clean_and_combine(nadal_2022, player_interest = "Nadal")
#'
#' @import tidyverse
#' @export

clean_and_combine <- function(list_of_matches, player_interest = "(.|\\s)*\\S(.|\\s)*") {
  ## create an empty list to store the cleaned matches
  list_of_cleaned_matches <- list()

  for (match_df in list_of_matches) {
    ## clean the match_df to the point level
    match_clean <- clean_point_level(match_df, player_of_interest = player_interest)

    ## add match_clean to a list of cleaned matches
    list_of_cleaned_matches[[length(list_of_cleaned_matches) + 1]] <- match_clean
  }

  ## bind rows of all cleaned matches together
  all_cleaned_matches <- bind_rows(list_of_cleaned_matches)

  return(all_cleaned_matches) ## return the combined data frame
}
