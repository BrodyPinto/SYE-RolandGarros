#' Filter Matches Function
#'
#' This is a function that finds all matches of a specified player, year, and/or round
#'
#' @param player is a string of the player's name - first or last name (case insensitive)
#' @param year is a string of the year the match was played - between 2019 and 2023
#' @param round is a string of the round the match was played - F, SF, QF, R16, R32, R64, or R128
#' @return returns a point-level data frame of all matches given the specified player, year, and/pr round
#'
#' @import tidyverse
#' @export

filter_matches <- function(player = "(.|\\s)*\\S(.|\\s)*",
                           year = "(.|\\s)*\\S(.|\\s)*",
                           round = "(.|\\s)*\\S(.|\\s)*") {

  filtered_df <- all_matches_combined |>
    # Filter based on the paramters of the function
    filter(player1 == player | player2 == player,
           year == year,
           round == round) |>

    # Store original player names and scores
    mutate(
      original_player1 = player1,
      original_player2 = player2,
      original_player1_game_score = player1_game_score,
      original_player2_game_score = player2_game_score,
      original_player1_set_score = player1_set_score,
      original_player2_set_score = player2_set_score) |>

    # Rearrange players and scores based on whether they match the player
    mutate(
      player1 = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player)) ~ original_player1,
        str_detect(str_to_lower(original_player2), str_to_lower(player)) ~ original_player2),
      player2 = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player)) ~ original_player2,
        str_detect(str_to_lower(original_player2), str_to_lower(player)) ~ original_player1),
      player1_game_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player)) ~ original_player1_game_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player)) ~ original_player2_game_score),
      player2_game_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player)) ~ original_player2_game_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player)) ~ original_player1_game_score),
      player1_set_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player)) ~ original_player1_set_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player)) ~ original_player2_set_score),
      player2_set_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player)) ~ original_player2_set_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player)) ~ original_player1_set_score)) |>
    relocate(set, player1_game_score, player2_game_score, player1_set_score, player2_set_score, player1, player2)

  return(filtered_df)

}
