#' Filter Matches Function
#'
#' This is a function that finds all matches of a specified player, year, and/or round
#' using the all_matches_importance data frame
#'
#' @param player is a string of the player's name - first and last name (case sensitive)
#' @param year_of_interest is a string of the year the match was played - between 2019 and 2023
#' @return returns a point-level data frame of all matches given the specified player and year
#'
#' @import tidyverse
#' @export

filter_matches <- function(player = "(.|\\s)*\\S(.|\\s)*",
                           year_of_interest = "(.|\\s)*\\S(.|\\s)*") {

  filtered_df <- all_matches_importance |>
    # is_important variables
    mutate(atp_is_important = if_else(atp_importance >= 0.1, 1, 0),
           atp_is_important = as.logical(atp_is_important),
           wta_is_important = if_else(wta_importance >= 0.1, 1, 0),
           wta_is_important = as.logical(wta_is_important)) |>

    # Filter based on the parameters of the function
    filter(player1 == player | player2 == player) |>
    filter(year == year_of_interest) |>

    # Parse and combine match_score_overall for plot label
    mutate(
      set1_score = if_else(player == player1, paste(player1_set1, player2_set1, sep = "-"), paste(player2_set1, player1_set1, sep = "-")),
      set2_score = if_else(player == player1, paste(player1_set2, player2_set2, sep = "-"), paste(player2_set2, player1_set2, sep = "-")),
      set3_score = if_else(player == player1, paste(player1_set3, player2_set3, sep = "-"), paste(player2_set3, player1_set3, sep = "-")),
      set4_score = if_else(player == player1, paste(player1_set4, player2_set4, sep = "-"), paste(player2_set4, player1_set4, sep = "-")),
      set5_score = if_else(player == player1, paste(player1_set5, player2_set5, sep = "-"), paste(player2_set5, player1_set5, sep = "-")),

      match_score_overall = pmap_chr(
        list(set1_score, set2_score, set3_score, set4_score, set5_score),
        ~ str_c(
          discard(
            c(...),
            ~ str_count(.x, "-") != 1),
          collapse = ", "
        )
      )
    ) |>

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

    # Create plot_label variable
    mutate(plot_label = str_c(player2, round, match_score_overall, sep = "\n")) |>

    # Add numeric round before grouping
    mutate(round = factor(round, levels = c("R64", "R32", "R16", "QF", "SF", "F")),
           round_num = as.numeric(round)) |>

    # Create plot label and final plot label per match
    mutate(plot_label = str_c(player2, round, match_score_overall, sep = "\n")) |>
    group_by(match_id) |>
    mutate(plot_label_final = last(plot_label[!is.na(plot_label)])) |>
    ungroup() |>
    mutate(
      plot_label_final = as_factor(plot_label_final),
      plot_label_final = fct_reorder(plot_label_final, round_num)
    ) |>

    relocate(plot_label_final, set, player1_game_score, player2_game_score, player1_set_score, player2_set_score, player1, player2)

  return(filtered_df)

}
