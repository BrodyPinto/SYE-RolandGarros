#' Clean Point Level
#'
#' This is a function that cleans the Court Vision data to the *point* level of granularity.
#'
#' @param raw_data is a data frame of the raw Court Vision data (must be output from the fetch_all_matches function)
#' @param player_of_interest is a string of the player's name we want as player1 - first or last name (case insensitive)
#' @return a data frame with a row for each point played in the match/matches of interest
#'
#' @examples
#' nadal_final_2022 <- fetch_all_matches(player = "Nadal", year = "2022", round = "F")
#' clean_point_level(nadal_final_2022)
#'
#' @import tidyverse
#' @export

clean_point_level <- function(raw_data, player_of_interest = "(.|\\s)*\\S(.|\\s)*") {
  second_serve_points <- raw_data |>
    ## must use fetch_all_matches(player, year, round) function to get dataset with match_id variable
    group_by(match_id) |>
    ## match_info parsing:
    separate(match_info, into = c("label", "round", "opponents", "year", "court_number", "file_ending"), sep = "_") |>
    separate(opponents, into = c("player1", "player2"), sep = "-vs-") |>
    mutate(player1 = sub("-", " ", player1),
           player2 = sub("-", " ", player2)) |>
    mutate(point_index = row_number()) |>
    ## pointId parsing:
    separate(pointId, into = c("set", "game", "point", "serve"), sep = "_") |>
    mutate(set = as.numeric(set),
           game = as.numeric(game),
           point = as.numeric(point),
           serve = as.numeric(serve)) |>
    ## matchScore parsing:
    mutate(matchScore = sub("^.", "", matchScore)) |>
    mutate(matchScore = sub(".$", "", matchScore)) |>
    separate(matchScore, into = c("player1_set1_score",
                                  "player1_set2_score",
                                  "player1_set3_score",
                                  "player1_set4_score",
                                  "player1_set5_score",
                                  "player1_set1_tbscore",
                                  "player1_set2_tbscore",
                                  "player1_set3_tbscore",
                                  "player1_set4_tbscore",
                                  "player1_set5_tbscore",
                                  "player2_set1_score",
                                  "player2_set2_score",
                                  "player2_set3_score",
                                  "player2_set4_score",
                                  "player2_set5_score",
                                  "player2_set1_tbscore",
                                  "player2_set2_tbscore",
                                  "player2_set3_tbscore",
                                  "player2_set4_tbscore",
                                  "player2_set5_tbscore",
                                  "player1_game_score",
                                  "player2_game_score"), sep = ",") |>
    separate(player1_set1_score, into = c("label", "player1_set1"), sep = ": ") |>
    separate(player1_set2_score, into = c("label", "player1_set2"), sep = ": ") |>
    separate(player1_set3_score, into = c("label", "player1_set3"), sep = ": ") |>
    separate(player1_set4_score, into = c("label", "player1_set4"), sep = ": ") |>
    separate(player1_set5_score, into = c("label", "player1_set5"), sep = ": ") |>
    separate(player1_set1_tbscore, into = c("label", "player1_set1_tb"), sep = ": ") |>
    separate(player1_set2_tbscore, into = c("label", "player1_set2_tb"), sep = ": ") |>
    separate(player1_set3_tbscore, into = c("label", "player1_set3_tb"), sep = ": ") |>
    separate(player1_set4_tbscore, into = c("label", "player1_set4_tb"), sep = ": ") |>
    separate(player1_set5_tbscore, into = c("label", "player1_set5_tb"), sep = ": ") |>
    separate(player2_set1_score, into = c("label", "player2_set1"), sep = ": ") |>
    separate(player2_set2_score, into = c("label", "player2_set2"), sep = ": ") |>
    separate(player2_set3_score, into = c("label", "player2_set3"), sep = ": ") |>
    separate(player2_set4_score, into = c("label", "player2_set4"), sep = ": ") |>
    separate(player2_set5_score, into = c("label", "player2_set5"), sep = ": ") |>
    separate(player2_set1_tbscore, into = c("label", "player2_set1_tb"), sep = ": ") |>
    separate(player2_set2_tbscore, into = c("label", "player2_set2_tb"), sep = ": ") |>
    separate(player2_set3_tbscore, into = c("label", "player2_set3_tb"), sep = ": ") |>
    separate(player2_set4_tbscore, into = c("label", "player2_set4_tb"), sep = ": ") |>
    separate(player2_set5_tbscore, into = c("label", "player2_set5_tb"), sep = ": ") |>
    separate(player1_game_score, into = c("label", "player1_game"), sep = ": ") |>
    separate(player2_game_score, into = c("label", "player2_game"), sep = ": ") |>
    mutate(player1_set1 = parse_number(player1_set1),
           player1_set2 = parse_number(player1_set2),
           player1_set3 = parse_number(player1_set3),
           player1_set4 = parse_number(player1_set4),
           player1_set5 = parse_number(player1_set5),
           player2_set1 = parse_number(player2_set1),
           player2_set2 = parse_number(player2_set2),
           player2_set3 = parse_number(player2_set3),
           player2_set4 = parse_number(player2_set4),
           player2_set5 = parse_number(player2_set5),
           player1_game = sub("^.", "", player1_game),
           player1_game = sub(".$", "", player1_game),
           player2_game = sub("^.", "", player2_game),
           player2_game = sub(".$", "", player2_game)) |>
    filter(serve == 2)

  formatted_point_level <- raw_data |>
    ## must use fetch_all_matches(player, year, round) function to get dataset with match_id variable
    group_by(match_id) |>
    ## match_info parsing:
    separate(match_info, into = c("label", "round", "opponents", "year", "court_number", "file_ending"), sep = "_") |>
    separate(opponents, into = c("player1", "player2"), sep = "-vs-") |>
    mutate(player1 = sub("-", " ", player1),
           player2 = sub("-", " ", player2)) |>
    mutate(point_index = row_number()) |>
    ## pointId parsing:
    separate(pointId, into = c("set", "game", "point", "serve"), sep = "_") |>
    mutate(set = as.numeric(set),
           game = as.numeric(game),
           point = as.numeric(point),
           serve = as.numeric(serve)) |>
    ## matchScore parsing:
    mutate(matchScore = sub("^.", "", matchScore)) |>
    mutate(matchScore = sub(".$", "", matchScore)) |>
    separate(matchScore, into = c("player1_set1_score",
                                  "player1_set2_score",
                                  "player1_set3_score",
                                  "player1_set4_score",
                                  "player1_set5_score",
                                  "player1_set1_tbscore",
                                  "player1_set2_tbscore",
                                  "player1_set3_tbscore",
                                  "player1_set4_tbscore",
                                  "player1_set5_tbscore",
                                  "player2_set1_score",
                                  "player2_set2_score",
                                  "player2_set3_score",
                                  "player2_set4_score",
                                  "player2_set5_score",
                                  "player2_set1_tbscore",
                                  "player2_set2_tbscore",
                                  "player2_set3_tbscore",
                                  "player2_set4_tbscore",
                                  "player2_set5_tbscore",
                                  "player1_game_score",
                                  "player2_game_score"), sep = ",") |>
    separate(player1_set1_score, into = c("label", "player1_set1"), sep = ": ") |>
    separate(player1_set2_score, into = c("label", "player1_set2"), sep = ": ") |>
    separate(player1_set3_score, into = c("label", "player1_set3"), sep = ": ") |>
    separate(player1_set4_score, into = c("label", "player1_set4"), sep = ": ") |>
    separate(player1_set5_score, into = c("label", "player1_set5"), sep = ": ") |>
    separate(player1_set1_tbscore, into = c("label", "player1_set1_tb"), sep = ": ") |>
    separate(player1_set2_tbscore, into = c("label", "player1_set2_tb"), sep = ": ") |>
    separate(player1_set3_tbscore, into = c("label", "player1_set3_tb"), sep = ": ") |>
    separate(player1_set4_tbscore, into = c("label", "player1_set4_tb"), sep = ": ") |>
    separate(player1_set5_tbscore, into = c("label", "player1_set5_tb"), sep = ": ") |>
    separate(player2_set1_score, into = c("label", "player2_set1"), sep = ": ") |>
    separate(player2_set2_score, into = c("label", "player2_set2"), sep = ": ") |>
    separate(player2_set3_score, into = c("label", "player2_set3"), sep = ": ") |>
    separate(player2_set4_score, into = c("label", "player2_set4"), sep = ": ") |>
    separate(player2_set5_score, into = c("label", "player2_set5"), sep = ": ") |>
    separate(player2_set1_tbscore, into = c("label", "player2_set1_tb"), sep = ": ") |>
    separate(player2_set2_tbscore, into = c("label", "player2_set2_tb"), sep = ": ") |>
    separate(player2_set3_tbscore, into = c("label", "player2_set3_tb"), sep = ": ") |>
    separate(player2_set4_tbscore, into = c("label", "player2_set4_tb"), sep = ": ") |>
    separate(player2_set5_tbscore, into = c("label", "player2_set5_tb"), sep = ": ") |>
    separate(player1_game_score, into = c("label", "player1_game"), sep = ": ") |>
    separate(player2_game_score, into = c("label", "player2_game"), sep = ": ") |>
    mutate(player1_set1 = parse_number(player1_set1),
           player1_set2 = parse_number(player1_set2),
           player1_set3 = parse_number(player1_set3),
           player1_set4 = parse_number(player1_set4),
           player1_set5 = parse_number(player1_set5),
           player2_set1 = parse_number(player2_set1),
           player2_set2 = parse_number(player2_set2),
           player2_set3 = parse_number(player2_set3),
           player2_set4 = parse_number(player2_set4),
           player2_set5 = parse_number(player2_set5),
           player1_game = sub("^.", "", player1_game),
           player1_game = sub(".$", "", player1_game),
           player2_game = sub("^.", "", player2_game),
           player2_game = sub(".$", "", player2_game)) |>
    filter(serve == 1) |>
    ## lag the game score to get accurate game score:
    mutate(player1_game_lag = lag(player1_game, default = "0"),
           player2_game_lag = lag(player2_game, default = "0")) |>
    ## fill in the second serve points:
    bind_rows(second_serve_points) |>
    arrange(set, game, point, serve) |>
    fill(player1_game_lag, player2_game_lag, .direction = "down") |>
    mutate(player1_game_score = if_else(player1_game_lag == "GAME" | player2_game_lag == "GAME",
                                        true = "0",
                                        false = player1_game_lag)) |>
    mutate(player2_game_score = if_else(player1_game_lag == "GAME" | player2_game_lag == "GAME",
                                        true = "0",
                                        false = player2_game_lag)) |>
    ## fix tiebreak ending:
    mutate(
      # Safely convert to numeric, assigning NA if conversion fails
      player1_score_numeric = suppressWarnings(as.numeric(player1_game_score)),
      player2_score_numeric = suppressWarnings(as.numeric(player2_game_score)),

      # Create a flag for the condition
      reset_scores = !(player1_game_score %in% c("0", "15", "30", "40")) &
        !(player2_game_score %in% c("0", "15", "30", "40")) &
        !is.na(player1_score_numeric) & !is.na(player2_score_numeric) &
        player1_score_numeric + player2_score_numeric >= 12 &
        abs(player1_score_numeric - player2_score_numeric) == 2,

      # Use the flag to set both scores
      player1_game_score = if_else(reset_scores, "0", player1_game_score),
      player2_game_score = if_else(reset_scores, "0", player2_game_score)
    ) |>
    ## set score lag:
    mutate(player1_set1_lag = ifelse(serve == 2, lag(player1_set1, 2, default = 0), lag(player1_set1, default = 0)),
           player1_set2_lag = ifelse(serve == 2, lag(player1_set2, 2, default = 0), lag(player1_set2, default = 0)),
           player1_set3_lag = ifelse(serve == 2, lag(player1_set3, 2, default = 0), lag(player1_set3, default = 0)),
           player1_set4_lag = ifelse(serve == 2, lag(player1_set4, 2, default = 0), lag(player1_set4, default = 0)),
           player1_set5_lag = ifelse(serve == 2, lag(player1_set5, 2, default = 0), lag(player1_set5, default = 0)),
           player2_set1_lag = ifelse(serve == 2, lag(player2_set1, 2, default = 0), lag(player2_set1, default = 0)),
           player2_set2_lag = ifelse(serve == 2, lag(player2_set2, 2, default = 0), lag(player2_set2, default = 0)),
           player2_set3_lag = ifelse(serve == 2, lag(player2_set3, 2, default = 0), lag(player2_set3, default = 0)),
           player2_set4_lag = ifelse(serve == 2, lag(player2_set4, 2, default = 0), lag(player2_set4, default = 0)),
           player2_set5_lag = ifelse(serve == 2, lag(player2_set5, 2, default = 0), lag(player2_set5, default = 0))) |>
    mutate(player1_set_score = case_when(set == 1 ~ player1_set1_lag,
                                         set == 2 ~ player1_set2_lag,
                                         set == 3 ~ player1_set3_lag,
                                         set == 4 ~ player1_set4_lag,
                                         set == 5 ~ player1_set5_lag)) |>
    mutate(player2_set_score = case_when(set == 1 ~ player2_set1_lag,
                                         set == 2 ~ player2_set2_lag,
                                         set == 3 ~ player2_set3_lag,
                                         set == 4 ~ player2_set4_lag,
                                         set == 5 ~ player2_set5_lag)) |>
    relocate(player1_game_score, player2_game_score, player1_set_score, player2_set_score) |>
    # Store original player names and scores
    mutate(
      original_player1 = player1,
      original_player2 = player2,
      original_player1_game_score = player1_game_score,
      original_player2_game_score = player2_game_score,
      original_player1_set_score = player1_set_score,
      original_player2_set_score = player2_set_score
    ) |>

    # TODO: this isn't exactly working as intended - look at Botic match in nadal_2022

    # Rearrange players and scores based on whether they match "nadal"
    mutate(
      player1 = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player_of_interest)) ~ original_player1,
        str_detect(str_to_lower(original_player2), str_to_lower(player_of_interest)) ~ original_player2
      ),
      player2 = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player_of_interest)) ~ original_player2,
        str_detect(str_to_lower(original_player2), str_to_lower(player_of_interest)) ~ original_player1
      ),
      player1_game_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player_of_interest)) ~ original_player1_game_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player_of_interest)) ~ original_player2_game_score
      ),
      player2_game_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player_of_interest)) ~ original_player2_game_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player_of_interest)) ~ original_player1_game_score
      ),
      player1_set_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player_of_interest)) ~ original_player1_set_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player_of_interest)) ~ original_player2_set_score
      ),
      player2_set_score = case_when(
        str_detect(str_to_lower(original_player1), str_to_lower(player_of_interest)) ~ original_player2_set_score,
        str_detect(str_to_lower(original_player2), str_to_lower(player_of_interest)) ~ original_player1_set_score
      )
    ) |>
    relocate(set, player1_game_score, player2_game_score, player1_set_score, player2_set_score, player1, player2)

  return(formatted_point_level)
}

nadal_2022 <- fetch_all_matches(player = "Nadal", year = 2022)
