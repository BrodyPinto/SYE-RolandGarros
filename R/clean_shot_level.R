#' Clean Shot Level
#'
#' This is a function that cleans the Court Vision data to the *shot* level of granularity.
#'
#' @param raw_data is a data frame of the raw Court Vision data (must be output from the fetch_all_matches function)
#' @param player_of_interest is a string of the player's name we want as player1 - first or last name (case insensitive)
#' @return returns a data frame with several rows (hit, net, peak, bounce) for each shot in the match/matches of interest
#'
#' @examples
#' nadal_final_2022 <- fetch_all_matches(player = "Nadal", year = "2022", round = "F")
#' clean_shot_level(nadal_final_2022)
#'
#' @import tidyverse
#' @export

clean_shot_level <- function(raw_data, player_of_interest = "(.|\\s)*\\S(.|\\s)*") {
  formatted_shot_level <- raw_data |>
    ## must use fetch_all_matches(player, year, round) function to get dataset with match_id variable
    group_by(match_id) |>
    ## match_info parsing:
    separate(match_info, into = c("label", "round", "opponents", "year", "court_number", "file_ending"), sep = "_") |>
    separate(opponents, into = c("player1", "player2"), sep = "-vs-") |>
    mutate(player1 = sub("-", " ", player1),
           player2 = sub("-", " ", player2)) |>
    mutate(point_index = row_number()) |>
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
    ## lag the game score to get accurate game score:
    mutate(player1_game_lag = lag(player1_game, default = "0"),
           player2_game_lag = lag(player2_game, default = "0"),
           player1_set1 = lag(player1_set1, default = 0),
           player1_set2 = lag(player1_set2, default = 0),
           player1_set3 = lag(player1_set3, default = 0),
           player1_set4 = lag(player1_set4, default = 0),
           player1_set5 = lag(player1_set5, default = 0),
           player2_set1 = lag(player2_set1, default = 0),
           player2_set2 = lag(player2_set2, default = 0),
           player2_set3 = lag(player2_set3, default = 0),
           player2_set4 = lag(player2_set4, default = 0),
           player2_set5 = lag(player2_set5, default = 0)) |>
    mutate(player1_game_score = if_else(player1_game_lag == "GAME" | player2_game_lag == "GAME",
                                        true = "0",
                                        false = player1_game_lag)) |>
    mutate(player2_game_score = if_else(player1_game_lag == "GAME" | player2_game_lag == "GAME",
                                        true = "0",
                                        false = player2_game_lag)) |>
    ## get the first numeric value from pointId whose format is: set_game_point_serve
    mutate(set = parse_number(pointId)) |>
    mutate(player1_set_score = case_when(set == 1 ~ player1_set1,
                                         set == 2 ~ player1_set2,
                                         set == 3 ~ player1_set3,
                                         set == 4 ~ player1_set4,
                                         set == 5 ~ player1_set5)) |>
    mutate(player2_set_score = case_when(set == 1 ~ player2_set1,
                                         set == 2 ~ player2_set2,
                                         set == 3 ~ player2_set3,
                                         set == 4 ~ player2_set4,
                                         set == 5 ~ player2_set5)) |>
    ## trajectoryData parsing:
    mutate(trajectoryData = sub("^..", "", trajectoryData)) |>
    mutate(trajectoryData = sub("..$", "", trajectoryData)) |>
    separate_longer_delim(trajectoryData, delim = "}, {") |>
    group_by(point_index) |>
    mutate(shot_index = row_number()) |>
    separate(trajectoryData, into = c("x", "y", "z", "position"), sep = "\\,") |>
    mutate(x = parse_number(x),
           y = parse_number(y),
           z = parse_number(z),
           position = sub("^.............", "", position)) |>
    mutate(position = gsub("'", "", position)) |>
    ## player_hit variable construction:
    mutate(is_hit = if_else(position == "hit", true = 1, false = 0)) |>
    group_by(point_index) |>
    mutate(hit_count = cumsum(is_hit)) |>
    mutate(player_hit = if_else(hit_count %% 2 == 1, serverId, receiverId)) |>
    ## net_height and net_clearance variables:
    mutate(net_height = 0.00619 * (y^2) + 0.914) |>
    mutate(net_clearance = z - net_height) |>
    relocate(match_id, set, player1_game_score, player2_game_score, player1_set_score, player2_set_score, x, y, z, position)

  return(formatted_shot_level)
}
