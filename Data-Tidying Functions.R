## function for tidying the data to the point level of granularity
tidy_point_level <- function(raw_data) {
  formatted_point_level <- raw_data |> mutate(point_index = row_number()) |>
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
           player1_set1_lag = lag(player1_set1, default = 0),
           player1_set2_lag = lag(player1_set2, default = 0),
           player1_set3_lag = lag(player1_set3, default = 0),
           player1_set4_lag = lag(player1_set4, default = 0),
           player1_set5_lag = lag(player1_set5, default = 0),
           player2_set1_lag = lag(player2_set1, default = 0),
           player2_set2_lag = lag(player2_set2, default = 0),
           player2_set3_lag = lag(player2_set3, default = 0),
           player2_set4_lag = lag(player2_set4, default = 0),
           player2_set5_lag = lag(player2_set5, default = 0)) |>
    mutate(player1_game_lag = if_else(player1_game_lag == "GAME" | player2_game_lag == "GAME", 
                                      true = "0", 
                                      false = player1_game_lag)) |>
    mutate(player2_game_lag = if_else(player2_game_lag == "GAME" | player2_game_lag == "GAME", 
                                      true = "0", 
                                      false = player2_game_lag)) |>
    relocate(player1_game_lag, player2_game_lag, player1_game, player2_game, player1_set1, 
             player2_set1, player1_set1_lag, player2_set1_lag)
  
  return(formatted_point_level)
}

