library(tidyverse)

## function for cleaning the data to the POINT level of granularity:
tidy_point_level <- function(raw_data) {
  formatted_point_level <- raw_data |> 
    ## TODO: group by match_id
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
    mutate(player1_game = lag(player1_game, default = "0"),
           player2_game = lag(player2_game, default = "0"),
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
    mutate(player1_game = if_else(player1_game == "GAME" | player2_game == "GAME", 
                                  true = "0", 
                                  false = player1_game)) |>
    mutate(player2_game = if_else(player2_game == "GAME" | player2_game == "GAME", 
                                  true = "0", 
                                  false = player2_game)) |>
    relocate(player1_game, player2_game, player1_set1, player2_set1)
  
  return(formatted_point_level)
}

## function for cleaning the data to the SHOT level of granularity:
tidy_shot_level <- function(raw_data) {
  formatted_shot_level <- raw_data |> 
    ## TODO: group by match_id
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
    mutate(player1_game = lag(player1_game, default = "0"),
           player2_game = lag(player2_game, default = "0"),
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
    mutate(player1_game = if_else(player1_game == "GAME" | player2_game == "GAME", 
                                  true = "0", 
                                  false = player1_game)) |>
    mutate(player2_game = if_else(player2_game == "GAME" | player2_game == "GAME", 
                                  true = "0", 
                                  false = player2_game)) |>
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
    relocate(point_index, hit_count, x, y, z, position, pointEndType, player_hit, net_height, net_clearance,
             player1_game, player2_game, player1_set1, player2_set1, player1_set2, player2_set2,
             player1_set3, player2_set3, player1_set4, player2_set4, player1_set5, player2_set5)
  
  return(formatted_shot_level)
}

## isn't working right, look at row 11
View(tidy_point_level(test_match1))
View(tidy_shot_level(test_match1))



