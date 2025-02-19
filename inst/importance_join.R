## Join all_matches and atp_importance

all_matches |> filter(year != "2019") |>
  relocate(player1_set_score, player2_set_score, player1_set1_lag, player2_set1_lag,
           player1_set2_lag, player2_set2_lag, player1_set3_lag, player2_set3_lag) |> View()

join_ready_df <- all_matches |>
  filter(year != "2019") |>

  ## Correct Player Names
  mutate(
    serverId = case_when(
      serverId == "Cori Gauff" ~ "Coco Gauff",
      serverId == "Alejandro Davidovich Fokina" ~ "Alejandro Davidovich-Fokina",
      serverId == "Tomas Martin Etcheverry" ~ "Tomas Martin-Etcheverry",
      serverId == "Beatriz Haddad Maia" ~ "Beatriz Haddad-Maia",
      serverId == "Pablo Carreno Busta" ~ "Pablo Carreno-Busta",
      serverId == "Bernabe Zapata Miralles" ~ "Bernabe Zapata-Miralles",
      serverId == "Anna Karolina Schmiedlova" ~ "Anna Karolina-Schmiedlova",
      serverId == "Jan-Lennard Struff" ~ "Jan Lennard-Struff",
      serverId == "Irina-Camelia Begu" ~ "Irina Camelia-Begu",
      serverId == "Juan Pablo Varillas" ~ "Juan Pablo-Varillas",
      serverId == "Sara Sorribes Tormo" ~ "Sara Sorribes-Tormo",
      serverId == "Botic Van De Zandschulp" ~ "Botic Van-De-Zandschulp",
      serverId == "Genaro Alberto Olivieri" ~ "Genaro Alberto-Olivieri",
      serverId == "Thiago Seyboth Wild" ~ "Thiago Seyboth-Wild",
      TRUE ~ serverId
    ),
    receiverId = case_when(
      receiverId == "Cori Gauff" ~ "Coco Gauff",
      receiverId == "Alejandro Davidovich Fokina" ~ "Alejandro Davidovich-Fokina",
      receiverId == "Tomas Martin Etcheverry" ~ "Tomas Martin-Etcheverry",
      receiverId == "Beatriz Haddad Maia" ~ "Beatriz Haddad-Maia",
      receiverId == "Pablo Carreno Busta" ~ "Pablo Carreno-Busta",
      receiverId == "Bernabe Zapata Miralles" ~ "Bernabe Zapata-Miralles",
      receiverId == "Anna Karolina Schmiedlova" ~ "Anna Karolina-Schmiedlova",
      receiverId == "Jan-Lennard Struff" ~ "Jan Lennard-Struff",
      receiverId == "Irina-Camelia Begu" ~ "Irina Camelia-Begu",
      receiverId == "Juan Pablo Varillas" ~ "Juan Pablo-Varillas",
      receiverId == "Sara Sorribes Tormo" ~ "Sara Sorribes-Tormo",
      receiverId == "Botic Van De Zandschulp" ~ "Botic Van-De-Zandschulp",
      receiverId == "Genaro Alberto Olivieri" ~ "Genaro Alberto-Olivieri",
      receiverId == "Thiago Seyboth Wild" ~ "Thiago Seyboth-Wild",
      TRUE ~ receiverId
    ),
    scorerId = case_when(
      scorerId == "Cori Gauff" ~ "Coco Gauff",
      scorerId == "Alejandro Davidovich Fokina" ~ "Alejandro Davidovich-Fokina",
      scorerId == "Tomas Martin Etcheverry" ~ "Tomas Martin-Etcheverry",
      scorerId == "Beatriz Haddad Maia" ~ "Beatriz Haddad-Maia",
      scorerId == "Pablo Carreno Busta" ~ "Pablo Carreno-Busta",
      scorerId == "Bernabe Zapata Miralles" ~ "Bernabe Zapata-Miralles",
      scorerId == "Anna Karolina Schmiedlova" ~ "Anna Karolina-Schmiedlova",
      scorerId == "Jan-Lennard Struff" ~ "Jan Lennard-Struff",
      scorerId == "Irina-Camelia Begu" ~ "Irina Camelia-Begu",
      scorerId == "Juan Pablo Varillas" ~ "Juan Pablo-Varillas",
      scorerId == "Sara Sorribes Tormo" ~ "Sara Sorribes-Tormo",
      scorerId == "Botic Van De Zandschulp" ~ "Botic Van-De-Zandschulp",
      scorerId == "Genaro Alberto Olivieri" ~ "Genaro Alberto-Olivieri",
      scorerId == "Thiago Seyboth Wild" ~ "Thiago Seyboth-Wild",
      TRUE ~ scorerId
    )
  ) |>
  group_by(match_id) |>
  mutate(server_game_score = case_when(serverId == player1 ~ player1_game_score,
                                       serverId == player2 ~ player2_game_score),
         receiver_game_score = case_when(receiverId == player1 ~ player1_game_score,
                                         receiverId == player2 ~ player2_game_score),
         server_set_score = case_when(serverId == player1 ~ player1_set_score,
                                      serverId == player2 ~ player2_set_score),
         receiver_set_score = case_when(receiverId == player1 ~ player1_set_score,
                                        receiverId == player2 ~ player2_set_score)) |>

  ## Calculate match scores
  mutate(player1_match_score = 0,
         player2_match_score = 0) |>
  mutate(player1_match_score = case_when(
    set == 1 ~ 0,
    (player1_set1_lag >= 6 & player1_set1_lag > player2_set1_lag) ~ (player1_match_score + 1),
    (player1_set2_lag >= 6 & player1_set2_lag > player2_set2_lag) ~ (player1_match_score + 1),
    (player1_set3_lag >= 6 & player1_set3_lag > player2_set3_lag) ~ (player1_match_score + 1),
    (player1_set4_lag >= 6 & player1_set4_lag > player2_set4_lag) ~ (player1_match_score + 1),
    (player1_set5_lag >= 6 & player1_set5_lag > player2_set5_lag) ~ (player1_match_score + 1),
    TRUE ~ player1_match_score)) |>
  mutate(player2_match_score = case_when(
    set == 1 ~ 0,
    (player2_set1_lag >= 6 & player2_set1_lag > player1_set1_lag) ~ (player2_match_score + 1),
    (player2_set2_lag >= 6 & player2_set2_lag > player1_set2_lag) ~ (player2_match_score + 1),
    (player2_set3_lag >= 6 & player2_set3_lag > player1_set3_lag) ~ (player2_match_score + 1),
    (player2_set4_lag >= 6 & player2_set4_lag > player1_set4_lag) ~ (player2_match_score + 1),
    (player2_set5_lag >= 6 & player2_set5_lag > player1_set5_lag) ~ (player2_match_score + 1),
    TRUE ~ player2_match_score)) |>
  mutate(server_match_score = case_when(serverId == player1 ~ player1_match_score,
                                        serverId == player2 ~ player2_match_score),
         receiver_match_score = case_when(receiverId == player1 ~ player1_match_score,
                                          receiverId == player2 ~ player2_match_score)) |>

  ## Combine scores
  mutate(game_score = paste(server_game_score, receiver_game_score, sep = "-"),
         set_score = paste(server_set_score, receiver_set_score, sep = "-"),
         match_score = paste(server_match_score, receiver_match_score, sep = "-")) |>

  ## Handle AD-40 and 40-AD game scores:
  mutate(game_score = case_when(game_score == "AD-40" ~ "40-30",
                                game_score == "40-AD" ~ "30-40",
                                TRUE ~ game_score)) |>

  ## TODO: still need to handle tiebreak scores like higher than 6-6
  relocate(server_game_score, receiver_game_score, game_score, server_set_score, receiver_set_score,
           set_score, server_match_score, receiver_match_score, match_score)

atp_importance_5 <- atp_importance |> filter(bestof == 5)

all_matches_importance <- join_ready_df |>
  left_join(atp_importance_5, by = c("game_score" = "point_score",
                                     "set_score" = "game_score",
                                     "match_score" = "set_score")) |>
  relocate(game_score, set_score, match_score, importance)

## TODO: still a couple strange things going on with the score, but overall mostly good :)
all_matches_importance |> filter(is.na(importance)) |> View()

