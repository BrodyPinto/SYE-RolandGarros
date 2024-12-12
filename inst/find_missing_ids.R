library(tidyverse)
all_players <- bind_rows(read_csv(here::here("inst/player_ids/RG_players_2019.csv")),
                         read_csv(here::here("inst/player_ids/RG_players_2020.csv")),
                         read_csv(here::here("inst/player_ids/RG_players_2021.csv")),
                         read_csv(here::here("inst/player_ids/RG_players_2022.csv")),
                         read_csv(here::here("inst/player_ids/RG_players_2023.csv"))) |>
  distinct(id, .keep_all = TRUE) |>
  select(id, firstName, lastName)



players_to_add <- tibble(id = c(31132, 43970, 31976, 42340,
                                18005, 30524, 9525, 42560,
                                11713, 42559, 12105, 32390,
                                33597, 34635),
                         firstName = c("Andrey", "Holger", "Hubert", "Lorenzo",
                                       "Camila", "Fiona", "Gael", "Qinwen",
                                       "Juan Martin", "Elina", "Leonardo", "Sebastian",
                                       "Mikael", "Zdenek"),
                         lastName = c("RUBLEV", "RUNE", "HURKACZ", "MUSETTI",
                                      "GIORGI", "FERRO", "MONFILS", "ZHENG",
                                      "DEL POTRO", "AVANESYAN", "MAYER", "OFNER",
                                      "YMER", "KOLAR"))
all_players <- bind_rows(all_players, players_to_add)
## all_players should now have all ID's represented



## all_players |> filter(lastName == "ZVEREV")
## these are listed in order of the draw
## I think we can combine all of these player ids and just add the missing ones manually
all_match_df <- fetch_all_matches()
cleaned_point_level <- clean_and_combine_point(all_match_df)


merged_df <- left_join(cleaned_point_level, all_players, by = join_by(serverId == id)) |>
  relocate(firstName, lastName) |>
  rename(server_firstName = firstName,
         server_lastName = lastName) |>
  left_join(all_players, by = join_by(receiverId == id)) |>
  relocate(firstName, lastName) |>
  rename(receiver_firstName = firstName,
         receiver_lastName = lastName) |>
  left_join(all_players, by = join_by(scorerId == id)) |>
  relocate(firstName, lastName) |>
  rename(scorer_firstName = firstName,
         scorer_lastName = lastName)




## write code to figure out who won each match, according to:
## (1) who won the last point
## (2) who the p1/p2 scores say won the match

## which matches are missing score?
## only 29 matches are missing the scores, 151 have the scores
missing_score_df <- merged_df |> filter(is.na(player1_game_score))

has_score_df <- anti_join(merged_df, missing_score_df, join_by(match_id == match_id))

## according to who won the last point, the player in the 2nd and 3rd column won the match
winners_by_last_point <- has_score_df |>
  slice(n()) |>
  relocate(round) |>
  select(round, scorer_firstName, scorer_lastName)
winners_by_last_point |> print(n = Inf)

View(winners_by_last_point)

## who won the match,
## according to who had the higher score at the final point of the match
winners_by_score <- has_score_df |> slice(n()) |>
  mutate(winner = case_when(player1_set_score > player2_set_score ~ player1,
                            player1_set_score < player2_set_score ~ player2,
                            player1_set_score == player2_set_score ~ "look_at_tiebreak")) |>
  ## warning message just comes from the following line, which is fine:
  ## we are only using this code for the matches that ended in a tiebreak and the warnings
  ## should be coming from other matches
  mutate(player1_tiebreak = if_else(as.numeric(player1_game_score) > as.numeric(player2_game_score),
                                    true = player1,
                                    false = player2)) |>
  mutate(winner = if_else(winner == "look_at_tiebreak",
                          true = player1_tiebreak,
                          false = winner)) |>
  relocate(winner) |>
  select(winner)
winners_by_score |> print(n = Inf)

bind_cols(winners_by_last_point, winners_by_score) |>
  View()
## p1/p2 order correct for 140 matches, incorrect for 11 matches, nearly all of which have
## hyphenated or a space in their first or last name

## mismatches:
## 123: Davidovich Fokina
## 116: carreno busta
## 114: karolina-schmiedlova
## 110 haddad maia
## 103 badosa
## 98 haddad maia
## 46: van-de-zandschulp
## 41: kwon
## 35: gaston
## 28: koepfer
## 21: tomas martin etcheverry




# match_one_row <- all_match_df |> distinct(match_id, .keep_all = TRUE)
# match_one_row |> slice(60) |>
#   pull(serverId)
# missing_ids <- left_join(match_one_row, all_players, by = join_by(serverId == id)) |>
#   relocate(firstName, lastName) |>
#   left_join(all_players, by = join_by(receiverId == id)) |>
#   relocate(firstName.y, lastName.y) |>
#   print(n = Inf)
# view(missing_ids)
#
#
# left_join(match_one_row, all_players, by = join_by(receiverId == id)) |>
#   relocate(firstName, lastName) |>
#   print(n = Inf)
