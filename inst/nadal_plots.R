## Nadal Plots

library(tidyverse)
library(viridis)

## Nadal's 2022 Run to the French Open Title:
nadal_2022 <- filter_matches(player = "Rafael Nadal", year_of_interest = "2022") # this only
## Nadal's 2021 Run to the semifinals, losing to Novak Djokovic
nadal_2021 <- filter_matches(player = "Rafael Nadal", year_of_interest = "2021")

nadal_2022_shots <- clean_shot_level(nadal_2022)
nadal_2021_shots <- clean_shot_level(nadal_2021)

## 2022:
nadal_2022_shots |> View()

nadal_2022_serves <- nadal_2022_shots |>
  filter(serverId == "Rafael Nadal") |>
  filter(position == "bounce") |>
  group_by(point_index) |>
  slice(1) |>
  relocate(position, shot_index, x, y, z) |>
  ungroup() |>
  filter(pointEndType != "Faulty Serve") |>
  filter(abs(x) < 6.410 & abs(x) > 0.5) |>
  mutate(break_point = if_else((player1_game_score %in% c(0,15,30) & player2_game_score == 40) |
                                 player2_game_score == "AD",
                               true = "Break Point",
                               false = "Not Break Point"))

nadal_2022_deuce <- nadal_2022_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2022_ad <- nadal_2022_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2022_deucead <- bind_rows(nadal_2022_deuce, nadal_2022_ad)

ggplot(data = nadal_2022_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = FALSE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal Serves - 2022 Title Run")

## 2021:
nadal_2021_shots |> View()

nadal_2021_serves <- nadal_2021_shots |>
  filter(serverId == "Rafael Nadal") |>
  filter(position == "bounce") |>
  group_by(point_index) |>
  slice(1) |>
  relocate(position, shot_index, x, y, z) |>
  ungroup() |>
  filter(pointEndType != "Faulty Serve") |>
  filter(abs(x) < 6.410 & abs(x) > 0.5) |>
  mutate(break_point = if_else((player1_game_score %in% c(0,15,30) & player2_game_score == 40) |
                                 player2_game_score == "AD",
                               true = "Break Point",
                               false = "Not Break Point"))

nadal_2021_deuce <- nadal_2021_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2021_ad <- nadal_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2021_deucead <- bind_rows(nadal_2021_deuce, nadal_2021_ad)

ggplot(data = nadal_2021_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = FALSE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal Serves - 2021 Finalist Run")







