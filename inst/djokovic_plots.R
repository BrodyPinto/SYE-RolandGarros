## Djokovic Plots

library(tidyverse)
library(viridis)

## Djokovic's 2020 Run to the French Open finals, losing to Nadal:
djokovic_2020 <- filter_matches(player = "Novak Djokovic", year_of_interest = "2020")
## Djokovic's 2021 Run to the championship
djokovic_2021 <- filter_matches(player = "Novak Djokovic", year_of_interest = "2021")

djokovic_2020_shots <- clean_shot_level(djokovic_2020)
djokovic_2021_shots <- clean_shot_level(djokovic_2021)

## 2022:
djokovic_2020_shots |> View()

djokovic_2020_serves <- djokovic_2020_shots |>
  filter(serverId == "Novak Djokovic") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(1) |>
  relocate(position, shot_index, x, y, z) |>
  ungroup() |>
  filter(pointEndType != "Faulty Serve") |>
  filter(abs(x) < 6.410 & abs(x) > 0.5) |>
  mutate(break_point = if_else((player1_game_score %in% c(0,15,30) & player2_game_score == 40) |
                                 player2_game_score == "AD",
                               true = "Break Point",
                               false = "Not Break Point"))

djokovic_2020_deuce <- djokovic_2020_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

djokovic_2020_ad <- djokovic_2020_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

djokovic_2020_deucead <- bind_rows(djokovic_2020_deuce, djokovic_2020_ad)

ggplot(data = djokovic_2020_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = FALSE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Serves - 2020 Finalist Run")

## 2021:
djokovic_2021_shots |> View()

djokovic_2021_serves <- djokovic_2021_shots |>
  filter(serverId == "Novak Djokovic") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(1) |>
  relocate(position, shot_index, x, y, z) |>
  ungroup() |>
  filter(pointEndType != "Faulty Serve") |>
  filter(abs(x) < 6.410 & abs(x) > 0.5) |>
  mutate(break_point = if_else((player1_game_score %in% c(0,15,30) & player2_game_score == 40) |
                                 player2_game_score == "AD",
                               true = "Break Point",
                               false = "Not Break Point"))

djokovic_2021_deuce <- djokovic_2021_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

djokovic_2021_ad <- djokovic_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

djokovic_2021_deucead <- bind_rows(djokovic_2021_deuce, djokovic_2021_ad)

ggplot(data = djokovic_2021_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = FALSE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Serves - 2021 Title Run")

djokovic_2021_deucead |>
  filter(player2 == "Ricardas Berankis")

djokovic_2021_serves |> View()






