## Zverev Plots

library(tidyverse)
library(viridis)

## TODO: ask Matt if there's a good way to modify filter_matches to have a default parameter for year and round
## feels clunky

## Zverev's 2022 Run to the semis:
zverev_2022 <- filter_matches(player = "Alexander Zverev", year_of_interest = "2022")

## Zverev's 2021 Run to the semis:
zverev_2021 <- filter_matches(player = "Alexander Zverev", year_of_interest = "2021")

zverev_2022_shots <- clean_shot_level(zverev_2022)
zverev_2021_shots <- clean_shot_level(zverev_2021)

## 2022:
zverev_2022_shots |> View()

zverev_2022_serves <- zverev_2022_shots |>
  filter(serverId == "Alexander Zverev") |>
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

zverev_2022_deuce <- zverev_2022_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

zverev_2022_ad <- zverev_2022_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

zverev_2022_deucead <- bind_rows(zverev_2022_deuce, zverev_2022_ad)

ggplot(data = zverev_2022_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = FALSE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Alexander Zverev Serves - 2022 Title Run")

## 2021:
zverev_2021_shots |> View()

zverev_2021_serves <- zverev_2021_shots |>
  filter(serverId == "Alexander Zverev") |>
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

zverev_2021_deuce <- zverev_2021_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

zverev_2021_ad <- zverev_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

zverev_2021_deucead <- bind_rows(zverev_2021_deuce, zverev_2021_ad)

ggplot(data = zverev_2021_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = FALSE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Alexander Zverev Serves - 2021 Finalist Run")







