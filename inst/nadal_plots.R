## Nadal Plots

library(tidyverse)
library(viridis)

## Nadal's 2022 Run to the French Open Title:
nadal_2022 <- filter_matches(player = "Rafael Nadal", year_of_interest = "2022")
## Nadal's 2021 Run to the semifinals, losing to Novak Djokovic
nadal_2021 <- filter_matches(player = "Rafael Nadal", year_of_interest = "2021")

nadal_2022_shots <- clean_shot_level(nadal_2022)
nadal_2021_shots <- clean_shot_level(nadal_2021)

## SERVES

## 2022:
nadal_2022_shots |> View()

nadal_2022_serves <- nadal_2022_shots |>
  filter(serverId == "Rafael Nadal") |>
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

## TODO: How do I get rid of the level thing on the legend?
ggplot(data = nadal_2022_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal Serve Locations - 2022 Title Run")

## First Serves Only:
nadal_2022_serves1 <- nadal_2022_shots |>
  filter(serverId == "Rafael Nadal") |>
  filter(serve == 1) |>
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

nadal_2022_deuce1 <- nadal_2022_serves1 |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2022_ad1 <- nadal_2022_serves1 |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2022_deucead1 <- bind_rows(nadal_2022_deuce1, nadal_2022_ad1)

ggplot(data = nadal_2022_deucead1, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal First Serve Locations - 2022 Title Run")


## 2021:
nadal_2021_shots |> View()

nadal_2021_serves <- nadal_2021_shots |>
  filter(serverId == "Rafael Nadal") |>
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
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal Serve Locations - 2021 Finalist Run")

## First Serves Only
nadal_2021_serves1 <- nadal_2021_shots |>
  filter(serverId == "Rafael Nadal") |>
  filter(serve == 1) |>
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

nadal_2021_deuce1 <- nadal_2021_serves1 |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2021_ad1 <- nadal_2021_serves1 |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

nadal_2021_deucead1 <- bind_rows(nadal_2021_deuce1, nadal_2021_ad1)

ggplot(data = nadal_2021_deucead1, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal First Serve Locations - 2021 Finalist Run")


## RETURNS

## 2022 Title Run
nadal_2022_returns <- nadal_2022_shots |>
  filter(receiverId == "Rafael Nadal") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(2) |>
  mutate(y = if_else(x < 0, true = -y, false = y),
         x = if_else(x < 0, true = -x, false = x),
         serve = as_factor(serve),
         serve = fct_relevel(serve, "1","2")) |>
  relocate(position, shot_index, x, y, z)

ggplot(data = nadal_2022_returns, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = serve)) +
  scale_color_manual(name = "Serve Type", values = c("black", "green3")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal Return Locations - 2022 Title Run")

## Joining for net clearance
nadal_2022_returns_clearance <- nadal_2022_shots |>
  filter(receiverId == "Rafael Nadal") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z)

nadal_2022_returns_joined <- left_join(nadal_2022_returns, nadal_2022_returns_clearance,
                                       by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = nadal_2022_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_b(name = "Net Clearance (m)", option = "rocket") +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Rafael Nadal Return Locations - 2022 Title Run")

## 2021 Finalist Run
nadal_2021_returns <- nadal_2021_shots |>
  filter(receiverId == "Rafael Nadal") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(2) |>
  mutate(y = if_else(x < 0, true = -y, false = y),
         x = if_else(x < 0, true = -x, false = x),
         serve = as_factor(serve),
         serve = fct_relevel(serve, "1","2")) |>
  relocate(position, shot_index, x, y, z)

ggplot(data = nadal_2021_returns, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = serve)) +
  scale_color_manual(name = "Serve Type", values = c("black", "green3")) +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Rafael Nadal Return Locations - 2021 Finalist Run")

## Joining for net clearance
nadal_2021_returns_clearance <- nadal_2021_shots |>
  filter(receiverId == "Rafael Nadal") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z)

nadal_2021_returns_joined <- left_join(nadal_2021_returns, nadal_2021_returns_clearance,
                                       by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = nadal_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_b(name = "Net Clearance (m)", option = "rocket") +
  scale_fill_brewer(palette = "Oranges") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Rafael Nadal Return Locations - 2021 Finalist Run")










