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
         x = if_else(x > 1, true = -x, false = x)) |>
  # flip points to the right side of the net to prevent induced concentration
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y))

djokovic_2020_ad <- djokovic_2020_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

djokovic_2020_deucead <- bind_rows(djokovic_2020_deuce, djokovic_2020_ad)

ggplot(data = djokovic_2020_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Serves - 2020 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

## First plot with is_important coloring
ggplot(data = djokovic_2020_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, aes(color = atp_is_important), show.legend = TRUE) +
  scale_colour_manual(name = "Important Point", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Serves - 2020 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

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
         x = if_else(x > 1, true = -x, false = x)) |>
  # flip points to the right side of the net to prevent induced concentration
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y))

djokovic_2021_ad <- djokovic_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

djokovic_2021_deucead <- bind_rows(djokovic_2021_deuce, djokovic_2021_ad)

ggplot(data = djokovic_2021_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Serves - 2021 Title Run") +
  coord_flip() +
  scale_y_reverse()

ggplot(data = djokovic_2021_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, aes(color = atp_is_important), show.legend = TRUE) +
  scale_colour_manual(name = "Important Point", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Serves - 2021 Title Run") +
  coord_flip() +
  scale_y_reverse()

djokovic_2021_serves |> View()

## RETURNS

## 2020 Finalist Run
djokovic_2020_returns <- djokovic_2020_shots |>
  filter(receiverId == "Novak Djokovic") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(2) |>
  mutate(y = if_else(x < 0, true = -y, false = y),
         x = if_else(x < 0, true = -x, false = x),
         serve = as_factor(serve),
         serve = fct_relevel(serve, "1","2")) |>
  relocate(position, shot_index, x, y, z)

ggplot(data = djokovic_2020_returns, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = serve)) +
  scale_color_manual(name = "Serve Type", values = c("black", "green3")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Return Locations - 2020 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

## Joining for net clearance
djokovic_2020_returns_clearance <- djokovic_2020_shots |>
  filter(receiverId == "Novak Djokovic") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z)

djokovic_2020_returns_joined <- left_join(djokovic_2020_returns, djokovic_2020_returns_clearance,
                                          by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## TODO: I think NA's in net clearance are coming from errors in data collection? join seems to be fine
View(djokovic_2020_returns |> relocate(net_clearance, match_id, set, game, point, hit_count))

View(djokovic_2020_returns_joined |> relocate(net_clearance.y, match_id, set, game, point, hit_count))

## THIS IS COOL 😎
ggplot(data = djokovic_2020_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Novak Djokovic Return Locations - 2020 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

## 2021 Title Run
djokovic_2021_returns <- djokovic_2021_shots |>
  filter(receiverId == "Novak Djokovic") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(2) |>
  mutate(y = if_else(x < 0, true = -y, false = y),
         x = if_else(x < 0, true = -x, false = x),
         serve = as_factor(serve),
         serve = fct_relevel(serve, "1","2")) |>
  relocate(position, shot_index, x, y, z)

ggplot(data = djokovic_2021_returns, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = serve)) +
  scale_color_manual(name = "Serve Type", values = c("black", "green3")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Novak Djokovic Return Locations - 2021 Title Run") +
  coord_flip() +
  scale_y_reverse()

## Joining for net clearance
djokovic_2021_returns_clearance <- djokovic_2021_shots |>
  filter(receiverId == "Novak Djokovic") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z)

djokovic_2021_returns_joined <- left_join(djokovic_2021_returns, djokovic_2021_returns_clearance,
                                          by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = djokovic_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Novak Djokovic Return Locations - 2021 Title Run") +
  coord_flip() +
  scale_y_reverse()

## COLORING BY BREAK POINT
ggplot(data = djokovic_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Novak Djokovic Return Locations - 2021 Title Run") +
  coord_flip() +
  scale_y_reverse()

## COLORING BY IMPORTANCE
ggplot(data = djokovic_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = atp_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Novak Djokovic Return Locations - 2021 Title Run") +
  coord_flip() +
  scale_y_reverse()

## COLORING BY IS_IMPORTANT
ggplot(data = djokovic_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = atp_is_important.x)) +
  scale_colour_manual(name = "Is important?", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Novak Djokovic Return Locations - 2021 Title Run") +
  coord_flip() +
  scale_y_reverse()

## Histogram for net clearance
ggplot(data = djokovic_2020_returns_joined, aes(net_clearance.y)) +
  geom_histogram()






