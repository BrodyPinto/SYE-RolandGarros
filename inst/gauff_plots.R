## Gauff Plots

library(tidyverse)
library(viridis)

## Gauff's 2022 Run to the French Open Final, losing to Iga Swiatek:
gauff_2022 <- filter_matches(player = "Coco Gauff", year_of_interest = "2022")
## Gauff's 2021 Run to the quarterfinals, losing to Barbora Krejcikova
gauff_2021 <- filter_matches(player = "Coco Gauff", year_of_interest = "2021")

gauff_2022_shots <- clean_shot_level(gauff_2022)
gauff_2021_shots <- clean_shot_level(gauff_2021)

## SERVES

## 2022 Title Run:
gauff_2022_shots |> View()

gauff_2022_serves <- gauff_2022_shots |>
  filter(serverId == "Coco Gauff") |>
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

gauff_2022_deuce <- gauff_2022_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # flip points to the right side of the net to prevent induced concentration
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y))

gauff_2022_ad <- gauff_2022_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

gauff_2022_deucead <- bind_rows(gauff_2022_deuce, gauff_2022_ad)

ggplot(data = gauff_2022_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.5, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Coco Gauff Serves - 2022 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

## First plot with is_important coloring
ggplot(data = gauff_2022_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, aes(color = atp_is_important), show.legend = TRUE) +
  scale_colour_manual(name = "Important Point", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Coco Gauff Serves - 2022 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

## 2021:
gauff_2021_shots |> View()

gauff_2021_serves <- gauff_2021_shots |>
  filter(serverId == "Coco Gauff") |>
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

gauff_2021_deuce <- gauff_2021_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # flip points to the right side of the net to prevent induced concentration
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y))

gauff_2021_ad <- gauff_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

gauff_2021_deucead <- bind_rows(gauff_2021_deuce, gauff_2021_ad)

ggplot(data = gauff_2021_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, aes(color = break_point), show.legend = TRUE) +
  scale_colour_manual(values = c("green", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Coco Gauff Serves - 2021 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

ggplot(data = gauff_2021_deucead, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, aes(color = atp_is_important), show.legend = TRUE) +
  scale_colour_manual(name = "Important Point", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Coco Gauff Serves - 2021 Finalist Run") +
  coord_flip() +
  scale_y_reverse()

gauff_2021_serves |> View()

## RETURNS

## 2022 Title Run
gauff_2022_returns <- gauff_2022_shots |>
  filter(receiverId == "Coco Gauff") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(2) |>
  mutate(y = if_else(x < 0, true = -y, false = y),
         x = if_else(x < 0, true = -x, false = x),
         serve = as_factor(serve),
         serve = fct_relevel(serve, "1","2")) |>
  relocate(position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

ggplot(data = gauff_2022_returns, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = serve)) +
  scale_color_manual(name = "Serve Type", values = c("black", "green3")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Coco Gauff Return Locations - 2022 Title Run")

## Joining for net clearance
gauff_2022_returns_clearance <- gauff_2022_shots |>
  filter(receiverId == "Coco Gauff") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

gauff_2022_returns_joined <- left_join(gauff_2022_returns, gauff_2022_returns_clearance,
                                         by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = gauff_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Coco Gauff Return Locations - 2022 Title Run")

## COLORING BY BREAK POINT
ggplot(data = gauff_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Coco Gauff Return Locations - 2022 Title Run")

## COLORING BY IMPORTANCE
ggplot(data = gauff_2022_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = wta_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Coco Gauff Return Locations - 2022 Title Run") +
  coord_flip() +
  scale_y_reverse()

## COLORING BY IS_IMPORTANT
ggplot(data = gauff_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = FALSE, aes(color = wta_is_important.x)) +
  scale_colour_manual(name = "Is important?", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x, nrow = 1) +
  labs(title = "Coco Gauff Return Locations - 2022 Title Run")

## 2021 Quarterfinalist Run:
gauff_2021_returns <- gauff_2021_shots |>
  filter(receiverId == "Coco Gauff") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(2) |>
  mutate(y = if_else(x < 0, true = -y, false = y),
         x = if_else(x < 0, true = -x, false = x),
         serve = as_factor(serve),
         serve = fct_relevel(serve, "1","2")) |>
  relocate(position, shot_index, x, y, z)

ggplot(data = gauff_2021_returns, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = serve)) +
  scale_color_manual(name = "Serve Type", values = c("black", "green3")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2) +
  labs(title = "Coco Gauff Return Locations - 2021 Quarterfinal Run") +
  coord_flip() +
  scale_y_reverse()

## Joining for net clearance
gauff_2021_returns_clearance <- gauff_2021_shots |>
  filter(receiverId == "Coco Gauff") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z)

gauff_2021_returns_joined <- left_join(gauff_2021_returns, gauff_2021_returns_clearance,
                                         by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = gauff_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Coco Gauff Return Locations - 2021 Quarterfinal Run") +
  coord_flip() +
  scale_y_reverse()

## COLORING BY BREAK POINT
ggplot(data = gauff_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Coco Gauff Return Locations - 2021 Quarterfinal Run") +
  coord_flip() +
  scale_y_reverse()

## COLORING BY IMPORTANCE
ggplot(data = gauff_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = wta_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Coco Gauff Return Locations - 2021 Quarterfinal Run") +
  coord_flip() +
  scale_y_reverse()

## COLORING BY IS_IMPORTANT
ggplot(data = gauff_2021_returns_joined, aes(x = x.x, y = y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.6, size = 1.2, show.legend = TRUE, aes(color = wta_is_important.x)) +
  scale_colour_manual(name = "Is important?", values = c("black", "green")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~player2.x) +
  labs(title = "Coco Gauff Return Locations - 2021 Quarterfinal Run") +
  coord_flip() +
  scale_y_reverse()

## Histogram for net clearance
ggplot(data = gauff_2022_returns_joined, aes(net_clearance.y)) +
  geom_histogram()
