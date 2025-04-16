## Nadal Plots

library(tidyverse)
library(viridis)

## Nadal's 2022 Run to the French Open Title:
nadal_2022 <- filter_matches(player = "Rafael Nadal", year_of_interest = "2022")
## Nadal's 2021 Run to the semifinals, losing to Novak Djokovic
nadal_2021 <- filter_matches(player = "Rafael Nadal", year_of_interest = "2021")

nadal_2022_shots <- clean_shot_level(nadal_2022)
nadal_2021_shots <- clean_shot_level(nadal_2021)

## =============================================================================
## SERVES
## =============================================================================

## 2022 Title Run:
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
         x = if_else(x > 1, true = -x, false = x)) |>
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

nadal_2022_ad <- nadal_2022_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

nadal_2022_deucead <- bind_rows(nadal_2022_deuce, nadal_2022_ad)

ggplot(data = nadal_2022_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = break_point, alpha = break_point, size = break_point), show.legend = TRUE) +
  scale_alpha_manual(name = "Break Point", values = c(1, 0.4)) +
  scale_size_manual(name = "Break Point", values = c(1.3, 1)) +
  scale_colour_manual(name = "Break Point", values = c("#00FFFF", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Rafael Nadal Serves - 2022 Title Run")

## First plot with is_important coloring
ggplot(data = nadal_2022_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = atp_is_important, alpha = atp_is_important, size = atp_is_important), show.legend = TRUE) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Rafael Nadal Serves - 2022 Title Run")

## -----------------------------------------------------------------------------
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
         x = if_else(x > 1, true = -x, false = x)) |>
  # flip points to the right side of the net to prevent induced concentration
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

nadal_2021_ad <- nadal_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

nadal_2021_deucead <- bind_rows(nadal_2021_deuce, nadal_2021_ad)

ggplot(data = nadal_2021_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = break_point, alpha = break_point, size = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("#00FFFF", "black")) +
  scale_alpha_manual(name = "Break Point", values = c(1, 0.4)) +
  scale_size_manual(name = "Break Point", values = c(1.3, 1)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Rafael Nadal Serves - 2021 Semifinal Run")

ggplot(data = nadal_2021_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = atp_is_important, alpha = atp_is_important, size = atp_is_important), show.legend = TRUE) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Rafael Nadal Serves - 2021 Semifinal Run")

nadal_2021_serves |> View()

## =============================================================================
## RETURNS
## =============================================================================

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
  relocate(position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

ggplot(data = nadal_2022_returns, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(size = 1.2, show.legend = TRUE, aes(color = serve, alpha = serve)) +
  scale_alpha_manual(name = "Serve Type", values = c(0.4, 1)) +
  scale_color_manual(name = "Serve Type", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Rafael Nadal Return Locations - 2022 Title Run")

## Joining for net clearance
nadal_2022_returns_clearance <- nadal_2022_shots |>
  filter(receiverId == "Rafael Nadal") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

nadal_2022_returns_joined <- left_join(nadal_2022_returns, nadal_2022_returns_clearance,
                                          by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = nadal_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2022 Title Run")

## COLORING BY BREAK POINT
ggplot(data = nadal_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = breakPoint.x, alpha = breakPoint.x, size = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Break Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Break Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2022 Title Run")

## COLORING BY IMPORTANCE
ggplot(data = nadal_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = atp_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2022 Title Run")

## COLORING BY IS_IMPORTANT
ggplot(data = nadal_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = atp_is_important.x, alpha = atp_is_important.x, size = atp_is_important.x)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2022 Title Run")

## -----------------------------------------------------------------------------
## 2021 Semifinal Run:
nadal_2021_returns <- nadal_2021_shots |>
  filter(receiverId == "Rafael Nadal") |>
  filter(position == "bounce") |>
  group_by(point_index, player2) |>
  slice(2) |>
  mutate(y = if_else(x < 0, true = -y, false = y),
         x = if_else(x < 0, true = -x, false = x),
         serve = as_factor(serve),
         serve = fct_relevel(serve, "1","2")) |>
  relocate(position, shot_index, x, y, z)  |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

ggplot(data = nadal_2021_returns, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(size = 1.2, show.legend = TRUE, aes(color = serve, alpha = serve)) +
  scale_alpha_manual(name = "Serve Type", values = c(0.4, 1)) +
  scale_color_manual(name = "Serve Type", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Rafael Nadal Return Locations - 2021 Semifinal Run")

## Joining for net clearance
nadal_2021_returns_clearance <- nadal_2021_shots |>
  filter(receiverId == "Rafael Nadal") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

nadal_2021_returns_joined <- left_join(nadal_2021_returns, nadal_2021_returns_clearance,
                                          by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = nadal_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2021 Semifinal Run")

## COLORING BY BREAK POINT
ggplot(data = nadal_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = breakPoint.x, alpha = breakPoint.x, size = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Break Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Break Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2021 Semifinal Run")

## COLORING BY IMPORTANCE
ggplot(data = nadal_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = atp_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2021 Semifinal Run")

## COLORING BY IS_IMPORTANT
ggplot(data = nadal_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = atp_is_important.x, alpha = atp_is_important.x, size = atp_is_important.x)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Rafael Nadal Return Locations - 2021 Semifinal Run")

## =============================================================================
## SUMMARY STATS
## =============================================================================

## Histogram for net clearance
ggplot(data = nadal_2022_returns_joined, aes(net_clearance.y)) +
  geom_histogram()
ggplot(data = nadal_2021_returns_joined, aes(net_clearance.y)) +
  geom_histogram()

## Mean and SD Net Clearance on Return - 2022 Title
## Importance:
nadal_2022_returns_joined |>
  group_by(atp_is_important.y) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Break Points:
nadal_2022_returns_joined |>
  group_by(breakPoint.x) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Mean and SD Net Clearance on Return - 2021 Semifinal
## Importance:
nadal_2021_returns_joined |>
  group_by(atp_is_important.y) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Break Points:
nadal_2021_returns_joined |>
  group_by(breakPoint.x) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Proportion of Returns Made:
## Important Points:
nadal_2022_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(atp_is_important) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

nadal_2021_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(atp_is_important) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

## Break Points:
nadal_2022_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(breakPoint) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

nadal_2021_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(breakPoint) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

## NOTE: there doesn't appear to be a huge difference in Nadal's numbers
## between when he won (2022) and when he lost in the semis (2021), apart from
## his return net clearance being slightly higher in 2022

## Serve Speed:
nadal_2022_deucead |>
  group_by(atp_is_important, serve) |>
  summarise(avg_serve_speed = mean(ballSpeed),
            sd_serve_speed = sd(ballSpeed))

