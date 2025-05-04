## Zverev Plots

library(tidyverse)
library(viridis)

## Zverev's 2022 Run to the French Open Semifinal:
zverev_2022 <- filter_matches(player = "Alexander Zverev", year_of_interest = "2022")
## Zverev's 2021 Run to the semifinals
zverev_2021 <- filter_matches(player = "Alexander Zverev", year_of_interest = "2021")

zverev_2022_shots <- clean_shot_level(zverev_2022)
zverev_2021_shots <- clean_shot_level(zverev_2021)

## =============================================================================
## SERVES
## =============================================================================

## 2022 Semifinal Run:
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
         x = if_else(x > 1, true = -x, false = x)) |>
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

zverev_2022_ad <- zverev_2022_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

zverev_2022_deucead <- bind_rows(zverev_2022_deuce, zverev_2022_ad)

ggplot(data = zverev_2022_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = break_point, alpha = break_point, size = break_point), show.legend = TRUE) +
  scale_alpha_manual(name = "Break Point", values = c(1, 0.4)) +
  scale_size_manual(name = "Break Point", values = c(1.3, 1)) +
  scale_colour_manual(name = "Break Point", values = c("#00FFFF", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Alexander Zverev Serves - 2022 Semifinal Run")

## First plot with is_important coloring
ggplot(data = zverev_2022_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = atp_is_important, alpha = atp_is_important, size = atp_is_important), show.legend = TRUE) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Alexander Zverev Serves - 2022 Semifinal Run")

## -----------------------------------------------------------------------------
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
         x = if_else(x > 1, true = -x, false = x)) |>
  # flip points to the right side of the net to prevent induced concentration
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

zverev_2021_ad <- zverev_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

zverev_2021_deucead <- bind_rows(zverev_2021_deuce, zverev_2021_ad)

ggplot(data = zverev_2021_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = break_point, alpha = break_point, size = break_point), show.legend = TRUE) +
  scale_colour_manual(name = "Break Point", values = c("#00FFFF", "black")) +
  scale_alpha_manual(name = "Break Point", values = c(1, 0.4)) +
  scale_size_manual(name = "Break Point", values = c(1.3, 1)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Alexander Zverev Serves - 2021 Semifinal Run")

ggplot(data = zverev_2021_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = atp_is_important, alpha = atp_is_important, size = atp_is_important), show.legend = TRUE) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Alexander Zverev Serves - 2021 Semifinal Run")

zverev_2021_serves |> View()

## =============================================================================
## RETURNS
## =============================================================================

## 2022 Semifinal Run
zverev_2022_returns <- zverev_2022_shots |>
  filter(receiverId == "Alexander Zverev") |>
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

ggplot(data = zverev_2022_returns, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(size = 1.2, show.legend = TRUE, aes(color = serve, alpha = serve)) +
  scale_alpha_manual(name = "Serve Type", values = c(0.4, 1)) +
  scale_color_manual(name = "Serve Type", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Alexander Zverev Return Locations - 2022 Semifinal Run")

## Joining for net clearance
zverev_2022_returns_clearance <- zverev_2022_shots |>
  filter(receiverId == "Alexander Zverev") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

zverev_2022_returns_joined <- left_join(zverev_2022_returns, zverev_2022_returns_clearance,
                                        by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = zverev_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2022 Semifinal Run")

## COLORING BY BREAK POINT
ggplot(data = zverev_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = breakPoint.x, alpha = breakPoint.x, size = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Break Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Break Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2022 Semifinal Run")

## COLORING BY IMPORTANCE
ggplot(data = zverev_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = atp_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2022 Semifinal Run")

## COLORING BY IS_IMPORTANT
ggplot(data = zverev_2022_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = atp_is_important.x, alpha = atp_is_important.x, size = atp_is_important.x)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2022 Semifinal Run")

## -----------------------------------------------------------------------------
## 2021 Semifinal Run:
zverev_2021_returns <- zverev_2021_shots |>
  filter(receiverId == "Alexander Zverev") |>
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

ggplot(data = zverev_2021_returns, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(size = 1.2, show.legend = TRUE, aes(color = serve, alpha = serve)) +
  scale_alpha_manual(name = "Serve Type", values = c(0.4, 1)) +
  scale_color_manual(name = "Serve Type", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Alexander Zverev Return Locations - 2021 Semifinal Run")

## Joining for net clearance
zverev_2021_returns_clearance <- zverev_2021_shots |>
  filter(receiverId == "Alexander Zverev") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

zverev_2021_returns_joined <- left_join(zverev_2021_returns, zverev_2021_returns_clearance,
                                        by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = zverev_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2021 Semifinal Run")

## COLORING BY BREAK POINT
ggplot(data = zverev_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = breakPoint.x, alpha = breakPoint.x, size = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Break Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Break Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2021 Semifinal Run")

## COLORING BY IMPORTANCE
ggplot(data = zverev_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = atp_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2021 Semifinal Run")

## COLORING BY IS_IMPORTANT
ggplot(data = zverev_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = atp_is_important.x, alpha = atp_is_important.x, size = atp_is_important.x)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Alexander Zverev Return Locations - 2021 Semifinal Run")

## =============================================================================
## SUMMARY STATS
## =============================================================================

## Histogram for net clearance
ggplot(data = zverev_2022_returns_joined, aes(net_clearance.y)) +
  geom_histogram()
ggplot(data = zverev_2021_returns_joined, aes(net_clearance.y)) +
  geom_histogram()

## Mean and SD Net Clearance on Return - 2022 Semifinal
## Importance:
zverev_2022_returns_joined |>
  group_by(atp_is_important.y) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Break Points:
zverev_2022_returns_joined |>
  group_by(breakPoint.x) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Mean and SD Net Clearance on Return - 2021 Semifinal
## Importance:
zverev_2021_returns_joined |>
  group_by(atp_is_important.y) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Break Points:
zverev_2021_returns_joined |>
  group_by(breakPoint.x) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Proportion of Returns Made:
## Important Points:
## 2022:
zverev_2022_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(atp_is_important) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

## 2021
zverev_2021_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(atp_is_important) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

## Break Points:
zverev_2022_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(breakPoint) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

zverev_2021_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(breakPoint) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

## Serve Speed:
zverev_2021_deucead |>
  filter(serve == 1) |>
  group_by(breakPoint) |>
  summarise(avg_serve_speed = mean(ballSpeed),
            sd_serve_speed = sd(ballSpeed))

