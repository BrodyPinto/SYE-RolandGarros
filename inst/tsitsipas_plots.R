## Tsitsipas Plots

library(tidyverse)
library(viridis)

## Tsitsipas's 2020 Run to the French Open Final (missing Final round data against Nadal):
tsitsipas_2020 <- filter_matches(player = "Stefanos Tsitsipas", year_of_interest = "2020")
## Tsitsipas's 2021 Run to the Final, losing to Djokovic
tsitsipas_2021 <- filter_matches(player = "Stefanos Tsitsipas", year_of_interest = "2021")

tsitsipas_2020_shots <- clean_shot_level(tsitsipas_2020)
tsitsipas_2021_shots <- clean_shot_level(tsitsipas_2021)

## =============================================================================
## SERVES
## =============================================================================

## 2020 Semifinal Run:
tsitsipas_2020_shots |> View()

tsitsipas_2020_serves <- tsitsipas_2020_shots |>
  filter(serverId == "Stefanos Tsitsipas") |>
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

tsitsipas_2020_deuce <- tsitsipas_2020_serves |>
  filter(court == "DeuceCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  mutate(x = if_else(y < 0, true = -x, false = x),
         y = if_else(y < 0, true = -y, false = y)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

tsitsipas_2020_ad <- tsitsipas_2020_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

tsitsipas_2020_deucead <- bind_rows(tsitsipas_2020_deuce, tsitsipas_2020_ad)

ggplot(data = tsitsipas_2020_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = break_point, alpha = break_point, size = break_point), show.legend = TRUE) +
  scale_alpha_manual(name = "Break Point", values = c(1, 0.4)) +
  scale_size_manual(name = "Break Point", values = c(1.3, 1)) +
  scale_colour_manual(name = "Break Point", values = c("#00FFFF", "black")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Stefanos Tsitsipas Serves - 2020 Semifinal Run")

## First plot with is_important coloring
ggplot(data = tsitsipas_2020_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = atp_is_important, alpha = atp_is_important, size = atp_is_important), show.legend = TRUE) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Stefanos Tsitsipas Serves - 2020 Semifinal Run")

## -----------------------------------------------------------------------------
## 2021 Final:
tsitsipas_2021_shots |> View()

tsitsipas_2021_serves <- tsitsipas_2021_shots |>
  filter(serverId == "Stefanos Tsitsipas") |>
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

tsitsipas_2021_deuce <- tsitsipas_2021_serves |>
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

tsitsipas_2021_ad <- tsitsipas_2021_serves |>
  filter(court == "AdCourt") |>
  mutate(x = if_else(abs(x) < 1, true = -abs(x), false = x),
         y = if_else(abs(x) < 1, true = -abs(y), false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x)) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

tsitsipas_2021_deucead <- bind_rows(tsitsipas_2021_deuce, tsitsipas_2021_ad)

ggplot(data = tsitsipas_2021_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = break_point, alpha = break_point, size = break_point), show.legend = FALSE) +
  scale_colour_manual(name = "Break Point", values = c("#00FFFF", "black")) +
  scale_alpha_manual(name = "Break Point", values = c(1, 0.4)) +
  scale_size_manual(name = "Break Point", values = c(1.3, 1)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final, nrow = 1) +
  labs(title = "Stefanos Tsitsipas Serves - 2021 Title Run")

ggplot(data = tsitsipas_2021_deucead, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(aes(color = atp_is_important, alpha = atp_is_important, size = atp_is_important), show.legend = TRUE) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Stefanos Tsitsipas Serves - 2021 Title Run")

tsitsipas_2021_serves |> View()

## =============================================================================
## RETURNS
## =============================================================================

## 2020 Semifinal Run
tsitsipas_2020_returns <- tsitsipas_2020_shots |>
  filter(receiverId == "Stefanos Tsitsipas") |>
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

ggplot(data = tsitsipas_2020_returns, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(size = 1.2, show.legend = TRUE, aes(color = serve, alpha = serve)) +
  scale_alpha_manual(name = "Serve Type", values = c(0.4, 1)) +
  scale_color_manual(name = "Serve Type", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2020 Semifinal Run")

## Joining for net clearance
tsitsipas_2020_returns_clearance <- tsitsipas_2020_shots |>
  filter(receiverId == "Stefanos Tsitsipas") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

tsitsipas_2020_returns_joined <- left_join(tsitsipas_2020_returns, tsitsipas_2020_returns_clearance,
                                          by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = tsitsipas_2020_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2020 Semfinal Run")

## COLORING BY BREAK POINT
ggplot(data = tsitsipas_2020_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = breakPoint.x, alpha = breakPoint.x, size = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Break Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Break Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2020 Semifinal Run")

## COLORING BY IMPORTANCE
ggplot(data = tsitsipas_2020_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = atp_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2020 Semifinal Run")

## COLORING BY IS_IMPORTANT
ggplot(data = tsitsipas_2020_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = TRUE, aes(color = atp_is_important.x, alpha = atp_is_important.x, size = atp_is_important.x)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2020 Semifinal Run")

## -----------------------------------------------------------------------------
## 2021 Finalist Run:
tsitsipas_2021_returns <- tsitsipas_2021_shots |>
  filter(receiverId == "Stefanos Tsitsipas") |>
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

ggplot(data = tsitsipas_2021_returns, aes(x = rot_x, y = rot_y)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(size = 1.2, show.legend = TRUE, aes(color = serve, alpha = serve)) +
  scale_alpha_manual(name = "Serve Type", values = c(0.4, 1)) +
  scale_color_manual(name = "Serve Type", values = c("black", "#00FFFF")) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2021 Title Run")

## Joining for net clearance
tsitsipas_2021_returns_clearance <- tsitsipas_2021_shots |>
  filter(receiverId == "Stefanos Tsitsipas") |>
  filter(position == "net") |>
  group_by(point_index, player2) |>
  slice(2) |>
  filter(net_clearance > 0) |>
  relocate(net_clearance, position, shot_index, x, y, z) |>
  # Rotate 90 degrees CCW: (x, y) -> (-y, x)
  mutate(rot_x = -y, rot_y = x)

tsitsipas_2021_returns_joined <- left_join(tsitsipas_2021_returns, tsitsipas_2021_returns_clearance,
                                          by = c("match_id", "set", "game", "point", "hit_count")) |>
  relocate(net_clearance.y)

## THIS IS COOL 😎
ggplot(data = tsitsipas_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = net_clearance.y)) +
  scale_color_viridis_c(name = "Net Clearance (m)", option = "viridis") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2021 Title Run")

## COLORING BY BREAK POINT
ggplot(data = tsitsipas_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = FALSE, aes(color = breakPoint.x, alpha = breakPoint.x, size = breakPoint.x)) +
  scale_colour_manual(name = "Break Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Break Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Break Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x, nrow = 1) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2021 Title Run")

## COLORING BY IMPORTANCE
ggplot(data = tsitsipas_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(alpha = 0.8, size = 1.2, show.legend = TRUE, aes(color = atp_importance.x)) +
  scale_colour_viridis_c(name = "Importance") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2021 Title Run")

## COLORING BY IS_IMPORTANT
ggplot(data = tsitsipas_2021_returns_joined, aes(x = rot_x.x, y = rot_y.x)) +
  geom_density_2d_filled(show.legend = FALSE, bins = 9) +
  geom_point(show.legend = FALSE, aes(color = atp_is_important.x, alpha = atp_is_important.x, size = atp_is_important.x)) +
  scale_colour_manual(name = "Important Point", values = c("black", "#00FFFF")) +
  scale_alpha_manual(name = "Important Point", values = c(0.4, 1)) +
  scale_size_manual(name = "Important Point", values = c(1, 1.3)) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = "none") +
  draw_court() +
  facet_wrap(~plot_label_final.x, nrow = 1) +
  labs(title = "Stefanos Tsitsipas Return Locations - 2021 Title Run")

## =============================================================================
## SUMMARY STATS
## =============================================================================

## Histogram for net clearance
ggplot(data = tsitsipas_2020_returns_joined, aes(net_clearance.y)) +
  geom_histogram()
ggplot(data = tsitsipas_2021_returns_joined, aes(net_clearance.y)) +
  geom_histogram()

## Mean and SD Net Clearance on Return - 2020 Semifinal
## Important Points:
tsitsipas_2020_returns_joined |>
  group_by(atp_is_important.y) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Break Points:
tsitsipas_2020_returns_joined |>
  group_by(breakPoint.x) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Mean and SD Net Clearance on Return - 2021 Final
## Important Points:
tsitsipas_2021_returns_joined |>
  group_by(atp_is_important.y) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Break Points:
tsitsipas_2021_returns_joined |>
  group_by(breakPoint.x) |>
  summarise(avg_net_clearance = mean(net_clearance.y, na.rm = TRUE),
            sd_net_clearance = sd(net_clearance.y, na.rm = TRUE),
            count = n()) |>
  slice(1:2)

## Proportion of Returns Made:
## Important Points:
tsitsipas_2020_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(atp_is_important) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

return_stats = tsitsipas_2021_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(atp_is_important) |>
  summarise(returns_hit = n(),
            returns_made = sum(return_made)) |>
  mutate(proportion_returns_made = returns_made / returns_hit) |>
  rename(important_point = atp_is_important)

## To make a pretty table:
library(kableExtra)
return_stats <- tsitsipas_2021_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(atp_is_important) |>
  summarise(
    returns_hit = n(),
    returns_made = sum(return_made),
    .groups = "drop"
  ) |>
  mutate(proportion_returns_made = returns_made / returns_hit) |>
  rename(important_point = atp_is_important)

# Pretty table with kableExtra
return_stats |>
  mutate(proportion_returns_made = scales::percent(proportion_returns_made, accuracy = 0.1)) |>
  kable("html", caption = "Tsitsipas 2021 Return Stats by Point Importance") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE, font_size = 16) |>
  column_spec(1, bold = TRUE) |>
  row_spec(0, bold = TRUE, background = "#E0E0E0")

## Break Points:
tsitsipas_2020_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(breakPoint) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

## TODO take this table
tsitsipas_2021_returns |>
  mutate(return_made = if_else(abs(x) <= 11.88 & abs(x) > 0 & abs(y) <= 4.11, 1, 0)) |>
  group_by(breakPoint) |>
  summarise(count = n(),
            returns_made = sum(return_made)) |>
  mutate(prop_in_play = returns_made / count)

## Serve Speed:
tsitsipas_2020_deucead |>
  group_by(atp_is_important, serve) |>
  summarise(avg_serve_speed = mean(ballSpeed),
            sd_serve_speed = sd(ballSpeed))

