## Nadal Plots

# Nadal's 2022 Run to the French Open Title:
nadal_2022 <- fetch_all_matches(year = 2022, player = "Nadal")
## Nadal's 2021 Run to the semifinals, losing to Novak Djokovic
nadal_2021 <- fetch_all_matches(year = 2021, player = "Nadal")

View(tidy_point_level(nadal_2022))
View(tidy_shot_level(nadal_2022))

fetch_match_info(player_last = "Nadal")



