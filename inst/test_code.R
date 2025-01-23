## Test Code

library(tidyverse)

## Nadal's 2022 title run
nadal_2022 <- fetch_all_matches(player = "Nadal", year = 2022)
nadal_2022_cleaned <- clean_and_combine(nadal_2022, player_interest = "Nadal")
nadal_2022_cleaned |> View()


## Nadal 2019-2022
nadal_all <- fetch_all_matches(player = "Nadal")
nadal_all_cleaned <- clean_and_combine(nadal_all, player_interest = "Nadal")
nadal_all_cleaned |>
  mutate(round = factor(round, levels = c("R64", "R32", "R16", "QF", "SF", "F"))) |>
  arrange(year, round) |>
  View()

table(nadal_all_cleaned$round)
