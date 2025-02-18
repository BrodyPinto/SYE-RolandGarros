library(tidyverse)

## list matches available: default gives all matches
fetch_match_info <- function(year = "(.|\\s)*\\S(.|\\s)*",
                             player_last = "(.|\\s)*\\S(.|\\s)*",
                             round = "(.|\\s)*\\S(.|\\s)*") {

  file_vec <- list.files(path = "data/")
  contains_year <- file_vec |>
    str_detect(pattern = as.character(year))
  contains_player <- file_vec |>
    str_detect(pattern = str_to_title(player_last))

  ## round should be F, SF, QF, R16, R32, R64, or R128
  contains_round <- file_vec |>
    str_detect(pattern = paste0("_", round, "_"))
  matches_of_interest <- file_vec[contains_year & contains_player & contains_round]

  return(matches_of_interest)
}

fetch_match_info() ## should give all matches in data/ folder
fetch_match_info(year = "2022", player_last = "Ruud")
fetch_match_info(year = "2022", player_last = "Ruud", round = "F")
match <- fetch_match_info(year = "2022", player_last = "Ruud", round = "F")


raw_df <- read_csv(here::here(paste0("data/", match)))
raw_df

long_df <- raw_df |> mutate(point_index = row_number()) |>
  separate_longer_delim(trajectoryData, delim = "}") |>
  relocate(point_index) |>
  group_by(point_index) |>
  mutate(shot_index = row_number()) |>
  separate(trajectoryData, into = c("blank", "x", "y", "z", "position"),
                       sep = "\\,") |>
  filter(blank != "]") |>  ## a bit sloppy but I don't think these rows
## have anything now
  mutate(position = if_else(blank == "", ## move everything over one slot for the first shot....more than a bit sloppy
                            true = position,
                            false = z),
         z = if_else(blank == "",
                     true = z,
                     false = y),
         y = if_else(blank == "",
                     true = y,
                     false = x),
         x = if_else(blank == "",
                     true = x,
                     false = blank)
        ) |>
  mutate(x = parse_number(x),
         y = parse_number(y),
         z = parse_number(z)) ## a couple of parsing warnings for rows with no coordinates

long_df

serve_bounce_df <- long_df |>
  filter(str_detect(position, "bounce")) |> ## only keep rows where serve bounced
  group_by(point_index) |>
  slice(1) |> ## keep only the first bounce, which should correspond to
## the serve bounce (might drop serves that went into the net)
  ungroup() |>
  mutate(serve_in_or_out = if_else(pointEndType == "Faulty Serve",
                                   true = "Fault", false = "In"))

ruud_deuce <- serve_bounce_df |>
  filter(serverId == 36389) |> ## keep only Ruud serves
  filter(court == "DeuceCourt")
ruud_ad <- serve_bounce_df |>
  filter(serverId == 36389) |> ## keep only Ruud serves
  filter(court == "AdCourt")


ggplot(data = ruud_ad, aes(x = x, y = y)) +
  geom_point()
ggplot(data = ruud_deuce, aes(x = x, y = y)) +
  geom_point()
## need to deal with the swapping of sides issue

ruud_ad <- ruud_ad |>
  ## deal with net shots first: put them on one side
  mutate(x = if_else(abs(x) < 1,
                 true = -abs(x),
                 false = x),
         y = if_else(abs(x) < 1,
                     true = abs(y),
                     false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x))

ggplot(data = ruud_ad, aes(x = x, y = y)) +
  geom_point()  ## assuming the two dots by the net are lets...maybe?

ruud_deuce <- ruud_deuce |>
  mutate(x = if_else(abs(x) < 1,
                     true = -abs(x),
                     false = x),
         y = if_else(abs(x) < 1,
                     true = -abs(y),
                     false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x),
)

## I think the dimensions for these are in meters? so, the service box
## should be 6.4 m long by 4.11 m wide

## can wrap the code generating the court dimensions into a function
ggplot(data = ruud_ad, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE) +
  geom_point(alpha = 0.4, size = 1, aes(colour = serve_in_or_out),
             show.legend = FALSE) +
  scale_colour_manual(values = c("red", "black")) +
  ## draw the court
  annotate(geom = "segment", y = 5.02, yend = 5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 4.11, yend = 4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -5.02, yend = -5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -4.11, yend = -4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 0, yend = 0, x = -6.4, xend = 6.4, alpha = 0.5) +
  annotate(geom = "segment", x = 0, xend = 0, y = -5.02, yend = 5.02,
           linetype = 2, alpha = 0.5) +
  annotate(geom = "segment", x = -11.88, xend = -11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = 11.88, xend = 11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = -6.4, xend = -6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  annotate(geom = "segment", x = 6.4, xend = 6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  theme_void() +
  coord_fixed() +## preserve the aspect ratio of a tennis court.
  labs(title = "Ruud serving to Nadal on Ad Side")
##ggsave(filename = "ruud_to_nadal_ad.png")

## looks pretty good overall: makes sense that Ruud served
## majority to Nadal backhand
## one obvious error (either data entry
## or something in my code: Ruud def did not hit a serve
## that went past the baseline)
ggplot(data = ruud_deuce, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE) +
  geom_point(alpha = 0.4, size = 1, aes(colour = serve_in_or_out),
             show.legend = FALSE) +
  scale_colour_manual(values = c("red", "black")) +

  annotate(geom = "segment", y = 5.02, yend = 5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 4.11, yend = 4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -5.02, yend = -5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -4.11, yend = -4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 0, yend = 0, x = -6.4, xend = 6.4, alpha = 0.5) +
  annotate(geom = "segment", x = 0, xend = 0, y = -5.02, yend = 5.02,
           linetype = 2, alpha = 0.5) +
  annotate(geom = "segment", x = -11.88, xend = -11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = 11.88, xend = 11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = -6.4, xend = -6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  annotate(geom = "segment", x = 6.4, xend = 6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  theme_void() +
  coord_fixed() +## preserve the aspect ratio of a tennis court.
  labs(title = "Ruud serving to Nadal on Deuce Side")
## ggsave(filename = "ruud_to_nadal_deuce.png")




###########################################
###########################################
## Repeat everything for Ruud vs. Cilic to
## see if serve locations change
###########################################
###########################################

library(tidyverse)

## list matches available: default gives all matches
fetch_match_info <- function(year = "(.|\\s)*\\S(.|\\s)*",
                             player_last = "(.|\\s)*\\S(.|\\s)*",
                             round = "(.|\\s)*\\S(.|\\s)*") {

  file_vec <- list.files(path = "data/")
  contains_year <- file_vec |>
    str_detect(pattern = as.character(year))
  contains_player <- file_vec |>
    str_detect(pattern = str_to_title(player_last))

  ## round should be F, SF, QF, R16, R32, R64, or R128
  contains_round <- file_vec |>
    str_detect(pattern = paste0("_", round, "_"))
  matches_of_interest <- file_vec[contains_year & contains_player & contains_round]

  return(matches_of_interest)
}

fetch_match_info() ## should give all matches in data/ folder
fetch_match_info(year = "2022", player_last = "Ruud")
fetch_match_info(year = "2022", player_last = "Ruud", round = "SF")
match <- fetch_match_info(year = "2022", player_last = "Ruud", round = "SF")


raw_df <- read_csv(here::here(paste0("data/", match)))
raw_df

long_df <- raw_df |> mutate(point_index = row_number()) |>
  separate_longer_delim(trajectoryData, delim = "}") |>
  relocate(point_index) |>
  group_by(point_index) |>
  mutate(shot_index = row_number()) |>
  separate(trajectoryData, into = c("blank", "x", "y", "z", "position"),
           sep = "\\,") |>
  filter(blank != "]") |>  ## a bit sloppy but I don't think these rows
  ## have anything now
  mutate(position = if_else(blank == "", ## move everything over one slot for the first shot....more than a bit sloppy
                            true = position,
                            false = z),
         z = if_else(blank == "",
                     true = z,
                     false = y),
         y = if_else(blank == "",
                     true = y,
                     false = x),
         x = if_else(blank == "",
                     true = x,
                     false = blank)
  ) |>
  mutate(x = parse_number(x),
         y = parse_number(y),
         z = parse_number(z)) ## a couple of parsing warnings for rows with no coordinates

serve_bounce_df <- long_df |>
  filter(str_detect(position, "bounce")) |> ## only keep rows where serve bounced
  group_by(point_index) |>
  slice(1) |> ## keep only the first bounce, which should correspond to
  ## the serve bounce (might drop serves that went into the net)
  ungroup() |>
  mutate(serve_in_or_out = if_else(pointEndType == "Faulty Serve",
                                   true = "Fault", false = "In"))

ruud_deuce <- serve_bounce_df |>
  filter(serverId == 36389) |> ## keep only Ruud serves
  filter(court == "DeuceCourt")
ruud_ad <- serve_bounce_df |>
  filter(serverId == 36389) |> ## keep only Ruud serves
  filter(court == "AdCourt")

ggplot(data = ruud_ad, aes(x = x, y = y)) +
  geom_point()
ggplot(data = ruud_deuce, aes(x = x, y = y)) +
  geom_point()
## need to deal with the swapping of sides issue

ruud_ad <- ruud_ad |>
  ## deal with net shots first: put them on one side
  mutate(x = if_else(abs(x) < 1,
                     true = -abs(x),
                     false = x),
         y = if_else(abs(x) < 1,
                     true = abs(y),
                     false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1,
                     true = -x,
                     false = x)
  )

ggplot(data = ruud_ad, aes(x = x, y = y)) +
  geom_point()  ## assuming the two dots by the net are lets...maybe?

ruud_deuce <- ruud_deuce |>
  mutate(x = if_else(abs(x) < 1,
                     true = -abs(x),
                     false = x),
         y = if_else(abs(x) < 1,
                     true = -abs(y),
                     false = y)) |>
  mutate(y = if_else(x > 1, true = -y, false = y),
         x = if_else(x > 1, true = -x, false = x),
  )

## can wrap the code generating the court dimensions into a function
ggplot(data = ruud_ad, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE) +
  geom_point(alpha = 0.4, size = 1, aes(colour = serve_in_or_out),
             show.legend = FALSE) +
  scale_colour_manual(values = c("red", "black")) +
  ## draw the court
  annotate(geom = "segment", y = 5.02, yend = 5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 4.11, yend = 4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -5.02, yend = -5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -4.11, yend = -4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 0, yend = 0, x = -6.4, xend = 6.4, alpha = 0.5) +
  annotate(geom = "segment", x = 0, xend = 0, y = -5.02, yend = 5.02,
           linetype = 2, alpha = 0.5) +
  annotate(geom = "segment", x = -11.88, xend = -11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = 11.88, xend = 11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = -6.4, xend = -6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  annotate(geom = "segment", x = 6.4, xend = 6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  theme_void() +
  coord_fixed() +## preserve the aspect ratio of a tennis court.
  labs(title = "Ruud serving to Cilic on Ad Side")
##ggsave(filename = "ruud_to_cilic_ad.png")


ggplot(data = ruud_deuce, aes(x = x, y = y)) +
  geom_density_2d_filled(show.legend = FALSE) +
  geom_point(alpha = 0.4, size = 1, aes(colour = serve_in_or_out),
             show.legend = FALSE) +
  scale_colour_manual(values = c("red", "black")) +

  annotate(geom = "segment", y = 5.02, yend = 5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 4.11, yend = 4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -5.02, yend = -5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = -4.11, yend = -4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
  annotate(geom = "segment", y = 0, yend = 0, x = -6.4, xend = 6.4, alpha = 0.5) +
  annotate(geom = "segment", x = 0, xend = 0, y = -5.02, yend = 5.02,
           linetype = 2, alpha = 0.5) +
  annotate(geom = "segment", x = -11.88, xend = -11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = 11.88, xend = 11.88, y = -5.02,
           yend = 5.02, alpha = 0.5) +
  annotate(geom = "segment", x = -6.4, xend = -6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  annotate(geom = "segment", x = 6.4, xend = 6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
  theme_void() +
  coord_fixed() +## preserve the aspect ratio of a tennis court.
  labs(title = "Ruud serving to Cilic on Deuce Side")
## ggsave(filename = "ruud_to_cilic_deuce.png")
