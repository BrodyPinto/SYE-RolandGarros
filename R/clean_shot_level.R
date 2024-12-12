#' Clean Shot Level Function
#'
#' This is a function that parses the trajectory data from the Court Vision data - breaking the match/matches down to the *shot* level of granularity.
#'
#' @param cleaned_data is a data frame of cleaned point-level data - from the clean_and_combine() function
#' @return returns a data frame with several rows (hit, net, peak, bounce) for each shot in the match/matches of interest
#'
#' @examples
#' nadal_2022_cleaned <- clean_and_combine(nadal_2022, player_interest = "Nadal")
#' clean_shot_level(nadal_2022_cleaned)
#'
#' @import tidyverse
#' @export

clean_shot_level <- function(cleaned_data) {
  formatted_shot_level <- cleaned_data |>
    ## trajectoryData parsing:
    mutate(trajectoryData = sub("^..", "", trajectoryData)) |>
    mutate(trajectoryData = sub("..$", "", trajectoryData)) |>
    separate_longer_delim(trajectoryData, delim = "}, {") |>
    group_by(point_index) |>
    mutate(shot_index = row_number()) |>
    separate(trajectoryData, into = c("x", "y", "z", "position"), sep = "\\,") |>
    mutate(x = parse_number(x),
           y = parse_number(y),
           z = parse_number(z),
           position = sub("^.............", "", position)) |>
    mutate(position = gsub("'", "", position)) |>
    ## player_hit variable construction:
    mutate(is_hit = if_else(position == "hit", true = 1, false = 0)) |>
    group_by(point_index) |>
    mutate(hit_count = cumsum(is_hit)) |>
    mutate(player_hit = if_else(hit_count %% 2 == 1, serverId, receiverId)) |>
    ## net_height and net_clearance variables:
    mutate(net_height = 0.00619 * (y^2) + 0.914) |>
    mutate(net_clearance = z - net_height) |>
    relocate(set, player1_game_score, player2_game_score, player1_set_score, player2_set_score, player1, player2, x, y, z, position)

  return(formatted_shot_level)
}
