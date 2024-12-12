clean_and_combine_point <- function(list_of_matches, player_interest = "(.|\\s)*\\S(.|\\s)*") {
  ## create an empty list to store the cleaned matches
  list_of_cleaned_matches <- list()

  for (match_df in list_of_matches) {
    ## clean the match_df to the point level
    match_clean <- clean_point_level(match_df, player_of_interest = player_interest)

    ## add match_clean to a list of cleaned matches
    list_of_cleaned_matches[[length(list_of_cleaned_matches) + 1]] <- match_clean
  }

  ## bind rows of all cleaned matches together
  all_cleaned_matches <- bind_rows(list_of_cleaned_matches)

  return(all_cleaned_matches) ## return the combined data frame
}

clean_and_combine_point(nadal_2022, player_interest = "Nadal") |> View()
clean_and_combine_point(test_match_steph_final) |> View()
clean_and_combine_point(all_matches_2022) |> View()
