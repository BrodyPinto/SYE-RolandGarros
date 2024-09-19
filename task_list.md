## Task List

### For September 26 (and maybe October 3)

1. Get match score variable (sets, games, points for each player), either from the column that's in the data or from making it by hand.

2. Make a separate tibble for net height at a grid of maybe 100 to 1000 y-values.

    * can then use this tibble to join to the trajectory data and figure out the "height above net." 
    
3. With height above net, can start to look into net clearance for serves on important points.

    
### For September 19

1. Get player hit variable correct.

2. A bit of data deep diving with things like errors that go off the court, let serves, does the data distinguish between a made and missed return? how is a return winner coded? are faulty serves counted as an additional point?


trajectory_formatted_df |> mutate(is_hit = if_else(position == " hit",
                                                  true = 1, false = 0)) |>
  relocate(is_hit) |>
  group_by(point_index) |>
  mutate(shot_index2 = cumsum(is_hit)) |>
  relocate(shot_index2)
  

### For September 12

1. Format the data to be a column for type, x, y, z, player_hit.

2. Pick a match. Get the serve bounce locations and the return bounce locations and make a graph of each for one player.


### For September 5

1. Read Paper <https://martiningram.github.io/papers/hot-heads-cool-heads.pdf> (no need to focus on details).

2. Look into `R` package book <https://r-pkgs.org/whole-game.html>. Is this something you're interested in learning? Pros include learning about software tests, function documentation, and the organization that comes with an R package. The biggest con is that time spent learning this means less time spent on the analysis itself. To help decide you can also skim through James Wolpe's SYE project, which was in an R package format (functions are in the R folder, tests are in the tests folder, etc.). <https://github.com/jameswolpe/compr>. 

3. Mess around with data on other git repo (put /data folder in gitignore so it doesn't get pushed to your SYE repo. It's fine to have that data on the courtvisionr repo because that repo is private. But, we'll eventually want to make your SYE repo public, so it shouldn't have that data on there).



