## Task List

## For November 7

1. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).

2. Parse to get player names, round, year.

3. For Nadal, looking at his play through time. 

4. Think through how to get a particular player as "Player 1" for an entire set of matches. 

5. MH: Look at player_id in data Ben used.

6. MH: Refresh on how to start an R package (especially if it's possible to do it with an existing repository).

## For October 31

1. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).

2. Add the file string as a variable in the data set and parse to get player names, round, year.

3. For Nadal, looking at his play through time. 

## For October 24

1. Create two functions that have the raw data as input and then output the cleaned data set with each row as one point and one function that takes the point cleaned data set and returns a cleaned data set with one row per shot.

2. Maybe adjust the match function to be able to pull in (and subsequently bind, with a match id) all matches for a player of interest.

3. Creating a "base plot" function that plots the tennis court background, lines, etc, taking no inputs and outputting a ggplot object.

4. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).

## For October 10

1. Fix the score variable.

2. Maybe adjust the match function to be able to pull in (and subsequently bind, with a match id) all matches for a player of interest.

3. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).


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



