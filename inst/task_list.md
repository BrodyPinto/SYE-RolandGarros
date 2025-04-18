## Task List

References:

- atp_importance from the deuce R package - by Kovalchik (google this)

- atp_players from Jeff Sackmann - just give web address

- the paper by Kovalchik

- reference Roland Garros data is from InfoSys

- flip server_game_scoer and server_set_score in federer_2021?

## April 10

1. Tweak the break point / first serve colours and fix the coord fixed dimensions, add match scores to plot titles, order plots by round.

2. write abstract. (1) Introduce the shot by shot data. (2) State what you did (wrangling, visualizing, etc.), (3) one or two interesting results.

3. Start thinking through the story that you want to tell.

4. Start outlining the poster. posterdown: <https://github.com/brentthorne/posterdown>. <https://github.com/hquin20/SYE/blob/main/poster/poster.Rmd>.

5. Numeric Summary of:
percentage of important returns made, 
avg shot/return depth for important vs non-important points (t-test?), 
avg net clearance depending for important vs non-important points,
avg shot/return safety (distance from closest line) for important vs non-important points,
counts for types of serves to each box, 
avg first serve/second serve speed depending on importance,
avg serve safety (distance from closest line) for important vs non-important points, 
etc...
 - across years? across matches within one year?

 

## April 3

1. Finish up the atp stuff (Djokovic, Nadal, Zverev).

2. Finish the wta stuff (Swiatek, Gauff, Muchova?, Kredjikova?).

3. Start thinking through the story that you want to tell.

4. Numeric Summary of:
percentage of important returns made, 
avg shot/return depth for important vs non-important points (t-test?), 
avg net clearance depending for important vs non-important points,
avg shot/return safety (distance from closest line) for important vs non-important points,
counts for types of serves to each box, 
avg first serve/second serve speed depending on importance,
avg serve safety (distance from closest line) for important vs non-important points, 
etc...
 - across years? across matches within one year?
 
 5. order plots by round, add match scores to plot titles


## March 27

1. Look into net clearance joining issue.

2. Issue with the smoothing (either do two separate serve plots or do the same plot). Maybe do something similar to the return. Look at first serve returns vs. second serve returns.

3. return on break vs. non-break points or important points.

4. Make a few plots for a couple of the top women (maybe add importance to wta).

## March 13

1. Look into net clearance joining issue.

2. Issue with the smoothing (either do two separate serve plots or do the same plot). Maybe do something similar to the return. Look at first serve returns vs. second serve returns.

3. Look at Djokovic returns.

4. return on break vs. non-break points or important points.

5. Modify importance for women?

## March 6

1. Keep working on visualizations of serve (and return if you want and third shot if you want).

2. Continue model exploration of other possible predictors (net clearance, for example).

## February 27

1. Keep working on visualizations of serve (and return if you want). Maybe look into shot placement of other shots during a rally.

2. Continue model exploration of other possible predictors (net clearance, for example).

3. Fix up the last of the importance joining issues.

## February 20

1. Document data.

2. Make some initial plots of Nadal's serve location (facet by match) and colour by a metric for "pressure." 

3. Joining a point importance data set to the big data set. <https://github.com/skoval/deuce>.

4. Fit some exploratory logistic regression models.

5. Create some exploratory plots for some other player of interest.

6. Compare WTA and ATP tours and do some summary stats on things like serve location, net clearance, percent returns in play/cross-court.



## February 6

1. Make a new function that grabs matches from a player of interest (and makes that player player1).

2. Document data.

3. Make some initial plots of Nadal's serve location (facet by match) and colour by a metric for "pressure." 

4. Joining a point importance data set to the big data set. <https://github.com/skoval/deuce>.

## January 30

1. Finish data cleaning so that data is in one .rda file.

2. Make some initial plots of Nadal's serve location (facet by match) and colour by a metric for "pressure." 

3. Joining a point importance data set to the big data set. <https://github.com/skoval/deuce>.


## December 19

1. Replace player ID's with their names (three joins).

2. Modify clean_shot_level to take in the clean_and_combine_point output.

## December 12

1. Fix the 13 matches (directly in the file names).

2. Replace player ID's with their names (three joins).

3. Modify clean_shot_level to take in the clean_and_combine_point output.

4. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).

5. For Nadal, looking at his play through time. 

## December 5

1. Fix score issues (second serves and tiebreak scores).

2. Fix the 13 matches (directly in the file names).

3. Replace player ID's with their names.

4. Think through how to get a particular player as "Player 1" for an entire set of matches. 

5. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).

6. For Nadal, looking at his play through time. 

## November 21

1. Think through how to get a particular player as "Player 1" for an entire set of matches. 

2. Is there an issue with the score and who is player1 vs. who is player2?

3. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).

4. For Nadal, looking at his play through time. 

## November 14

1. Document all functions in package.

2. Get necessary imports.

3. Think through how to get a particular player as "Player 1" for an entire set of matches. 

4. MH: think more about .rda files (one vs. many).

5. For a player of interest, start to make plots of perhaps their serve location and/or return location, colouring by whether or the point was "important" (was a break point, was a deuce point, was a tiebreak point, or some other "easy" metric of importance).

6. For Nadal, looking at his play through time. 

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



