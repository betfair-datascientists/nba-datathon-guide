NBA Datathon Model Tutorial
================
Kaushik Lakshman
26 February 2020

## 0\. Introduction

  - The Betfair Data Science Team has created a guide to building a
    model for the Betfair NBA Datathon to be held in March 2020.

  - Find more information about the datathon here -
    <https://www.betfair.com.au/hub/nba-datathon>

  - The tutorial touches on the different steps required to go from data
    to model to finally entering a submission. The data has already been
    provided to participants but is also available in this Github
    repository.

  - All code examples are built to run in `R` and the only prerequisites
    are that you have `R` installed, along with the packages/libraries
    listed in section 0.

  - Familiarity with R will make this tutorial easier to follow.
    Alternatively, if you are a Python user, these are conceptually
    similar ways of implementing in Python using packages like `pandas`,
    `scikitlearn` etc.

## 1\. Set up the environment

  - Below is the code to load all the packages and files to set up the
    environment.

  - For the purposes of this tutorial we are only using team level data,
    but feel free to use player level data, or the odds data or any
    external data that you think might be useful as well.

<!-- end list -->

``` r
## Tidyverse for data manipulation packages like dplyr, purrr etc.
library(tidyverse)
## Here package for resolving relative paths so this works on any computer that clones this repo
library(here)
## Janitor package for cleaning names of columns
library(janitor)
## ELO package for defining and tuning an ELO model
library(elo)
## RcppRoll for calculating rolling mean in the feature engineering step
library(RcppRoll)
## Recipes for preprocessing 
library(recipes)
## Parsnip for Model framework
library(parsnip)
## Rsample for splitting data
library(rsample)
## Tune for hyperparameter tuning
library(tune)
## Dials for setting up hyperparameter grids
library(dials)
## Yardstick for optimising model training on RMSE
library(yardstick)

## Loading all team logs files at once
team_logs <- map_dfr(.x = list.files(
  here::here("data"),
  pattern = "team_logs_",
  full.names = TRUE
),
.f = read_csv) %>%
  janitor::clean_names() %>%
  mutate(name_team = ifelse(name_team == "LA Clippers", "Los Angeles Clippers", name_team)) ## NBA API inconsistent with Clippers name

team_logs
```

    ## # A tibble: 13,882 x 47
    ##    year_season slug_season slug_league type_season date_game  id_game
    ##          <dbl> <chr>       <chr>       <chr>       <date>       <dbl>
    ##  1        2015 2014-15     NBA         Regular Se~ 2014-10-28  2.14e7
    ##  2        2015 2014-15     NBA         Regular Se~ 2014-10-28  2.14e7
    ##  3        2015 2014-15     NBA         Regular Se~ 2014-10-28  2.14e7
    ##  4        2015 2014-15     NBA         Regular Se~ 2014-10-28  2.14e7
    ##  5        2015 2014-15     NBA         Regular Se~ 2014-10-28  2.14e7
    ##  6        2015 2014-15     NBA         Regular Se~ 2014-10-28  2.14e7
    ##  7        2015 2014-15     NBA         Regular Se~ 2014-10-29  2.14e7
    ##  8        2015 2014-15     NBA         Regular Se~ 2014-10-29  2.14e7
    ##  9        2015 2014-15     NBA         Regular Se~ 2014-10-29  2.14e7
    ## 10        2015 2014-15     NBA         Regular Se~ 2014-10-29  2.14e7
    ## # ... with 13,872 more rows, and 41 more variables:
    ## #   number_game_team_season <dbl>, name_team <chr>, id_team <dbl>,
    ## #   is_b2b <lgl>, is_b2b_first <lgl>, is_b2b_second <lgl>, location_game <chr>,
    ## #   slug_matchup <chr>, slug_team <chr>, count_days_rest_team <dbl>,
    ## #   count_days_next_game_team <dbl>, slug_opponent <chr>,
    ## #   slug_team_winner <chr>, slug_team_loser <chr>, outcome_game <chr>,
    ## #   is_win <lgl>, fgm_team <dbl>, fga_team <dbl>, pct_fg_team <dbl>,
    ## #   fg3m_team <dbl>, fg3a_team <dbl>, pct_fg3team <dbl>, pct_ft_team <dbl>,
    ## #   has_video <lgl>, fg2m_team <dbl>, fg2a_team <dbl>, pct_fg2team <dbl>,
    ## #   minutes_team <dbl>, ftm_team <dbl>, fta_team <dbl>, oreb_team <dbl>,
    ## #   dreb_team <dbl>, treb_team <dbl>, ast_team <dbl>, stl_team <dbl>,
    ## #   blk_team <dbl>, tov_team <dbl>, pf_team <dbl>, pts_team <dbl>,
    ## #   plusminus_team <dbl>, url_team_season_logo <chr>

## 2\. Exploring the Data

``` r
glimpse(team_logs)
```

    ## Observations: 13,882
    ## Variables: 47
    ## $ year_season               <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 2015,...
    ## $ slug_season               <chr> "2014-15", "2014-15", "2014-15", "2014-15...
    ## $ slug_league               <chr> "NBA", "NBA", "NBA", "NBA", "NBA", "NBA",...
    ## $ type_season               <chr> "Regular Season", "Regular Season", "Regu...
    ## $ date_game                 <date> 2014-10-28, 2014-10-28, 2014-10-28, 2014...
    ## $ id_game                   <dbl> 21400001, 21400001, 21400003, 21400003, 2...
    ## $ number_game_team_season   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ name_team                 <chr> "New Orleans Pelicans", "Orlando Magic", ...
    ## $ id_team                   <dbl> 1610612740, 1610612753, 1610612747, 16106...
    ## $ is_b2b                    <lgl> FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, F...
    ## $ is_b2b_first              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,...
    ## $ is_b2b_second             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,...
    ## $ location_game             <chr> "H", "A", "H", "A", "A", "H", "H", "A", "...
    ## $ slug_matchup              <chr> "NOP vs. ORL", "ORL @ NOP", "LAL vs. HOU"...
    ## $ slug_team                 <chr> "NOP", "ORL", "LAL", "HOU", "DAL", "SAS",...
    ## $ count_days_rest_team      <dbl> 120, 120, 120, 120, 120, 120, 120, 120, 1...
    ## $ count_days_next_game_team <dbl> 3, 1, 0, 0, 1, 2, 1, 0, 2, 2, 2, 1, 2, 0,...
    ## $ slug_opponent             <chr> "ORL", "NOP", "HOU", "LAL", "SAS", "DAL",...
    ## $ slug_team_winner          <chr> "NOP", "NOP", "HOU", "HOU", "SAS", "SAS",...
    ## $ slug_team_loser           <chr> "ORL", "ORL", "LAL", "LAL", "DAL", "DAL",...
    ## $ outcome_game              <chr> "W", "L", "L", "W", "L", "W", "W", "L", "...
    ## $ is_win                    <lgl> TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TR...
    ## $ fgm_team                  <dbl> 41, 32, 28, 31, 38, 37, 43, 39, 40, 37, 4...
    ## $ fga_team                  <dbl> 101, 84, 79, 73, 78, 70, 82, 86, 80, 90, ...
    ## $ pct_fg_team               <dbl> 0.406, 0.381, 0.354, 0.425, 0.487, 0.529,...
    ## $ fg3m_team                 <dbl> 4, 4, 3, 12, 8, 14, 4, 6, 13, 8, 6, 7, 6,...
    ## $ fg3a_team                 <dbl> 17, 11, 10, 29, 21, 28, 15, 12, 22, 26, 2...
    ## $ pct_fg3team               <dbl> 0.2352941, 0.3636364, 0.3000000, 0.413793...
    ## $ pct_ft_team               <dbl> 0.484, 0.762, 0.795, 0.680, 0.842, 0.813,...
    ## $ has_video                 <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,...
    ## $ fg2m_team                 <dbl> 37, 28, 25, 19, 30, 23, 39, 33, 27, 29, 3...
    ## $ fg2a_team                 <dbl> 84, 73, 69, 44, 57, 42, 67, 74, 58, 64, 8...
    ## $ pct_fg2team               <dbl> 0.4404762, 0.3835616, 0.3623188, 0.431818...
    ## $ minutes_team              <dbl> 240, 240, 240, 240, 240, 240, 240, 240, 2...
    ## $ ftm_team                  <dbl> 15, 16, 31, 34, 16, 13, 15, 17, 9, 27, 20...
    ## $ fta_team                  <dbl> 31, 21, 39, 50, 19, 16, 20, 23, 17, 33, 2...
    ## $ oreb_team                 <dbl> 26, 16, 11, 14, 9, 9, 10, 19, 10, 16, 15,...
    ## $ dreb_team                 <dbl> 36, 40, 25, 33, 24, 29, 23, 28, 32, 32, 3...
    ## $ treb_team                 <dbl> 62, 56, 36, 47, 33, 38, 33, 47, 42, 48, 5...
    ## $ ast_team                  <dbl> 20, 17, 16, 22, 17, 23, 21, 20, 26, 26, 2...
    ## $ stl_team                  <dbl> 10, 5, 7, 7, 9, 5, 10, 8, 6, 13, 8, 9, 7,...
    ## $ blk_team                  <dbl> 17, 9, 3, 3, 3, 3, 5, 4, 8, 9, 9, 10, 4, ...
    ## $ tov_team                  <dbl> 9, 18, 13, 14, 10, 21, 17, 23, 19, 10, 13...
    ## $ pf_team                   <dbl> 17, 25, 32, 30, 20, 20, 21, 21, 24, 22, 2...
    ## $ pts_team                  <dbl> 101, 84, 90, 108, 100, 101, 105, 101, 102...
    ## $ plusminus_team            <dbl> 17, -17, -18, 18, -1, 1, 4, -4, -7, 7, 2,...
    ## $ url_team_season_logo      <chr> "https://stats.nba.com/media/img/teams/lo...

  - There are lots of cool stats in here like box score stats - points,
    rebounds, steals, blocks, shooting percentages etc

  - Apart from that there are some other very useful fields which
    contains information about whether the game is a back to back, how
    many days of rest a team has had etc. NBA fans know this kind of
    stuff is very important as a predictor for matches. Teams have
    ‘schedule losses’ even if they are better on paper than their
    opponents.

  - The other main thing to notice is that data from each match is
    spread out on two rows. If the Celtics played a game against the
    Rockets, there is one row for the Celtics and one for the Rockets.
    This has to be kept in mind while trying to build the model matrix
    for prediction because we have to bring them into one row where we
    predict the margin for the game.

## 3\. ELO Model

### 3.1. What is ELO?

  - ELO is a very simplistic model for ranking and comparing relative
    strengths of two players or teams. Originally developed and used for
    Chess, it has since been used in other sports as well.

  - The fundamental principle of ELO is that you earn a certain number
    of points for defeating an opponent. The number is higher if you
    defeat a higher ranked opponent. Conversely you also lose more
    points if you lose to a worse opponent. The ratings exchanged
    between opponents in a match can also account for factors like
    importance of a game - for instance when a playoff games counts for
    more than a regular season game etc.

  - Read more about the ELO system here -
    <https://en.wikipedia.org/wiki/Elo_rating_system>

  - ELO ratings can be used as a proxy for strength of teams. What we
    will do below, is to build a fairly simple ELO model using the `elo`
    package and then use the ELO ratings as a feature in our final
    predictive model

<!-- end list -->

``` r
## Converting data frame to granularity level of a game
game_level <- team_logs %>%
  filter(location_game == "H") %>%
  select(year_season, id_game, date_game, name_team, pts_team) %>%
  rename(home_team = name_team,
         home_points = pts_team) %>%
  inner_join(team_logs %>%
               filter(location_game == "A") %>%
               select(year_season, id_game, date_game, name_team, pts_team) %>%
               rename(away_team = name_team,
                      away_points = pts_team),
             by = c("year_season", "id_game", "date_game")) %>%
  mutate(home_team_win = ifelse(home_points > away_points, TRUE, FALSE),
         home_team_margin = home_points - away_points) %>%
  select(-ends_with("_points"))

game_level
```

    ## # A tibble: 6,941 x 7
    ##    year_season id_game date_game  home_team away_team home_team_win
    ##          <dbl>   <dbl> <date>     <chr>     <chr>     <lgl>        
    ##  1        2015  2.14e7 2014-10-28 New Orle~ Orlando ~ TRUE         
    ##  2        2015  2.14e7 2014-10-28 Los Ange~ Houston ~ FALSE        
    ##  3        2015  2.14e7 2014-10-28 San Anto~ Dallas M~ TRUE         
    ##  4        2015  2.14e7 2014-10-29 Memphis ~ Minnesot~ TRUE         
    ##  5        2015  2.14e7 2014-10-29 Toronto ~ Atlanta ~ TRUE         
    ##  6        2015  2.14e7 2014-10-29 Charlott~ Milwauke~ TRUE         
    ##  7        2015  2.14e7 2014-10-29 Denver N~ Detroit ~ TRUE         
    ##  8        2015  2.14e7 2014-10-29 Sacramen~ Golden S~ FALSE        
    ##  9        2015  2.14e7 2014-10-29 Utah Jazz Houston ~ FALSE        
    ## 10        2015  2.14e7 2014-10-29 Indiana ~ Philadel~ TRUE         
    ## # ... with 6,931 more rows, and 1 more variable: home_team_margin <dbl>

### 3.2 Tuning an ELO model

  - Elo model gives you a lot of parameters to tweak
    
      - K factor: A multiplier by which you can update ELO ratings after
        a result
    
      - Home advantage: Additional points to give to a home team
    
      - Regress: Regress elos back to a fixed value after a number of
        matches, like the end of a season

  - In this below example we will test a bunch of parameters to see
    which one yields the best accuracy. This is a very simplistic way of
    doing it, and can be made complex for a more complete solution using
    a variety of
approaches.

<!-- end list -->

``` r
## Creating a hyper parameter set for all combinations of a bunch of k factor, home advantage and regress factor options
elo_hyperparameters <- crossing(
  k_factor = seq(10, 40, 5),
  home_advantage = seq(10, 50, 5),
  regress_factor = seq(0.1, 0.5, 0.1)
)

## Lets have a look at what our parameter set looks like
elo_hyperparameters
```

    ## # A tibble: 315 x 3
    ##    k_factor home_advantage regress_factor
    ##       <dbl>          <dbl>          <dbl>
    ##  1       10             10            0.1
    ##  2       10             10            0.2
    ##  3       10             10            0.3
    ##  4       10             10            0.4
    ##  5       10             10            0.5
    ##  6       10             15            0.1
    ##  7       10             15            0.2
    ##  8       10             15            0.3
    ##  9       10             15            0.4
    ## 10       10             15            0.5
    ## # ... with 305 more rows

``` r
## Bringing the params back to vectors from the elo_hyperparameters data frame
## to feed into the 'loop' below
k_factor <- elo_hyperparameters %>% pull(k_factor)
home_advantage <- elo_hyperparameters %>% pull(home_advantage)
regress_factor <- elo_hyperparameters %>% pull(regress_factor)

## Running an elo model for all possible combinations and
## returning accuracy for all of them
elo_tuning_results <-
  pmap_dfr(
    .l = list(k_factor, home_advantage, regress_factor),
    .f = function(k_factor, home_advantage, regress_factor) {
      
      ## Creating elo model for an individual instance of the parameter set
      elo_model <- elo.run(
        formula = home_team_win ~ adjust(home_team, home_advantage) + away_team + regress(year_season, 1500, regress_factor),
        data = game_level,
        k = k_factor
      )
      
      ## Getting elo predictions based on the above model for every game
      ## and checking against the result whether the prediction was right
      elo_predictions <- game_level %>%
        bind_cols(
          elo_model %>%
            pluck("elos") %>%
            as_tibble() %>%
            select(V3) %>%
            rename(elo_win_probs = V3)
        ) %>%
        mutate(elo_predicted_winner = ifelse(elo_win_probs > 0.5, TRUE, FALSE)) %>%
        mutate(prediction_accuracy = ifelse(elo_predicted_winner == home_team_win, TRUE, FALSE))
      
      ## Generating an accuracy number for the given individual instance of the parameter set
      accuracy <- elo_predictions %>%
        group_by(prediction_accuracy) %>%
        summarise(total = n()) %>%
        mutate(percentage_accuracy = total / sum(total)) %>%
        filter(prediction_accuracy == TRUE) %>%
        select(percentage_accuracy) %>%
        mutate(
          k_factor = k_factor,
          home_advantage = home_advantage,
          regress_factor = regress_factor
        ) %>%
        select(k_factor,
               home_advantage,
               regress_factor,
               percentage_accuracy)
    }
  )

## Displaying accuracy of the hyperparameter set in descending order
elo_tuning_results %>%
  arrange(desc(percentage_accuracy))
```

    ## # A tibble: 315 x 4
    ##    k_factor home_advantage regress_factor percentage_accuracy
    ##       <dbl>          <dbl>          <dbl>               <dbl>
    ##  1       20             50            0.4               0.666
    ##  2       20             45            0.5               0.665
    ##  3       20             50            0.5               0.665
    ##  4       25             50            0.5               0.664
    ##  5       20             50            0.3               0.664
    ##  6       15             35            0.5               0.664
    ##  7       25             50            0.4               0.663
    ##  8       20             50            0.2               0.663
    ##  9       15             35            0.4               0.663
    ## 10       15             45            0.5               0.663
    ## # ... with 305 more rows

### 3.3. Picking the best ELO Model

  - Looks like the best ELO model in terms of prediction was one with
    `k_factor = 20`, `home_advantage = 50` and `regress_factor = 0.4`

  - If you set the baseline as home team wins every game (58%), this ELO
    model performs better at 66%, which is a good outcome

  - The method we have used is obviously a very simple way of tuning an
    ELO model, for demonstrative purposes.

  - If you want to be more thorough with ELO modelling below are some
    options to try
    
      - Creating a seperate training set to tune and a testing set for
        evaluation
    
      - Adding a factor for margin of victory for updating ELO scores
    
      - Modelling margin of victory instead of wins
    
      - Optimising on other metrics because Accuracy isn’t everything.
        Perhaps you could compare to market odds (data provided) and if
        your ELO probabilities have a better `logloss` than market
        probabilities then you’re on the right
track\!

### 3.4. Building the final ELO Model

``` r
## Running a final ELO model with the best parameters from the above process
final_elo_model <- elo.run(
  formula = home_team_win ~ adjust(home_team, 50) + away_team + regress(year_season, 1500, 0.4),
  data = game_level,
  k = 20
)

## Storing ELOs calculated after games for later use
calculated_elos <- game_level %>%
  bind_cols(
    final_elo_model %>%
      pluck("elos") %>%
      as_tibble() %>%
      select(V6, V7) %>%
      rename(home_team_elo = V6,
             away_team_elo = V7)
  )

calculated_elos_expanded <- calculated_elos %>%
  select(year_season, id_game, home_team, home_team_elo) %>%
  rename(name_team = home_team,
         elo = home_team_elo) %>%
  bind_rows(calculated_elos %>%
  select(year_season, id_game, away_team, away_team_elo) %>%
  rename(name_team = away_team,
         elo = away_team_elo)) %>%
  arrange(year_season, id_game)
```

  - We now have a data frame with ELO ratings calculated after the
    matches, which we will then use in our modelling stage

  - The important thing to keep in mind is these are ELO ratings from
    after the match has taken place. While trying to use these as
    features we must ensure that we take the most recently updated ELO
    ratings before the match we are trying to predict.

  - Otherwise we are suspect to something called feature leakage - where
    we have information about the thing we are trying to predict in the
    features. And models are amazing at picking this up. When it comes
    to actually predicting unplayed matches the model will start failing
    miserably.

  - This is the same concept we should keep in mind in the next section
    of feature engineering

## 4\. Feature Engineering

  - Feature engineering is probably the most interesting and vital part
    of the model building process

  - It is the art (or science) of converting data into features that
    your machine learning algorithm can find patterns in, which in then
    uses to predict whatever you are trying to predict.

  - You can go as in depth as you want. The obvious tradeoff is - the
    more complex you go, the more complex patterns you can find, but
    also the more overfit and less generalisable your model is for use
    against data it hasn’t seen before.

  - In our case, we already have an ELO feature for both teams that
    capture relative strengths of teams. What else could influence
    games?
    
      - Recent form perhaps? We could calculate average statistics for a
        certain number of games prior to the game we are trying to
        predict. There’s no single correct approach, but for simplicity
        sake, lets calculate the stats for the 5 most recent games.
    
      - Schedule related factors - In the NBA teams playing back to back
        games or their 3rd game in 4 nights etc have a notably poorer
        performance level than a regular game. In fact the data provided
        already has a few fields that have this information.

### 4.1. Rolling stats

``` r
## Rolling mean for certain important box score stats
## such as shooting percentages, rebounds, assists, steals, 
## blocks, turnover, Margin
rolling_mean_features <- team_logs %>%
  group_by(year_season, name_team) %>%
  arrange(id_game) %>%
  mutate_at(
    vars(
      pct_fg_team,
      pct_fg3team,
      pct_ft_team,
      treb_team,
      ast_team,
      stl_team,
      blk_team,
      tov_team,
      plusminus_team
    ), ## Columns for which we want a rolling mean
    .funs = ~ roll_mean(., 5, align = "right", fill = NA) ## Rolling mean for last 5 games
  ) %>%
  select(
    year_season,
    name_team,
    id_game,
    pct_fg_team,
    pct_fg3team,
    pct_ft_team,
    treb_team,
    ast_team,
    stl_team,
    blk_team,
    tov_team,
    plusminus_team
  ) %>%
  filter(!is.na(pct_fg_team))

rolling_mean_features
```

    ## # A tibble: 13,162 x 12
    ## # Groups:   year_season, name_team [180]
    ##    year_season name_team id_game pct_fg_team pct_fg3team pct_ft_team treb_team
    ##          <dbl> <chr>       <dbl>       <dbl>       <dbl>       <dbl>     <dbl>
    ##  1        2015 Houston ~  2.14e7       0.461       0.443       0.734      43.2
    ##  2        2015 Oklahoma~  2.14e7       0.443       0.269       0.704      43.2
    ##  3        2015 Los Ange~  2.14e7       0.432       0.324       0.760      40  
    ##  4        2015 Miami He~  2.14e7       0.457       0.361       0.747      38.4
    ##  5        2015 Charlott~  2.14e7       0.426       0.307       0.741      43  
    ##  6        2015 Philadel~  2.14e7       0.430       0.325       0.638      38  
    ##  7        2015 Orlando ~  2.14e7       0.448       0.338       0.683      45.6
    ##  8        2015 Toronto ~  2.14e7       0.431       0.289       0.780      37.8
    ##  9        2015 New York~  2.14e7       0.441       0.432       0.778      38.8
    ## 10        2015 Chicago ~  2.14e7       0.475       0.376       0.777      42.6
    ## # ... with 13,152 more rows, and 5 more variables: ast_team <dbl>,
    ## #   stl_team <dbl>, blk_team <dbl>, tov_team <dbl>, plusminus_team <dbl>

  - Very cool, we now have moving averages for quite a few stats.

  - Once again, the key thing to keep in mind is these are rolling
    averages including the game that has already been played. Therefore
    the first row in the above data frame- we can’t use the stats to
    predict `id_game = 21400050` but we should use these numbers the
    next time the Rockets play.

### 4.2. Schedule Information

  - There are a few different fields within the logs file that have
    schedule related information

  - Three fields talk about back to backs, and while that is useful, the
    field which is potentially more useful is `count_days_rest_team`

  - Similarly `count_days_next_game_team` could also be useful to see if
    the team has another game on their mind and if they preserve
    themselves for it. This could also incorporate the back to back
    situation so there probably isn’t a need to incorporate that
    information twice.

  - The big key thing here that is different to other features is that
    they are available by default in the data set which we can use to
    train the model, but we will have to calculate them for making
    predictions on the games yet to be played in the last phase. Lets
    get to that at a later stage.

### 4.3. Final Feature Matrix

  - First step is to create a lookup for the previous game played by
    every team and then use that to join and get features for a game

<!-- end list -->

``` r
last_game_lookup <- team_logs %>%
  group_by(year_season, name_team) %>%
  arrange(id_game) %>%
  mutate(id_game_prev = lag(id_game)) %>%
  select(year_season, name_team, id_game, id_game_prev) %>% 
  filter(!is.na(id_game_prev))

last_game_lookup
```

    ## # A tibble: 13,702 x 4
    ## # Groups:   year_season, name_team [180]
    ##    year_season name_team               id_game id_game_prev
    ##          <dbl> <chr>                     <dbl>        <dbl>
    ##  1        2015 Houston Rockets        21400012     21400003
    ##  2        2015 Los Angeles Lakers     21400013     21400003
    ##  3        2015 Washington Wizards     21400016     21400007
    ##  4        2015 Orlando Magic          21400016     21400001
    ##  5        2015 Detroit Pistons        21400017     21400011
    ##  6        2015 Minnesota Timberwolves 21400017     21400009
    ##  7        2015 New York Knicks        21400018     21400010
    ##  8        2015 Utah Jazz              21400019     21400012
    ##  9        2015 Dallas Mavericks       21400019     21400002
    ## 10        2015 Oklahoma City Thunder  21400020     21400015
    ## # ... with 13,692 more rows

  - Time to bring all of the features we have created together

  - An extra step we will do for the box scores part of features in
    order to simplify the information further is to find the difference
    between the home team’s feature and away team’s feature

  - Therefore, instead of having `home_treb_team` and `away_treb_team`
    we will subtract one from another and have just a `diff_treb_team`
    and capture the same information in fewer columns

<!-- end list -->

``` r
features <- game_level %>%
  inner_join(
    last_game_lookup,
    by = c(
      "id_game" = "id_game",
      "home_team" = "name_team",
      "year_season" = "year_season"
    )
  ) %>%
  rename(home_team_id_game_prev = id_game_prev) %>%
  inner_join(
    last_game_lookup,
    by = c(
      "id_game" = "id_game",
      "away_team" = "name_team",
      "year_season" = "year_season"
    )
  ) %>%
  rename(away_team_id_game_prev = id_game_prev) %>%
  inner_join(
    calculated_elos_expanded,
    by = c(
      "year_season" = "year_season",
      "home_team_id_game_prev" = "id_game",
      "home_team" = "name_team"
    )
  ) %>%
  rename(home_elo = elo) %>%
  inner_join(
    calculated_elos_expanded,
    by = c(
      "year_season" = "year_season",
      "away_team_id_game_prev" = "id_game",
      "away_team" = "name_team"
    )
  ) %>%
  rename(away_elo = elo) %>%
  inner_join(
    rolling_mean_features,
    by = c(
      "year_season" = "year_season",
      "home_team_id_game_prev" = "id_game",
      "home_team" = "name_team"
    )
  ) %>%
  rename(
    home_pct_fg_team = pct_fg_team,
    home_pct_fg3team = pct_fg3team,
    home_pct_ft_team = pct_ft_team,
    home_treb_team = treb_team,
    home_ast_team = ast_team,
    home_stl_team = stl_team,
    home_blk_team = blk_team,
    home_tov_team = tov_team,
    home_plusminus_team = plusminus_team
  ) %>%
  inner_join(
    rolling_mean_features,
    by = c(
      "year_season" = "year_season",
      "away_team_id_game_prev" = "id_game",
      "away_team" = "name_team"
    )
  ) %>%
  rename(
    away_pct_fg_team = pct_fg_team,
    away_pct_fg3team = pct_fg3team,
    away_pct_ft_team = pct_ft_team,
    away_treb_team = treb_team,
    away_ast_team = ast_team,
    away_stl_team = stl_team,
    away_blk_team = blk_team,
    away_tov_team = tov_team,
    away_plusminus_team = plusminus_team
  ) %>%
  mutate(
    diff_pct_fg_team = home_pct_fg_team - away_pct_fg_team,
    diff_pct_fg3team = home_pct_fg3team - away_pct_fg_team,
    diff_pct_ft_team = home_pct_ft_team - away_pct_ft_team,
    diff_treb_team = home_treb_team - away_treb_team,
    diff_ast_team = home_ast_team - away_ast_team,
    diff_stl_team = home_stl_team - away_stl_team,
    diff_blk_team = home_blk_team - away_blk_team,
    diff_tov_team = home_tov_team - away_tov_team,
    diff_plusminus_team = home_plusminus_team - away_plusminus_team
  ) %>%
  inner_join(
    team_logs %>%
      select(
        year_season,
        id_game,
        name_team,
        count_days_rest_team,
        count_days_next_game_team
      ),
    by = c(
      "year_season" = "year_season",
      "home_team" = "name_team",
      "id_game" = "id_game"
    )
  ) %>%
  rename(home_count_days_rest_team = count_days_rest_team,
         home_count_days_next_game_team = count_days_next_game_team) %>%
  inner_join(
    team_logs %>%
      select(
        year_season,
        id_game,
        name_team,
        count_days_rest_team,
        count_days_next_game_team
      ),
    by = c(
      "year_season" = "year_season",
      "away_team" = "name_team",
      "id_game" = "id_game"
    )
  ) %>%
  rename(away_count_days_rest_team = count_days_rest_team,
         away_count_days_next_game_team = count_days_next_game_team) %>%
  mutate(
    home_count_days_next_game_team = ifelse(
      is.na(home_count_days_next_game_team),
      -1,
      home_count_days_next_game_team
    ),
    away_count_days_next_game_team = ifelse(
      is.na(away_count_days_next_game_team),
      -1,
      away_count_days_next_game_team
    )
  ) %>%
  select(
    year_season,
    id_game,
    date_game,
    home_team,
    away_team,
    home_elo,
    away_elo,
    starts_with("diff_"),
    home_count_days_rest_team,
    home_count_days_next_game_team,
    away_count_days_rest_team,
    away_count_days_next_game_team,
    home_team_margin
  )

features
```

    ## # A tibble: 6,469 x 21
    ##    year_season id_game date_game  home_team away_team home_elo away_elo
    ##          <dbl>   <dbl> <date>     <chr>     <chr>        <dbl>    <dbl>
    ##  1        2015  2.14e7 2014-11-07 Oklahoma~ Memphis ~    1476.    1550.
    ##  2        2015  2.14e7 2014-11-07 Toronto ~ Washingt~    1529.    1530.
    ##  3        2015  2.14e7 2014-11-07 Utah Jazz Dallas M~    1491.    1511.
    ##  4        2015  2.14e7 2014-11-07 Phoenix ~ Sacramen~    1507.    1529.
    ##  5        2015  2.14e7 2014-11-07 Philadel~ Chicago ~    1451.    1529.
    ##  6        2015  2.14e7 2014-11-08 Milwauke~ Memphis ~    1483.    1559.
    ##  7        2015  2.14e7 2014-11-08 Indiana ~ Washingt~    1462.    1521.
    ##  8        2015  2.14e7 2014-11-08 Miami He~ Minnesot~    1507.    1491.
    ##  9        2015  2.14e7 2014-11-08 Los Ange~ Portland~    1507.    1507.
    ## 10        2015  2.14e7 2014-11-08 Chicago ~ Boston C~    1538.    1490.
    ## # ... with 6,459 more rows, and 14 more variables: diff_pct_fg_team <dbl>,
    ## #   diff_pct_fg3team <dbl>, diff_pct_ft_team <dbl>, diff_treb_team <dbl>,
    ## #   diff_ast_team <dbl>, diff_stl_team <dbl>, diff_blk_team <dbl>,
    ## #   diff_tov_team <dbl>, diff_plusminus_team <dbl>,
    ## #   home_count_days_rest_team <dbl>, home_count_days_next_game_team <dbl>,
    ## #   away_count_days_rest_team <dbl>, away_count_days_next_game_team <dbl>,
    ## #   home_team_margin <dbl>

  - We have our features ready along with our target label of
    `home_team_margin` which is what we are trying to predict

### 4.4. How to make feature engineering better

  - Other steps we can take to be more thorough with feature engineering
    are
    
      - Optimise for the right window to calculate rolling stats instead
        of just assuming 5 games
    
      - Add more advanced stats - Currently we only have basic stats
        like box scores. There are advanced stats available elsewhere
        which might be more predictive.
    
      - More complex schedule stats - Building in features such as how
        much a team travels, whether a game is a national TV telecast or
        not etc also has an impact on how much a team tries or how well
        it plays

## 5\. Build A Machine Learning Model

  - We are now ready to build a model

  - There are several different ways of going about building a model
    
      - There are several algorithms to pick from, starting from simple
        models like Linear Regression to more complex models like Deep
        Learning
    
      - Within each algorithm there are several hyper parameters to tune
        to get the optimal model. This process is not dissimilar to how
        we built our ELO Model.
    
      - With any algorithm we also need to pick an optimisation metric.
        Considering that we are predicting the margin, and the
        competition is scored on Root Mean Squared Error, that should be
        what we use for this.

  - For the purposes of this guide - we will build a `XGBoost` model
    using the `parsnip` & `tidymodels` model framework

  - Before we build a model we have to take a few steps

### 5.1. Splitting Data

  - Our data has to be split into two sets - testing set & training set

  - Training set is the data that we will build the model on (80% of our
    data)

  - Testing set is the data that we will use to check whether is the
    model is any good at generalising on new data (20% of our data)

  - The reason this splitting is done is to ensure that the model
    actually learns generic patterns and is not memorising one
    particular data set. The latter will give you amazing indicators as
    you build the model but will fail terribly when you have to predict
    unknown matches.

<!-- end list -->

``` r
splits <- initial_split(
  features,
  prop = 0.8
)
```

### 5.2. Pre processing

  - Pre processing is the step where we transform raw data into data
    that machine learning algorithms typically like

  - The `recipes` package helps us convert the preprocessing step into a
    pipeline that can then be used on new data

  - There are plenty of preprocessing steps that are available, but in
    our case we will only do something called centering and scaling,
    where we standardise our data so that all numeric columns are in the
    same scale and dimensions.

  - We will also remove unnecessary columns for example `id_game` which
    we won’t be using to train the model

<!-- end list -->

``` r
preprocessing_recipe <-
  recipe(home_team_margin ~ ., data = splits %>% training()) %>%
  step_rm(year_season, id_game, date_game, home_team, away_team) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())  %>%
  prep()

features_proprocessed <- preprocessing_recipe %>%
  bake(splits %>% training())
```

### 5.2. Cross Validation

  - Cross Validation is another step that we take to emphasise the point
    in the previous section, where we build models on subsets of data
    and validate them on unknown data

  - Read more about cross validation here -\>
    <https://en.wikipedia.org/wiki/Cross-validation_(statistics)>

  - We take the training data and create 5 fold sub sample sets with an
    80-20 percent split (again like the previous step) in each fold.

<!-- end list -->

``` r
## Set Seed for reproducibility 
set.seed(1729)
cv_folds <- training(splits) %>%
  bake(preprocessing_recipe, new_data = .) %>%
  vfold_cv(v = 5)
```

### 5.3. Model specification

  - Every model algorith has several parameters that can be tuned to
    achieve optimal results

  - In the case of `XGBoost` there are parameters such as `min_n`,
    `tree_depth`, and `learn_rate` which can be specified.

  - For the moment we will set them as placeholders and use a range of
    parameters in later steps to then decide which one is best for us

<!-- end list -->

``` r
xgboost_model <- boost_tree(
  mode       = "regression",
  trees      = 500,
  min_n      = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost")
```

### 5.4. Grid Search

  - Grid search is the process of specifying a variety of parameter
    values to be fed into the model

  - Popular methods for creating grids are specifying ranges for
    parameters, or random numbers with a view that training over a
    random set of params over a long period results in some form of
    optimisation.

  - We use the random approach using the function `grid_max_entropy` to
    create a set of 15 parameters.

  - The more number of parameters we have, the more time training the
    model takes. Especially because we have 5 fold cross validation, we
    will be training `5*15` models and `5*15*500` trees because we set
    up the model to train 500 trees for each set of params.

  - On the flip side we have more parameters we are more likely to find
    the optimal solution. It is always a tradeoff. For the purposes of
    this tutorial lets keep it simple to 15.

<!-- end list -->

``` r
## Set Seed for reproducibility
set.seed(1729)
xgboost_params <- parameters(min_n(), tree_depth(), learn_rate())
xgboost_grid <- grid_max_entropy(xgboost_params, size = 15)
xgboost_grid
```

    ## # A tibble: 15 x 3
    ##    min_n tree_depth learn_rate
    ##    <int>      <int>      <dbl>
    ##  1     9          2   1.28e- 2
    ##  2     3         14   1.02e- 5
    ##  3    39         13   1.37e- 3
    ##  4    40          3   2.72e-10
    ##  5    18         12   9.77e- 9
    ##  6     2          9   1.72e- 3
    ##  7    17          8   3.87e- 5
    ##  8     4          1   9.39e-10
    ##  9    39         13   9.15e-10
    ## 10    36          2   8.87e- 3
    ## 11    33          2   9.06e- 7
    ## 12    16          5   2.81e-10
    ## 13    32          8   5.27e- 2
    ## 14    30          9   3.74e-10
    ## 15    15         13   3.38e- 2

### 5.5. Hyperparameter Tuning

  - The below code runs the model tuning pipeline where all the steps
    outlined above come together

  - All the cross validated datasets go through the xgboost model, with
    the different combinations of params set up in the grid above

  - They are all tested against the RMSE cost function and we will pick
    the set of parameters that perform the best

<!-- end list -->

``` r
xgboost_training_results <- tune_grid(
    formula   = home_team_margin  ~ .,
    model     = xgboost_model,
    resamples = cv_folds,
    grid      = xgboost_grid,
    metrics   = metric_set(rmse),
    control   = control_grid(verbose = TRUE)
)

xgboost_training_results %>%
  show_best("rmse", n = 5, maximize = FALSE)
```

    ## # A tibble: 5 x 8
    ##   min_n tree_depth learn_rate .metric .estimator  mean     n std_err
    ##   <int>      <int>      <dbl> <chr>   <chr>      <dbl> <int>   <dbl>
    ## 1     9          2    0.0128  rmse    standard    12.5     5   0.129
    ## 2    36          2    0.00887 rmse    standard    12.6     5   0.126
    ## 3     2          9    0.00172 rmse    standard    12.9     5   0.143
    ## 4    39         13    0.00137 rmse    standard    12.9     5   0.137
    ## 5    32          8    0.0527  rmse    standard    13.1     5   0.156

### 5.6. Model selection

  - Looks like top 5 set of parameters all get an RMSE of \~ 12 or 13 on
    our Cross Validation datasets.

  - A better idea of the performance of the model will be clearer when
    we run predictions and compare to the testing data set which we held
    out from the model training phase

  - For this we will get the parameter of the best model and add it as
    parameters for the `xgboost` model we specified early on as
    placeholders with just `tune()`

<!-- end list -->

``` r
xgboost_best_params <- xgboost_training_results %>% 
    select_best("rmse", maximize = FALSE)

xgboost_chosen_model <- xgboost_model %>% 
    finalize_model(xgboost_best_params)

xgboost_chosen_model
```

    ## Boosted Tree Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   trees = 500
    ##   min_n = 9
    ##   tree_depth = 2
    ##   learn_rate = 0.0128254872619247
    ## 
    ## Computational engine: xgboost

### 5.7. Performance on Test Set

  - To measure the performance of the chosen model on the test set we
    must first generate our predictions on the test set

  - The steps for this are
    
      - Do the preprocessing of test set data using the `recipe` we
        created on the training set
    
      - Generate the predictions
    
      - Append it to the test data set

<!-- end list -->

``` r
test_data_preprocessed <-
  bake(preprocessing_recipe, new_data = splits %>% testing())

predictions <- xgboost_chosen_model %>%
  fit(formula = home_team_margin  ~ .,
      data = features_proprocessed) %>%
  predict(new_data = test_data_preprocessed)

test_set_with_predictions <- splits %>%
  testing() %>%
  bind_cols(predictions) %>%
  rename(predicted_margin = `.pred`)

test_set_with_predictions
```

    ## # A tibble: 1,293 x 22
    ##    year_season id_game date_game  home_team away_team home_elo away_elo
    ##          <dbl>   <dbl> <date>     <chr>     <chr>        <dbl>    <dbl>
    ##  1        2015  2.14e7 2014-11-08 Miami He~ Minnesot~    1507.    1491.
    ##  2        2015  2.14e7 2014-11-08 Chicago ~ Boston C~    1538.    1490.
    ##  3        2015  2.14e7 2014-11-09 Oklahoma~ Sacramen~    1467.    1539.
    ##  4        2015  2.14e7 2014-11-09 Detroit ~ Utah Jazz    1490.    1480.
    ##  5        2015  2.14e7 2014-11-09 Los Ange~ Charlott~    1452.    1499.
    ##  6        2015  2.14e7 2014-11-10 Clevelan~ New Orle~    1494.    1509.
    ##  7        2015  2.14e7 2014-11-11 Dallas M~ Sacramen~    1510.    1529.
    ##  8        2015  2.14e7 2014-11-11 Golden S~ San Anto~    1540.    1502.
    ##  9        2015  2.14e7 2014-11-12 Phoenix ~ Brooklyn~    1506.    1515.
    ## 10        2015  2.14e7 2014-11-13 Golden S~ Brooklyn~    1527.    1506.
    ## # ... with 1,283 more rows, and 15 more variables: diff_pct_fg_team <dbl>,
    ## #   diff_pct_fg3team <dbl>, diff_pct_ft_team <dbl>, diff_treb_team <dbl>,
    ## #   diff_ast_team <dbl>, diff_stl_team <dbl>, diff_blk_team <dbl>,
    ## #   diff_tov_team <dbl>, diff_plusminus_team <dbl>,
    ## #   home_count_days_rest_team <dbl>, home_count_days_next_game_team <dbl>,
    ## #   away_count_days_rest_team <dbl>, away_count_days_next_game_team <dbl>,
    ## #   home_team_margin <dbl>, predicted_margin <dbl>

  - We then run the `metrics` function from the `yardstick` package to
    calculate the performance of our model on the unknown testing data
    set, in comparison to actual margins

<!-- end list -->

``` r
metrics(test_set_with_predictions,
        truth = "home_team_margin",
        estimate = "predicted_margin")
```

    ## # A tibble: 3 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 rmse    standard      12.9  
    ## 2 rsq     standard       0.165
    ## 3 mae     standard      10.2

  - Looks like the RMSE on this data set is not too dissimilar to the
    RMSE on the cross validation data sets. This is a good sign, and a
    sign that our CV worked and the model hasn’t just memorised from the
    training data.

  - If this wasn’t the case we’d need to go back to the drawing board
    and re-tune the parameters until we found a model we are happy with

### 5.8. Retraining the model

  - Now that we are happy with the model we can use the information
    missed out on the test data set to create an updated final model
    learnt on the entire data with the chosen parameters

<!-- end list -->

``` r
final_model <- xgboost_chosen_model %>%
  fit(formula = home_team_margin  ~ .,
      data = bake(preprocessing_recipe, new_data = features))

final_model
```

    ## parsnip model object
    ## 
    ## Fit time:  1.4s 
    ## ##### xgb.Booster
    ## raw: 196.8 Kb 
    ## call:
    ##   xgboost::xgb.train(params = list(eta = 0.0128254872619247, max_depth = 2L, 
    ##     gamma = 0, colsample_bytree = 1, min_child_weight = 9L, subsample = 1), 
    ##     data = x, nrounds = 500, verbose = 0, objective = "reg:linear", 
    ##     nthread = 1)
    ## params (as set within xgb.train):
    ##   eta = "0.0128254872619247", max_depth = "2", gamma = "0", colsample_bytree = "1", min_child_weight = "9", subsample = "1", objective = "reg:linear", nthread = "1", silent = "1"
    ## xgb.attributes:
    ##   niter
    ## # of features: 15 
    ## niter: 500
    ## nfeatures : 15

### 5.9. How to make the modelling process better

  - We have gone through a fairly robust modelling process, but there
    are obviously lots more things you can do to improve your final
    model

  - Firstly, we can be a bit more thorough with hyper parameter tuning
    and try a larger range of options. We have restricted ourselves to
    15 for the purposes of this tutorial but with more parameters we are
    more likely to find a more optimal model.

  - Secondly, we could try alternate algorithms to `xgboost`. Simple
    models like `glm` could work well for regression problems, or we
    could go super complex and build something on `tensorflow` using
    deep learning. Each algorithm has tradeoffs.

  - Alternatively we could use the now common practice of `automl`. What
    `automl` does is take what we have done to the next level by
    automating the boring parts of hyperparameter tuning but also do it
    across a wide range of algorithms. An `automl` pipeline will build
    you multiple `randomforest`, `gbm`, `xgboost` models across multiple
    hyperparameters and let you choose the best one based on the best
    RMSE. `automl` models also have the capability of stacking - which
    is taking the outputs of multiple models and building an ensemble of
    models. The tradeoff here is, it could take a long time for you to
    get a good `automl` model but it erases a lot of the brunt work
    required to get there.

  - As always predictive modelling follows the principle of Garbage In
    Garbage Out, so the better quality data we have going into the
    modelling, the better our model is going to be. Nothing influences
    the quality of a model as much as better quality data. If you can
    find better data, more detailed data, it will certainly help in a
    better model.

## 6\. Predicting on unplayed games and creating a submission

  - Now that we have a final model ready, we are in a position to create
    a submission for the datathon

  - The steps required to create a submission file are
    
      - Create features for the matches in the provided submission files
        template. Timing is key here because we’d need information until
        the last match played before the matches we are making
        predictions for. However because this tutorial can’t wait until
        then, we can take the liberty to assume the last match played is
        the last match before the match in the submission file. Its not
        perfect, but the backbone of the code doesn’t need to change, we
        just re-run the predictions piece when fresher data is
        available.
    
      - The feature set would then need to be preprocessed, again using
        the `recipe` we created in the model training phase
    
      - Once the preprocessed data is ready, we run that through a
        `predict` function using the final model we have created and
        voila\!

<!-- end list -->

``` r
submission_file_week_1 <- read_csv(here::here("data", "submission_file_week_1.csv"))
season_schedule_2020 <- read_csv(here::here("data", "season_schedule_2020.csv"))
```

### 6.1. Features

  - Here we will take the rolling average data frame that we created
    earlier, and get the value for the last update

<!-- end list -->

``` r
most_recent_features <- 
  rolling_mean_features %>%
  filter(year_season == 2020) %>%
  group_by(name_team) %>%
  top_n(n = 1, wt = id_game) %>%
  ungroup() %>%
  select(-year_season, -id_game)

most_recent_features
```

    ## # A tibble: 30 x 10
    ##    name_team pct_fg_team pct_fg3team pct_ft_team treb_team ast_team stl_team
    ##    <chr>           <dbl>       <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
    ##  1 Milwauke~       0.468       0.342       0.701      56.2     27.6      5.8
    ##  2 Orlando ~       0.439       0.341       0.766      40       25.2      8  
    ##  3 Dallas M~       0.443       0.439       0.934      48.2     24.2      5.2
    ##  4 Charlott~       0.445       0.314       0.724      42.6     24.4      5.8
    ##  5 Detroit ~       0.452       0.385       0.718      40.2     25        6.2
    ##  6 Indiana ~       0.477       0.333       0.744      40       25.8      5.4
    ##  7 New Orle~       0.479       0.369       0.753      53.4     28.4      8.8
    ##  8 Toronto ~       0.506       0.405       0.793      42.4     26.2      8.4
    ##  9 Brooklyn~       0.496       0.408       0.779      49       28        6  
    ## 10 Minnesot~       0.473       0.374       0.729      42.6     27        9.8
    ## # ... with 20 more rows, and 3 more variables: blk_team <dbl>, tov_team <dbl>,
    ## #   plusminus_team <dbl>

  - Similarly we will do that for the calculated ELO as well

<!-- end list -->

``` r
most_recent_elo <- 
  calculated_elos_expanded %>%
  filter(year_season == 2020) %>%
  group_by(name_team) %>%
  top_n(n = 1, wt = id_game) %>%
  ungroup() %>%
  select(-year_season, -id_game)

most_recent_elo
```

    ## # A tibble: 30 x 2
    ##    name_team                elo
    ##    <chr>                  <dbl>
    ##  1 Orlando Magic          1454.
    ##  2 Milwaukee Bucks        1722.
    ##  3 Charlotte Hornets      1363.
    ##  4 Dallas Mavericks       1535.
    ##  5 Detroit Pistons        1381.
    ##  6 Indiana Pacers         1531.
    ##  7 New Orleans Pelicans   1495.
    ##  8 Toronto Raptors        1662.
    ##  9 Brooklyn Nets          1460.
    ## 10 Minnesota Timberwolves 1364.
    ## # ... with 20 more rows

  - Finally we need to calculate the data for the `count_days_rest_team`
    and `count_days_next_game_team` because they aren’t available to us
    yet. For this we will use the 2020 Season Schedule file

<!-- end list -->

``` r
schedule_features <- season_schedule_2020 %>%
  select(date_game, name_team_home) %>%
  rename(name_team = name_team_home) %>%
  bind_rows(
    season_schedule_2020 %>%
      select(date_game, name_team_away) %>%
      rename(name_team = name_team_away)
  ) %>%
  arrange(date_game) %>%
  group_by(name_team) %>%
  mutate(next_game_date = lead(date_game),
         prev_game_date = lag(date_game)) %>%
  ungroup() %>%
  filter(!is.na(next_game_date)) %>%
  filter(!is.na(prev_game_date)) %>%
  mutate(
    count_days_rest_team = (
      difftime(date_game, prev_game_date, units = "days") %>% as.integer()
    ) - 1,
    count_days_next_game_team = (
      difftime(next_game_date, date_game, units = "days") %>% as.integer() - 1
    )
  ) %>%
  select(date_game, name_team, count_days_rest_team, count_days_next_game_team)

schedule_features
```

    ## # A tibble: 2,400 x 4
    ##    date_game  name_team             count_days_rest_te~ count_days_next_game_te~
    ##    <date>     <chr>                               <dbl>                    <dbl>
    ##  1 2019-10-24 Detroit Pistons                         0                        1
    ##  2 2019-10-24 Los Angeles Clippers                    1                        1
    ##  3 2019-10-25 Boston Celtics                          1                        0
    ##  4 2019-10-25 Charlotte Hornets                       1                        1
    ##  5 2019-10-25 Brooklyn Nets                           1                        1
    ##  6 2019-10-25 Memphis Grizzlies                       1                        1
    ##  7 2019-10-25 New Orleans Pelicans                    2                        0
    ##  8 2019-10-25 Oklahoma City Thunder                   1                        1
    ##  9 2019-10-25 Denver Nuggets                          1                        2
    ## 10 2019-10-25 Sacramento Kings                        1                        0
    ## # ... with 2,390 more rows

  - Now lets bring all of the features together

<!-- end list -->

``` r
prediction_features <- submission_file_week_1 %>%
  select(-home_team_predicted_margin) %>%
  inner_join(most_recent_elo, by = c("home_team" = "name_team")) %>%
  rename(home_elo = elo) %>%
  inner_join(most_recent_elo, by = c("away_team" = "name_team")) %>%
  rename(away_elo = elo) %>%
  inner_join(most_recent_features, by = c("home_team" = "name_team")) %>%
  rename(
    home_pct_fg_team = pct_fg_team,
    home_pct_fg3team = pct_fg3team,
    home_pct_ft_team = pct_ft_team,
    home_treb_team = treb_team,
    home_ast_team = ast_team,
    home_stl_team = stl_team,
    home_blk_team = blk_team,
    home_tov_team = tov_team,
    home_plusminus_team = plusminus_team
  ) %>%
  inner_join(most_recent_features, by = c("away_team" = "name_team")) %>%
  rename(
    away_pct_fg_team = pct_fg_team,
    away_pct_fg3team = pct_fg3team,
    away_pct_ft_team = pct_ft_team,
    away_treb_team = treb_team,
    away_ast_team = ast_team,
    away_stl_team = stl_team,
    away_blk_team = blk_team,
    away_tov_team = tov_team,
    away_plusminus_team = plusminus_team
  ) %>%
  mutate(
    diff_pct_fg_team = home_pct_fg_team - away_pct_fg_team,
    diff_pct_fg3team = home_pct_fg3team - away_pct_fg_team,
    diff_pct_ft_team = home_pct_ft_team - away_pct_ft_team,
    diff_treb_team = home_treb_team - away_treb_team,
    diff_ast_team = home_ast_team - away_ast_team,
    diff_stl_team = home_stl_team - away_stl_team,
    diff_blk_team = home_blk_team - away_blk_team,
    diff_tov_team = home_tov_team - away_tov_team,
    diff_plusminus_team = home_plusminus_team - away_plusminus_team
  ) %>%
  inner_join(schedule_features,
             by = c("home_team" = "name_team",
                    "date" = "date_game")) %>%
  rename(home_count_days_rest_team = count_days_rest_team,
         home_count_days_next_game_team = count_days_next_game_team) %>%
  inner_join(schedule_features,
             by = c("away_team" = "name_team",
                    "date" = "date_game")) %>%
  rename(away_count_days_rest_team = count_days_rest_team,
         away_count_days_next_game_team = count_days_next_game_team) %>%
  select(
    date,
    home_team,
    away_team,
    home_elo,
    away_elo,
    starts_with("diff_"),
    home_count_days_rest_team,
    home_count_days_next_game_team,
    away_count_days_rest_team,
    away_count_days_next_game_team
  )

prediction_features
```

    ## # A tibble: 17 x 18
    ##    date       home_team away_team home_elo away_elo diff_pct_fg_team
    ##    <date>     <chr>     <chr>        <dbl>    <dbl>            <dbl>
    ##  1 2020-03-07 Charlott~ Houston ~    1363.    1582.         0.0162  
    ##  2 2020-03-07 Detroit ~ Utah Jazz    1381.    1605.        -0.00560 
    ##  3 2020-03-07 Clevelan~ Denver N~    1300.    1631.        -0.036   
    ##  4 2020-03-07 Memphis ~ Atlanta ~    1524.    1392.         0.000600
    ##  5 2020-03-07 Golden S~ Philadel~    1368.    1570.         0.0256  
    ##  6 2020-03-07 Portland~ Sacramen~    1532.    1451.        -0.0164  
    ##  7 2020-03-08 Brooklyn~ Chicago ~    1460.    1386.         0.0178  
    ##  8 2020-03-08 Minnesot~ New Orle~    1364.    1495.        -0.00580 
    ##  9 2020-03-08 Los Ange~ Los Ange~    1614.    1625.        -0.0236  
    ## 10 2020-03-08 Boston C~ Oklahoma~    1622.    1595.         0.0114  
    ## 11 2020-03-08 Phoenix ~ Milwauke~    1409.    1722.         0.00560 
    ## 12 2020-03-08 Washingt~ Miami He~    1405.    1564.         0.0018  
    ## 13 2020-03-08 Dallas M~ Indiana ~    1535.    1531.        -0.0338  
    ## 14 2020-03-08 Houston ~ Orlando ~    1582.    1454.        -0.0102  
    ## 15 2020-03-08 Clevelan~ San Anto~    1300.    1477.        -0.0074  
    ## 16 2020-03-08 New York~ Detroit ~    1383.    1381.         0.0198  
    ## 17 2020-03-08 Sacramen~ Toronto ~    1451.    1662.        -0.0164  
    ## # ... with 12 more variables: diff_pct_fg3team <dbl>, diff_pct_ft_team <dbl>,
    ## #   diff_treb_team <dbl>, diff_ast_team <dbl>, diff_stl_team <dbl>,
    ## #   diff_blk_team <dbl>, diff_tov_team <dbl>, diff_plusminus_team <dbl>,
    ## #   home_count_days_rest_team <dbl>, home_count_days_next_game_team <dbl>,
    ## #   away_count_days_rest_team <dbl>, away_count_days_next_game_team <dbl>

### 6.2. Predicting the margin

  - Now that we have the features ready, we use the `recipe` to
    preprocess and finally predict based on the `xgboost` model that we
    have built

<!-- end list -->

``` r
predictions_to_submit <- final_model %>%
  predict(new_data = bake(preprocessing_recipe, new_data = prediction_features))

submission_file_with_predictions <- submission_file_week_1 %>%
  select(-home_team_predicted_margin) %>%
  bind_cols(predictions_to_submit) %>%
  rename(home_team_predicted_margin = `.pred`)

submission_file_with_predictions
```

    ## # A tibble: 17 x 4
    ##    date       home_team             away_team           home_team_predicted_mar~
    ##    <date>     <chr>                 <chr>                                  <dbl>
    ##  1 2020-03-07 Charlotte Hornets     Houston Rockets                        -7.79
    ##  2 2020-03-07 Detroit Pistons       Utah Jazz                              -3.16
    ##  3 2020-03-07 Cleveland Cavaliers   Denver Nuggets                         -9.90
    ##  4 2020-03-07 Memphis Grizzlies     Atlanta Hawks                           7.13
    ##  5 2020-03-07 Golden State Warriors Philadelphia 76ers                     -2.85
    ##  6 2020-03-07 Portland Trail Blaze~ Sacramento Kings                        4.15
    ##  7 2020-03-08 Brooklyn Nets         Chicago Bulls                           5.34
    ##  8 2020-03-08 Minnesota Timberwolv~ New Orleans Pelica~                    -4.25
    ##  9 2020-03-08 Los Angeles Clippers  Los Angeles Lakers                      3.38
    ## 10 2020-03-08 Boston Celtics        Oklahoma City Thun~                     3.81
    ## 11 2020-03-08 Phoenix Suns          Milwaukee Bucks                        -7.30
    ## 12 2020-03-08 Washington Wizards    Miami Heat                             -3.70
    ## 13 2020-03-08 Dallas Mavericks      Indiana Pacers                          3.74
    ## 14 2020-03-08 Houston Rockets       Orlando Magic                           7.93
    ## 15 2020-03-08 Cleveland Cavaliers   San Antonio Spurs                      -5.18
    ## 16 2020-03-08 New York Knicks       Detroit Pistons                         4.51
    ## 17 2020-03-08 Sacramento Kings      Toronto Raptors                        -5.40

### 6.3. Submitting the predictions

  - All we now need to do is write our predictions data frame into a csv
    file and then submit
it\!

<!-- end list -->

``` r
write_csv(submission_file_with_predictions, here::here("data", "bf_ds_tutorial_submission_file_week_1.csv"))
```

  - The process is not going to be any different for the future weeks.
    Once the data is refreshed, the feature pipeline needs to be
    refreshed too. Load the right CSV for the appropriate week and
    repeat the process\!

## 7\. Baseline

  - Often a really useful concept with predictive models is establishing
    what is known as a baseline

  - A baseline is a simple rule of thumb that your model must beat for
    it to be any good

  - A simple baseline for a model that predicts whether a team beats
    another team in a match, should be better than random guessing, or
    in mathematical terms, have an accuracy better than 50%

  - A more intelligent baseline would be to consider something like home
    advantage and if you assume home teams win all the time, and that
    gives you say a 60% accuracy, your model is only better if it can be
    better than that and account for complexities

  - With the context of betting markets, we could also establish
    baselines like - the favourite always wins. If our model can beat
    this assumption, that’s a positive sign.

  - Baselines are trickier for margin prediction models like the one
    outlined in this tutorial. Taking the above point further, we could
    establish a baseline by calculating the RMSE of a betting line
    market. Theoretically it is the same as predicted margin of victory
    for the favourite against the underdog, and therefore our model
    needs to be around as good as that. Being better than it is hard
    because betting markets contain the collective opinion of punters’
    weight of money, and anything better than that is your edge. Feel
    free to use the odds data provided to establish your own baseline.

## 8\. Conclusion

  - Hopefully you enjoyed the tutorial and that was easy to follow.

  - As mentioned several times before, this is by no means an exhaustive
    approach to building a model, please do try the alternative
    approaches and you’ll definitely see an improvement

  - Don’t forget to submit your predictions before the deadlines
    (Saturday morning AEDT)

  - Feel free to reach out to us at <Datathon@betfair.com.au> if you
    have any questions at all

  - Good luck\!

![](https://media.giphy.com/media/10AYkGR9M75nLW/giphy.gif)
