# R/scores.R

# Summary Stats
IMDb_Rate_Count <- summarise_my_scores_by_field(Tidy_Ratings, IMDb_Rating) 
IMDb_Whole_Count <- summarise_my_scores_by_field(Tidy_Ratings, IMDb_Rating_Rnd)

# Filter Data
IMDb_Rate_Count_Filter <- IMDb_Rate_Count %>% 
  filter(no_of_films >= Defaults$min_films) 

# Correlation data
Fit_Ave <-  lm(my_ave ~ IMDb_Rating, data = IMDb_Rate_Count_Filter)
Fit_Round_Ave <- lm(my_ave ~ IMDb_Rating_Rnd, data =IMDb_Whole_Count)

Fit <- lm(your_rating ~ IMDb_Rating, data =Tidy_Ratings)
Fit_round <- lm(your_rating ~ round(IMDb_Rating, 0), data =Tidy_Ratings)

Ext_Diff <- Tidy_Ratings %>% 
  select(rate_dif) %>% 
  summarise(no_of_films = n(), big_dif = sum(rate_dif < Defaults$ex_dif & rate_dif > -Defaults$ex_dif))

# Stats for Overall Score Summary
stats = list(
  "Averages" = mean,
  "Standard Deviation" = sd,
  "Max Value" = max,
  "Min Value" = min,
  "Median Value" = median
)

# Average Film List Options and Constants to Play with
#Set function to use e.g. median or mean
Fun_List <- list(
  my = median(Tidy_Ratings$your_rating),
  pub = median(Tidy_Ratings$IMDb_Rating, na.rm = T),
  year = median(Tidy_Ratings$year),
  run = median(Tidy_Ratings$runtime_mins, na.rm = T),
  vot = median(Tidy_Ratings$num_votes)
)

# Variables to adjust
score_config <- list(
  year   = list(range = 5,   max_score = 10, power = 2),
  my     = list(range = 2,    max_score = 5, power = 1),
  pub = list(range = 0.9,  max_score = 20, power = 2),
  run = list(range = 0.1,   max_score = 20, power = 1, type = "relative"),
  vot  = list(range = 0.1, max_score = 10, power = 2, type = "relative")
)

# scoring function for linear data
score_linear <- function(x, center, max_range, max_score = 10, power = 2) {
  dist <- abs(x - center)
  ratio <- pmin(dist / max_range, 1)
  round(max_score * (1 - ratio)^power, 2)
}

# scoring function for variable data
score_relative <- function(x, center, max_pct, max_score = 10, power = 2) {
  if (center == 0) return(rep(0, length(x)))
  dist <- abs(x - center) / center
  ratio <- pmin(dist / max_pct, 1)
  round(max_score * (1 - ratio)^power, 2)
}


