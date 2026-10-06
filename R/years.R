# R/years.R

# Import analysis specific datasets
Film_Nos = read_csv(here::here("data", "Movie_Stats.csv"))

# summarising the year stats but as IMDB doesn't show a rating for films wth less than 5 votes I have kept them in for my rating
# but as that is then estimated for IMDB there is value for some of these smaller films to filter them out
Year_Stats <- Tidy_Ratings %>%
  group_by(year) %>%
  summarise(
  "no_of_films" = n(),
  "ave_rat" = mean(your_rating),
  "my_sd_rat" = sd(your_rating),
  "my_se" = my_sd_rat / sqrt(no_of_films),
    # IMDb ratings - exclude films with only 1 vote
  "ave_irat" = mean(IMDb_Rating[num_votes > 1]),
  "pub_sd_rat" = sd(IMDb_Rating[num_votes > 1]),
  "pub_se" = pub_sd_rat / sqrt(sum(num_votes > 1)),
  "ave_runtime" = mean(runtime_mins),
  "ave_dif" = round(mean(rate_dif[num_votes > 1]), 2),
  "dif_sd_rat" = sd(rate_dif[num_votes > 1]),
  "dif_se" = dif_sd_rat / sqrt(sum(num_votes > 1)),
  "ave_votes" = mean(num_votes))


# Heatmap function
build_heatmap_data <- function() {
  Year_Stats %>%
    select(year, no_of_films, ave_rat, my_se, ave_irat, pub_se, ave_dif, dif_se) %>%
    inner_join(
      Film_Nos %>% select(year, IMDb_Countries_Rated_5_votes), #need better note of what this is so I can at least improve this column 
      # at least say inner join so don't adjust new years without the data so will need updating - also needs to be clear in report!!
      by = "year"
    ) %>%
    mutate(My_Ratio = no_of_films / IMDb_Countries_Rated_5_votes)
}


# labels for Rating
rating_choices <- c(
  "My Ratings" = "ave_rat",
  "Public Ratings" = "ave_irat",
  "Rating Difference" = "ave_dif"
)

# labels for axis changes
axis_choices <- c(
  "Rating Range" = "focused",
  "Full Rating Scale" = "full"
)

# label for heat map ratings
HM_rating_choices <- c("My Rating" = "my_ave",
  "Public Rating" = "pub_ave",
  "Rating Difference" = "dif_ave")

# matrixs for shiny plots
method_start_year <- c(
  IMDb_Studio_Films = 1967,
  IMDb_5_votes_60_min = 1967,
  IMDb_5_votes_60_min_Countries = 1967,
  IMDb_Countries_Rated_5_votes = 1967,
  Rated_My_Rated = 1967,
  US_Cinema_Release = 1999,
  Studio_Ind_Total = 1994
)

# Choices for film number comparison
vol_choices <- c(
  "IMDb Studio Films" = "IMDb_Studio_Films",
  "IMDb 5 votes 60 min" = "IMDb_5_votes_60_min",
  "IMDb 5 votes 60 min Countries" = "IMDb_5_votes_60_min_Countries",
  "IMDb Countries Rated 5 votes" = "IMDb_Countries_Rated_5_votes",
#  "Rated My Rated" = "Rated_My_Rated",
  "US Cinema Release" = "US_Cinema_Release",
  "Studio Ind Total" = "Studio_Ind_Total"
)

# Heat Map Choices
HM_choices <- c("Films Watched" = "My_Ratio",   
            "My Rating" = "ave_rat", 
            "Public Rating" = "ave_irat",
            "Rating Difference" = "ave_dif")

metric_map <- list(
  my_ave = list(
    mean_col = "ave_rat",
    global_mean = "my_ave",
    se_col = "my_se",
    type = "absolute"
  ),
  pub_ave = list(
    mean_col = "ave_irat",
    global_mean = "pub_ave",
    se_col = "pub_se",
    type = "absolute"
  ),
  dif_ave = list(
    mean_col = "ave_dif",
    global_mean = "dif_ave",
    se_col = "dif_se",
    type = "diverging"
  )
)


# IS THIS BEING USED?  JUST KEEP IN ORIG?
Top_counts <- Tidy_Ratings %>% 
  group_by(year) %>% 
  filter(n() > 19) %>% 
  summarise(
    no_of_films = n(),
    tens_tot = sum(your_rating == 10), tens_per = round(tens_tot / no_of_films * 100, 0),
    nines_tot = sum(your_rating == 9), nins_per = round(nines_tot / no_of_films * 100, 0),
    tot_9_10 = tens_tot + nines_tot, per_9_10  = round(tot_9_10 / no_of_films * 100, 0)
  ) %>% 
  arrange(desc(per_9_10))

# Max year for movie volume comparison
year_vol_max <- 2025

# House Style for time series graph
point_col <- "darkgreen"
line_col <- "blue"
