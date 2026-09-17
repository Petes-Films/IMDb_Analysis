# R/utils_data.R

#Import datasets
load(here::here("data", "moviedata.Rdata"))

# summarise data for my average score
summarise_my_scores_by_field <- function(df, col_group, min_films=1) {
  df %>%
    group_by({{col_group}}) %>%
    summarise("no_of_films" = n(), "my_ave" = mean(your_rating), "pub_ave" = mean(IMDb_Rating), "dif_ave" = my_ave - pub_ave) %>% 
    # do i need this much, or even expand.  how best to set this up
    filter(no_of_films >= min_films)
}

# Run stat summaries across dataset
Stats_Table <- function(
    df,
    stats = list("Averages" = mean, "Standard Deviation" = sd, "Max Value" = max)
    )
{df %>%
    select(your_rating, IMDb_Rating, year, runtime_mins, num_votes) %>%
    {# build one row per function, with label in Variable
      imap_dfr(stats, \(fn, label) {
        summarise(., 
                  across(
                    everything(), \(x) fn(x)
                  )
        ) %>%
          mutate(Variable = label, .before = 1)
      })
    }
}

# Clean Output Table for Stats  
Stats_Table_Format <- function(
    df,
    title = "Overall Statistics", 
    sigfig=3, 
    threshold=100
    )   
{
  df %>% 
    gt() %>%
    cols_label(
      Variable     ~ "Variable",
      your_rating  ~ "My Rating",
      IMDb_Rating  ~ "Public Rating",
      year         ~ "Year",
      runtime_mins ~ "Run Time (Mins)",
      num_votes    ~ "Number of Votes"
    ) %>%
    fmt_number(
      columns = where(is.numeric),
      n_sigfig = sigfig,
      use_seps = T) %>%
    fmt_integer(
      columns = year,
      use_seps = F
    ) %>%
    fmt_integer(
      columns = num_votes,
      use_seps = T
    ) %>%
    tab_header(title = title) %>%
    my_gt_theme()
}

# Overall Summary Numbers
Total_Stats <- list(
  tot_films = nrow(Tidy_Ratings),
  my_ave = mean(Tidy_Ratings$your_rating),
  my_sd_ave = sd(Tidy_Ratings$your_rating),
  pub_ave = mean(Tidy_Ratings$IMDb_Rating, na.rm = T),
  pub_sd = sd(Tidy_Ratings$IMDb_Rating, na.rm = T),
  dif_ave = mean(Tidy_Ratings$rate_dif, na.rm = T),
  dif_lowerq = quantile(Tidy_Ratings$rate_dif, probs = c(0.025)),
  dif_upperq = quantile(Tidy_Ratings$rate_dif, probs = c(0.975))
)

