library(tidyverse)

# data source is
# https://indianavoters.in.gov/ENRHistorical/ElectionResults?year=2022#

election_results <-
  tribble(~ Country, ~ State, ~ Year, ~ Election, ~ Sourced)

election_results <- add_row(
  election_results,
  Country = "USA",
  State = "IN",
  Year = 2022,
  Election = "Primary",
  Sourced = read_csv("data/in_primary_2022.csv", show_col_types = FALSE)
)

election_results <- add_row(
  election_results,
  Country = "USA",
  State = "IN",
  Year = 2022,
  Election = "General",
  Sourced = read_csv("data/in_general_2022.csv", show_col_types = FALSE)
)


election_results <- add_row(
  election_results,
  Country = "USA",
  State = "IN",
  Year = 2020,
  Election = "Primary",
  Sourced = read_csv("data/in_primary_2020.csv", show_col_types = FALSE)
)

election_results <- add_row(
  election_results,
  Country = "USA",
  State = "IN",
  Year = 2020,
  Election = "General",
  Sourced = read_csv("data/in_general_2020.csv", show_col_types = FALSE)
)


# bind_rows() use to add one tibble to the end of another

election_results <-
  unnest(election_results,
         cols = c(Sourced),
         keep_empty = TRUE)

# primary data was exported and the gender (M/F) manually added
# to the csv file
gender_levels <- c("M", "F", "O")

candidate_gender <- tribble( ~ Country, ~ State, ~ Year, ~ Sourced)

candidate_gender <- add_row(
  candidate_gender,
  Country = "USA",
  State = "IN",
  Year = 2022,
  Sourced = read_csv("data/need_gender.csv",   col_types = cols(Gender = col_factor(gender_levels))
)  |>
    mutate(District = if_else(District == "1", "01", District)) |>
    mutate(District = if_else(District == "2", "02", District)) |>
    mutate(District = if_else(District == "3", "03", District)) |>
    mutate(District = if_else(District == "4", "04", District)) |>
    mutate(District = if_else(District == "5", "05", District)) |>
    mutate(District = if_else(District == "6", "06", District)) |>
    mutate(District = if_else(District == "7", "07", District)) |>
    mutate(District = if_else(District == "8", "08", District)) |>
    mutate(District = if_else(District == "9", "09", District))
)

candidate_gender <-
  unnest(candidate_gender,
         cols = c(Sourced),
         keep_empty = TRUE)


test_files <- function() {
  x <- list.files("data", pattern = "in_general.*\\.csv")

  print(x)
  print("--------")

  for (q in 1:length(x)) {
    y <- strsplit(x[q], "_")

    state <- y[[1]][1]
    election <- y[[1]][2]
    z <- strsplit(y[[1]][3], "\\.")
    year <- z[[1]][1]

    print(paste("State = ", state))
    print(paste("Election = ", election))
    print(paste("Year = ", year))
    print("--------")

  }
}

load_indiana <- function(year, election) {
  result = tryCatch({
    Sourced = read_csv(paste("data/in_", election, "_", year, ".csv", sep = ""), show_col_types = FALSE)
    print("Loaded")
    Sourced
  }, warning = function(w) {
    print(w)
  }, error = function(e) {
    print(e)
  }, finally = {
    print("finally")
  })

}
#


