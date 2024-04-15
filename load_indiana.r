library(tidyverse)

# data source is
# https://indianavoters.in.gov/ENRHistorical/ElectionResults?year=2022#

#------------------------------------------------------------------------
#
#------------------------------------------------------------------------
in_load_candidate_meta <- function(.data) {
  # primary data was exported and the gender (M/F) manually added
  # to the csv file

  gender_levels <- c("M", "F", "O")

  candidate_gender <-
    tribble( ~ Country, ~ State, ~ Year, ~ Sourced)

  candidate_gender <- add_row(
    candidate_gender,
    Country = "USA",
    State = "IN",
    Year = 2022,
    Sourced = read_csv("data/need_gender.csv",   col_types = cols(Gender = col_factor(gender_levels)))  |>
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

  .data <- bind_rows(.data, candidate_gender)
}

#------------------------------------------------------------------------
#
#------------------------------------------------------------------------
in_load_elections <- function(election_results) {
  x1 <- list.files("data", pattern = "in_general.*\\.csv")
  x2 <- list.files("data", pattern = "in_primary.*\\.csv")
  x <- c(x1, x2)

  print("--------")

  all_results <- NULL

  for (q in 1:length(x)) {
    y <- strsplit(x[q], "_")

    state <- y[[1]][1]
    election <- y[[1]][2]
    z <- strsplit(y[[1]][3], "\\.")
    year <- as.numeric(z[[1]][1])
    file <- paste("data/", x[q], sep = "")

    results <- in_load_election_file(file, year, election)
    if (is.null(all_results)) {
      all_results <- results
    } else {
      all_results <- bind_rows(all_results, results)
    }

  }

  print("--------")

  results <-
    unnest(all_results,
           cols = c(Sourced),
           keep_empty = TRUE)

  bind_rows(election_results, results)
}

#------------------------------------------------------------------------
in_load_election_file <- function(filename, year, election) {
  in_elections <-
    tribble(~ Country, ~ State, ~ Year, ~ Election, ~ Sourced)

  result = tryCatch({
    sourced  <-
      read_csv(filename,
               show_col_types = FALSE)
    print(paste("Loaded IN ", year, election))

    in_elections <- add_row(
      in_elections,
      Country = "USA",
      State = "IN",
      Year = as.integer(year),
      Election = election,
      Sourced = sourced
    )
  }, warning = function(w) {
    print(w)
  }, error = function(e) {
    print(e)
    sourced <- NULL
  }, finally = {
    in_elections
  })

}
#
