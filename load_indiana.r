library(tidyverse)

# data source is
# https://indianavoters.in.gov/ENRHistorical/ElectionResults?year=2022#

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

  # .data <- bind_rows(.data, candidate_gender)
  bind_rows(.data, candidate_gender)
}


#------------------------------------------------------------------------
#------------------------------------------------------------------------
in_load_elections2 <- function(election_results) {
  extract_election <- function(filename) {
    temp <- strsplit(filename, "_")
    temp[[1]][2]
  }

  extract_state <- function(filename) {
    temp <- strsplit(filename, "_")
    toupper(temp[[1]][1])
  }

  extract_year <- function(filename) {
    temp <- strsplit(filename, "_")
    temp2 <- temp[[1]][3]
    temp <- strsplit(temp2, "\\.")
    temp2 <- temp[[1]][1]
  }

  x <-
    list.files("data", pattern = "in_(general|primary).*\\.csv", full.names = TRUE) |> set_names(basename)

  save_option <- getOption("readr.show_col_types")
  options(readr.show_col_types = FALSE)

  results <-
    x  |> map(\(x) read_csv(x)) |>
    list_rbind(names_to = "filename") %>%
    rowwise() %>%
    mutate(Election = mapply(extract_election, filename)) %>%
    mutate(State = mapply(extract_state, filename)) %>%
    mutate(Year = mapply(extract_year, filename)) %>%
    mutate(Country = "USA") %>% mutate(
      across(Country, as.character),
      across(State, as.character),
      across(Year, as.integer),
      across(Office, as.character),
      across(Party, as.character),
      across(District, as.character),
      across(Candidate, as.character),
      across(Votes, as.double)
    )

  options(readr.show_col_types = save_option)

  results$filename <- NULL
  bind_rows(election_results, results)
}
