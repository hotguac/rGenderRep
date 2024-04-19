library(tidyverse, warn.conflicts = FALSE)

# data source is
# https://indianavoters.in.gov/ENRHistorical/ElectionResults?year=2022#

#------------------------------------------------------------------------
in_load_candidate_meta2 <- function(.data) {
  # primary data was exported and the gender (M/F) manually added
  # to the csv file
  gender_levels <- c("M", "F", "O")

  candidate_gender <-
    tribble(~ Country, ~ State, ~ Year, ~ Sourced)

  print("Loading candidate meta data...")

  x <-
    list.files("data", pattern = "need_gender.*\\.csv", full.names = TRUE) |> set_names(basename)

  save_option <- getOption("readr.show_col_types")
  options(readr.show_col_types = FALSE)

  results <-
    x  |> map(\(x) read_csv(x, col_types = cols(Gender = col_factor(gender_levels)))) |>
    list_rbind() |>
    filter(!is.na(Gender)) |>
    mutate(
      across(Country, as.character),
      across(State, as.character),
      across(Year, as.integer),
      across(Office, as.character),
      across(Party, as.character),
      across(District, as.character),
      across(Candidate, as.character)
    )

  results$District <- gsub("District ", "", results$District)

  results <- results |>
    mutate(District = if_else(District == "1", "01", District)) |>
    mutate(District = if_else(District == "2", "02", District)) |>
    mutate(District = if_else(District == "3", "03", District)) |>
    mutate(District = if_else(District == "4", "04", District)) |>
    mutate(District = if_else(District == "5", "05", District)) |>
    mutate(District = if_else(District == "6", "06", District)) |>
    mutate(District = if_else(District == "7", "07", District)) |>
    mutate(District = if_else(District == "8", "08", District)) |>
    mutate(District = if_else(District == "9", "09", District))

  options(readr.show_col_types = save_option)

  results
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

  print("Loading election results...")

  results <-
    x |> map(\(x) read_csv(x))

  print("Doing list_rbind")

  results <- results |>
    list_rbind(names_to = "filename")

  print("Doing mutate for Election, State, and Year")

  results <- results %>%
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

  print("Doing gsub to remove District literal...")
  results$District <- gsub("District ", "", results$District)

  print("Doing mutates to add leading zero to single digit districts...")
  results <- results |>
    mutate(District = if_else(District == "1", "01", District)) |>
    mutate(District = if_else(District == "2", "02", District)) |>
    mutate(District = if_else(District == "3", "03", District)) |>
    mutate(District = if_else(District == "4", "04", District)) |>
    mutate(District = if_else(District == "5", "05", District)) |>
    mutate(District = if_else(District == "6", "06", District)) |>
    mutate(District = if_else(District == "7", "07", District)) |>
    mutate(District = if_else(District == "8", "08", District)) |>
    mutate(District = if_else(District == "9", "09", District))

  options(readr.show_col_types = save_option)

  print("Removing filename...")
  results$filename <- NULL

  print("Binding to existing results...")
  bind_rows(election_results, results)
}
