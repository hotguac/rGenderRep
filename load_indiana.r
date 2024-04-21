library(tidyverse, warn.conflicts = FALSE)

# data source is
# https://indianavoters.in.gov/ENRHistorical/ElectionResults?year=2022#

hh_ss <- function (form = "%H:%M:%S") {
  cat(format(Sys.time(), format = form), "\n")
}

#------------------------------------------------------------------------
in_load_candidate_meta2 <- function(.data) {
  # primary data was exported and the gender (M/F) manually added
  # to the csv file
  candidate_gender <-
    tribble(~ Country, ~ State, ~ Year, ~ Sourced)

  print("Loading candidate meta data...")
  hh_ss()

  x <-
    list.files("data", pattern = "need_gender.*\\.csv", full.names = TRUE) |> set_names(basename)

  save_option <- getOption("readr.show_col_types")
  options(readr.show_col_types = FALSE)

  c_types = cols(
    Country = col_character(),
    State = col_character(),
    Year = col_integer(),
    Office = col_character(),
    Party = col_character(),
    District = col_character(),
    Candidate = col_character(),
    Gender = col_factor(c("M", "F", "O"))
  )

  results <-
    x  |> map(\(x) read_csv(x, col_types = c_types)) |>
    list_rbind() |>
    filter(!is.na(Gender))

  results$District <- gsub("District ", "", results$District)
  hh_ss()
  results <- results |>
    mutate(District = replace(District, District == "1", "01")) |>
    mutate(District = replace(District, District == "2", "03")) |>
    mutate(District = replace(District, District == "3", "03")) |>
    mutate(District = replace(District, District == "4", "04")) |>
    mutate(District = replace(District, District == "5", "05")) |>
    mutate(District = replace(District, District == "6", "06")) |>
    mutate(District = replace(District, District == "7", "07")) |>
    mutate(District = replace(District, District == "8", "08")) |>
    mutate(District = replace(District, District == "9", "09"))

  hh_ss()
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
    as.integer(temp2)
  }

  x <-
    list.files("data", pattern = "in_(general|primary).*\\.csv", full.names = TRUE) |> set_names(basename)

  save_option <- getOption("readr.show_col_types")
  options(readr.show_col_types = FALSE)

  print("Loading election results...")
  hh_ss()

  c_types = cols(
    County = col_character(),
    Office = col_character(),
    Party = col_character(),
    District = col_character(),
    Candidate = col_character(),
    Votes = col_integer()
  )

  results <-
    x |> map(\(x) read_csv(x, col_types = c_types))

  print("Doing list_rbind")
  hh_ss()

  results <- results |>
    list_rbind(names_to = "filename")

  print("Doing mutate for Election, State, and Year")
  hh_ss()

  results <- results %>%
    rowwise() %>%
    mutate(Election = mapply(extract_election, filename)) %>%
    mutate(State = mapply(extract_state, filename)) %>%
    mutate(Year = mapply(extract_year, filename))

  print("Setting country name")
  hh_ss()

  results$Country <- "USA"

  print("Doing gsub to remove District literal...")
  hh_ss()
  results$District <- gsub("District ", "", results$District)

  print("Doing mutates to add leading zero to single digit districts...")
  hh_ss()
  results <- results |> ungroup() |>
    mutate(District = replace(District, District == "1", "01")) |>
    mutate(District = replace(District, District == "2", "03")) |>
    mutate(District = replace(District, District == "3", "03")) |>
    mutate(District = replace(District, District == "4", "04")) |>
    mutate(District = replace(District, District == "5", "05")) |>
    mutate(District = replace(District, District == "6", "06")) |>
    mutate(District = replace(District, District == "7", "07")) |>
    mutate(District = replace(District, District == "8", "08")) |>
    mutate(District = replace(District, District == "9", "09"))
  hh_ss()

  options(readr.show_col_types = save_option)

  print("Removing filename...")
  results$filename <- NULL

  print("Binding to existing results...")
  bind_rows(election_results, results)
}
