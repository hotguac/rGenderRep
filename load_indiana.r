
# data source is
# https://indianavoters.in.gov/ENRHistorical/ElectionResults?year=2022#

hh_ss <- function (form = "%H:%M:%S") {
  cat(format(Sys.time(), format = form), "\n")
}

reformat_district <- function(districts) {
  print("Standardizing the District field")
  districts <- gsub("State Representative, District ", "", districts)
  districts <- gsub("United States Representative, ", "", districts)
  districts <- gsub("State Senator, District ", "", districts)
  districts <- gsub("United States Senator From Indiana", "statewide", districts)
  districts <- gsub("First District", "1", districts)
  districts <- gsub("Second District", "2", districts)
  districts <- gsub("Third District", "3", districts)
  districts <- gsub("Fourth District", "4", districts)
  districts <- gsub("Fifth District", "5", districts)
  districts <- gsub("Sixth District", "6", districts)
  districts <- gsub("Seventh District", "7", districts)
  districts <- gsub("Eighth District", "8", districts)
  districts <- gsub("Ninth District", "9", districts)
  districts <- gsub("1, 2, 3, 4, 5, 6, 7, 8, 9", "statewide", districts)
  districts <- gsub("1,2,3,4,5,6,7,8,9", "statewide", districts)
  districts <- gsub("District ", "", districts)
  districts <- sub("^0+", "", districts)

  districts
}

#------------------------------------------------------------------------
in_load_candidate_meta2 <- function(.data) {
  # primary data was exported and the gender (M/F) manually added
  # to the csv file
  candidate_gender <-
    tribble( ~ Country, ~ State, ~ Year, ~ Sourced)

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

  results$District <- reformat_district(results$District)

  options(readr.show_col_types = save_option)

  results
}

#------------------------------------------------------------------------
#------------------------------------------------------------------------
in_load_elections3 <- function(election_results) {
  x <-
    list.files("data", pattern = "in-x_(general|primary).*\\.csv", full.names = TRUE) |> set_names(basename)

  save_option <- getOption("readr.show_col_types")
  options(readr.show_col_types = FALSE)

  print("Loading election results...")
  hh_ss()

  c_types = cols(
    Election = col_character(),
    JurisdictionName = col_character(),
    ReportingCountyName = col_character(),
    DataEntryJurisdictionName = col_character(),
    DataEntryLevelName = col_character(),
    Office = col_character(),
    OfficeCategory = col_character(),
    BallotOrder = col_integer(),
    NameonBallot = col_character(),
    PoliticalParty = col_character(),
    Winner = col_character(),
    NumberofOfficeSeats = col_integer(),
    TotalVotes = col_integer()
  )

  results <-
    x |> map(\(x) read_csv(x, col_types = c_types)) |>
    list_rbind(names_to = "filename")


  print("Doing mutate for Election, State, and Year")

  results <-
    results |> separate_wider_delim(filename,
                                    delim = "_",
                                    names = c("State", "Election2", "TheRest"))
  results <-
    results |> separate_wider_delim(TheRest, delim = ".", names = c("Year", "ext")) |> mutate(Year = parse_integer(Year))
  results$ext <- NULL

  print("Setting country name")

  results$Country <- "USA"
  #results$District <- gsub("District ", "", results$District)
  results$State <- toupper(results$State)
  results$State <- gsub("-X", "", results$State)
  results <- results |>
    rename(District = Office,
           SaveElection = Election) |>
    rename(Office = OfficeCategory,
           Election = Election2,
           Party = PoliticalParty,
           Candidate = NameonBallot,
           Votes = TotalVotes,
           County = ReportingCountyName) |>
    select(Country, State, Year, Office, Party, District, District, Candidate, Votes, Election, County)

  results <- results |> group_by(Country, State, Year, Office, Party, District, Candidate, Election) |>
    summarize(Votes = sum(Votes))

  results$county <- NA

  print("Need to standardize the District, maybe using additional columns")

  results$District <- reformat_district(results$District)
  results |> ungroup()
}

#------------------------------------------------------------------------
#------------------------------------------------------------------------
in_load_elections2 <- function(election_results) {
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
    x |> map(\(x) read_csv(x, col_types = c_types)) |>
    list_rbind(names_to = "filename")

  print("Doing mutate for Election, State, and Year")

  results <-
    results |> separate_wider_delim(filename,
                                    delim = "_",
                                    names = c("State", "Election", "TheRest"))
  results <-
    results |> separate_wider_delim(TheRest, delim = ".", names = c("Year", "ext")) |> mutate(Year = parse_integer(Year))
  results$ext <- NULL

  print("Setting country name")

  results$Country <- "USA"

  print("Doing gsub to remove District literal...")

  results$District <- reformat_district(results$District)

  results$State <- toupper(results$State)

  print("Doing mutates to add leading zero to single digit districts...")

  results <- results |> ungroup()

  options(readr.show_col_types = save_option)

  print("Binding to existing results...")
  z <- bind_rows(election_results, results)
  hh_ss()

  z
}

propagate_meta <- function(data, meta_data) {
  results <- data |> filter(!is.na(Gender))

  x <- data |> filter(is.na(Gender))
  x$Gender <- NULL
  #print(x)

  y <-
    meta_data |> select(Country, State, Party, Candidate, Gender) |> distinct()
  #print(y)

  z <- left_join(x, y)
  write_csv(
    z |> select(
      Country,
      State,
      Year,
      Gender,
      Candidate,
      Office,
      Party,
      District
    ) |> filter(!is.na(Gender)),
    "output/propagated_gender.csv",
    na = ""
  )

  bind_rows(results, z)
}
