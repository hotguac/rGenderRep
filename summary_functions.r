#------------------------------------------------------------------------
combine_meta <-
  function(.data,
           .meta_data) {
    y <-
      left_join(.data,
                .meta_data,
                by = join_by(Country, State, Year, Candidate, Office, Party, District))

    if (is.grouped_df(y)) {
      y <- ungroup(y)
    }

    y
  }

#-----------------------------------------------------------------------
get_gender_count <-
  function(.data, .gender) {
    tt <- ungroup(.data)
    if (.gender == "ALL") {
      result <- (tt |> count())$n
    } else {
      result <- (tt |> filter(Gender == .gender) |> count())$n
    }
    result
  }

#-----------------------------------------------------------------------
get_missing_gender <- function(.data,
                               .election = "ALL",
                               .office = "ALL") {
  .data |> filter(is.na(Gender)) |> select(c(
    Country,
    State,
    Year,
    Gender,
    Candidate,
    Office,
    Party,
    District
  ))
}

#-----------------------------------------------------------------------
add_gender_rows <- function(.data, .cat, .m, .f, .o, .u) {
  .data |> add_row(Category = .cat,
                   Gender = "Women",
                   Seats = .f) |>
    add_row(Category = .cat,
            Gender = "Men",
            Seats = .m) |>
    add_row(Category = .cat,
            Gender = "Other",
            Seats = .o) |>
    add_row(Category = .cat,
            Gender = "Unknown",
            Seats = .u)
}

#-----------------------------------------------------------------------
summarize_gender <- function(.data, .year, .state, .office) {
  gender_summary <- tribble(~ Category, ~ Gender, ~ Seats)

  #--
  election <- "primary"
  election_data <-
    .data |> filter(Election == election,
                    Year == .year,
                    State == .state,
                    Office == .office)
  m <- get_gender_count(election_data, "M")
  f <- get_gender_count(election_data, "F")
  o <- get_gender_count(election_data, "O")
  u <-
    get_gender_count(election_data, "ALL") - (m + f + o)

  gender_summary <-
    add_gender_rows(gender_summary, election, m, f, o, u)

  #--
  election <- "general"
  election_data <-
    .data |> filter(Election == election,
                    Year == .year,
                    State == .state,
                    Office == .office)
  m <- get_gender_count(election_data, "M")
  f <- get_gender_count(election_data, "F")
  o <- get_gender_count(election_data, "O")
  u <-
    get_gender_count(election_data, "ALL") - (m + f + o)

  gender_summary <-
    add_gender_rows(gender_summary, election, m, f, o, u)

  #--
  election <- "Elected"

  election_data <-
    .data |> filter(Election == "general",
                    Year == .year,
                    State == .state,
                    Office == .office,
                    Votes > 0) |>
    group_by(Country, State, Year, District, Office)

  if (sum(election_data$Votes) > 0) {
    election_data <- election_data |>
      filter(Votes == max(Votes))
  }

  m <- get_gender_count(election_data, "M")
  f <- get_gender_count(election_data, "F")
  o <- get_gender_count(election_data, "O")
  u <-
    get_gender_count(election_data, "ALL") - (m + f + o)

  gender_summary <-
    add_gender_rows(gender_summary, election, m, f, o, u)

  #--
  gender_levels <- c("Unknown", "Other", "Women", "Men")
  gender_summary$Gender <-
    factor(gender_summary$Gender, levels = gender_levels)

  gender_summary
}
