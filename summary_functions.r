get_selection <-
  function(.country = "ALL",
           .state = "ALL",
           .year = "ALL",
           .election = "ALL",
           .office = "ALL",
           .district = "ALL",
           .gender = "ALL") {
    #
    if (.election == "ALL") {
      x <- election_results
    } else {
      if (.election == "Elected") {
        x <- get_elected()
      } else {
        x <- filter(election_results, Election == .election)
      }
    }

    if (.country != "ALL") {
      x <- filter(x, Country == .country)
    }
    if (.year != "ALL") {
      x <- filter(x, Year == .year)
    }

    if (.state != "ALL") {
      x <- filter(x, State == .state)
    }

    if (.office != "ALL") {
      x <- filter(x, Office == .office)
    }

    if (.district != "ALL") {
      x <- filter(x, District == .district)
    }

    y <-
      left_join(x,
                candidate_gender,
                by = join_by(Country, State, Year, Candidate, Office, Party, District))

    if (.gender != "ALL") {
      #print(y)
      x <- filter(y, Gender == .gender)
    } else {
      x <- y
    }

    if (is.grouped_df(x)) {
      x <- ungroup(x)
    } else
      x
  }

get_elected <- function() {
  t <- get_selection(.election = "General") |>
    group_by(Country, State, Year, District, Office) |>
    filter(Votes == max(Votes))
  # remove the Gender column so it won't get duplicated later
  t$Gender <- NULL
  t
}

#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
get_gender_count <-
  function(.year, .state, .election, .gender, .office) {
    (
      get_selection(
        .year = .year,
        .state = .state,
        .election = .election,
        .gender = .gender,
        .office = .office
      ) |> count()
    )$n
  }

#-----------------------------------------------------------------------
#  write_csv(get_missing_gender(.election = "General", .office = "State Senator"),file = "data/missing1.csv", na = "")
#-----------------------------------------------------------------------
get_missing_gender <- function(.election = "ALL", .office = "ALL") {
  get_selection(.election = .election,
                .office = .office) |> filter(is.na(Gender)) |> select(c(
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
#
#-----------------------------------------------------------------------
add_gender_rows <- function(.data, .cat, .m, .f, .o, .u) {
  .data <- add_row(.data,
                   Category = .cat,
                   Gender = "Women",
                   Seats = .f)

  .data <- add_row(.data,
                   Category = .cat,
                   Gender = "Men",
                   Seats = .m)
  .data <- add_row(.data,
                   Category = .cat,
                   Gender = "Other",
                   Seats = .o)

  .data <- add_row(.data,
                   Category = .cat,
                   Gender = "Unknown",
                   Seats = .u)
}

#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
summarize_gender <- function(.year, .state, .office) {
  gender_summary <- tribble( ~ Category, ~ Gender, ~ Seats)

  election <- "Primary"
  m <- get_gender_count(.year, .state, election, "M", .office)
  f <- get_gender_count(.year, .state, election, "F", .office)
  o <- get_gender_count(.year, .state, election, "O", .office)
  u <-
    get_gender_count(.year, .state, election, "ALL", .office) - (m + f + o)

  gender_summary <-
    add_gender_rows(gender_summary, election, m, f, o, u)

  election <- "General"
  m <- get_gender_count(.year, .state, election, "M", .office)
  f <- get_gender_count(.year, .state, election, "F", .office)
  o <- get_gender_count(.year, .state, election, "O", .office)
  u <-
    get_gender_count(.year, .state, election, "ALL", .office) - (m + f + o)

  gender_summary <-
    add_gender_rows(gender_summary, election, m, f, o, u)

  election <- "Elected"
  m <- get_gender_count(.year, .state, election, "M", .office)
  f <- get_gender_count(.year, .state, election, "F", .office)
  o <- get_gender_count(.year, .state, election, "O", .office)
  u <-
    get_gender_count(.year, .state, election, "ALL", .office) - (m + f + o)

  gender_summary <-
    add_gender_rows(gender_summary, election, m, f, o, u)

  gender_levels <- c("Unknown", "Other", "Women", "Men")
  gender_summary$Gender <-
    factor(gender_summary$Gender, levels = gender_levels)

  #|>
  # group_by(Gender) |> mutate(label_y = cumsum(Seats) - 0.5 * Seats)

  gender_summary
}
