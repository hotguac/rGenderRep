#------------------------------------------------------------------------
#
#------------------------------------------------------------------------
get_selection <-
  function(.data,
           .meta_data,
           .country = "ALL",
           .state = "ALL",
           .year = "ALL",
           .election = "ALL",
           .office = "ALL",
           .district = "ALL",
           .gender = "ALL") {
    #
    if (.election == "ALL") {
      x <- .data
    } else {
      if (.election == "Elected") {
        x <- get_elected(.data, .meta_data)
      } else {
        x <- filter(.data, Election == .election)
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
                .meta_data,
                by = join_by(Country, State, Year, Candidate, Office, Party, District))

    if (.gender != "ALL") {
      x <- filter(y, Gender == .gender)
    } else {
      x <- y
    }

    if (is.grouped_df(x)) {
      x <- ungroup(x)
    } else
      x
  }

#------------------------------------------------------------------------
#
#------------------------------------------------------------------------
# get_elected <- function(.data, .meta_data) {
#   t <- get_selection(.data, .meta_data, .election = "general") |>
#     group_by(Country, State, Year, District, Office) |>
#     filter(Votes == max(Votes))
#   # remove the Gender column so it won't get duplicated later
#   t$Gender <- NULL
#   t
# }

#-----------------------------------------------------------------------
#
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
#  write_csv(get_missing_gender(.election = "General", .office = "State Senator"),
#             file = "data/missing1.csv", na = "")
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
summarize_gender <- function(.data, .year, .state, .office) {
  gender_summary <- tribble(~ Category, ~ Gender, ~ Seats)

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

  gender_levels <- c("Unknown", "Other", "Women", "Men")
  gender_summary$Gender <-
    factor(gender_summary$Gender, levels = gender_levels)

  gender_summary
}
