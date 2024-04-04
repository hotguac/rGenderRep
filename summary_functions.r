get_selection <-
  function(.election = "ALL",
           .country = "ALL",
           .year = "ALL",
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
    
    if (.office != "ALL") {
      x <- filter(x, Office == .office)
    }
    
    if (.district != "ALL") {
      x <- filter(x, District == .district)
    }
    
    y <-
      left_join(x,
                candidate_gender)
    
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

get_elected <- function() {
  get_selection(.election = "General") |>
    group_by(Country, State, Year, District, Office) |>
    filter(Votes == max(Votes))
}

#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
get_gender_count <- function(.election, .gender, .office) {
  (get_selection(
    .election = .election,
    .gender = .gender,
    .office = .office
  ) |> count())$n
}

#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
get_missing_gender <- function(.election, .office) {
  get_selection(
    .election = .election,
    .office = .office
  ) |> filter(is.na(Gender))
}

#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
add_gender_rows <- function(.data, .cat, .m, .f, .o, .u) {
  .data <- add_row(
    .data,
    Category = .cat,
    Gender = "Women",
    Seats = .f
  )
  
  .data <- add_row(
    .data,
    Category = .cat,
    Gender = "Men",
    Seats = .m
  )
  .data <- add_row(
    .data,
    Category = .cat,
    Gender = "Other",
    Seats = .o
  )
  
  .data <- add_row(
    .data,
    Category = .cat,
    Gender = "Unknown",
    Seats = .u
  )
}

#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
summarize_gender <- function(.office) {
  gender_summary <- tribble( ~ Category, ~ Gender, ~ Seats)
  
  election <- "Primary"
  m <- get_gender_count(election, "M", .office)
  f <- get_gender_count(election, "F", .office)
  o <- get_gender_count(election, "O", .office)
  u <- get_gender_count(election, "ALL", .office) - (m + f + o)
  
  gender_summary <- add_gender_rows(gender_summary, election, m, f, o, u)
  
  election <- "General"
  m <- get_gender_count(election, "M", .office)
  f <- get_gender_count(election, "F", .office)
  o <- get_gender_count(election, "O", .office)
  u <- get_gender_count(election, "ALL", .office) - (m + f + o)
  
  gender_summary <- add_gender_rows(gender_summary, election, m, f, o, u)
  
  election <- "Elected"
  m <- get_gender_count(election, "M", .office)
  f <- get_gender_count(election, "F", .office)
  o <- get_gender_count(election, "O", .office)
  u <- get_gender_count(election, "ALL", .office) - (m + f + o)
  
  gender_summary <- add_gender_rows(gender_summary, election, m, f, o, u)
  
  gender_levels <- c("Unknown", "Other", "Women", "Men")
  gender_summary$Gender <-
    factor(gender_summary$Gender, levels = gender_levels)
  
  #|>
  # group_by(Gender) |> mutate(label_y = cumsum(Seats) - 0.5 * Seats)
  
  gender_summary
}


