#' Analysis of candidates in elections
#'
#' This is a system for analyzing elections
#'
#'
#'
library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(ggplot2)

source(file = "sources.r")

#------------------------------------------------------------------------
init_elections <- function() {
  tribble(~ Country,
          ~ State,
          ~ Year,
          ~ Office,
          ~ Party,
          ~ District,
          ~ Candidate,
          ~ Votes) %>% mutate(
            across(Country, as.character),
            across(State, as.character),
            across(Year, as.integer),
            across(Office, as.character),
            across(Party, as.character),
            across(District, as.character),
            across(Candidate, as.character),
            across(Votes, as.double)
          )
}

#------------------------------------------------------------------------
init_meta <- function() {
  meta <-
    tribble(~ Country,
            ~ State,
            ~ Year,
            ~ Office,
            ~ Party,
            ~ District,
            ~ Candidate,
            ~ Gender,
            ~ Race,
            ~ Age) %>% mutate(
              across(Country, as.character),
              across(State, as.character),
              across(Year, as.integer),
              across(Office, as.character),
              across(Party, as.character),
              across(District, as.character),
              across(Candidate, as.character),
              across(Gender, as.character),
              across(Race, as.character),
              across(Age, as.integer)
            )
}

#------------------------------------------------------------------------
main <- function() {
  election_results <- init_elections() |> in_load_elections2() |>
    #------------------------------------------------------------------------
  # To speed up testing let's filter data a little
  # Comment out when we're ready for all data (including pipe above)
  filter((
    Office == "US Senator" |
      Office == "US Representative" |
      Office == "State Senator" | Office == "State Representative"
  )
  )
  #
  # End test filter
  #------------------------------------------------------------------------

  candidate_meta <- in_load_candidate_meta2() |> distinct()

  print("Combining data and meta data...")
  all_rows <- combine_meta(election_results, candidate_meta)
  all_years <- unique(all_rows$Year)

  print("Go time!")

  for (data_year in 1:length(all_years)) {
    target_year <- all_years[data_year]
    print(paste("Generating for year ", target_year))

    year_data <- all_rows |> filter(Year == target_year)
    all_offices <- unique(year_data$Office)
    for (office_year in 1:length(all_offices)) {
      target_office <- all_offices[office_year]

      p <-
        generate_plot(all_rows,
                      .year = target_year,
                      .state = "IN",
                      .office = target_office)

      output_filename <-
        gsub(" ",
             "_",
             paste(target_year, "_IN_", target_office, ".png"))
      output_filename <- gsub("/", "_", output_filename)
      ggsave(
        paste("plots/", output_filename),
        plot = p,
        width = 7,
        height = 7
      )
    }
  }

  write_csv(all_rows |> get_missing_gender(),
            "~/LWV/output/missing_gender.csv",
            na = "")
}
