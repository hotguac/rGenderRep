source(file = "sources.r")

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

main <- function() {
  election_results <- init_elections() |> in_load_elections3() |> in_load_elections2() |>
    #------------------------------------------------------------------------
  # To speed up testing let's filter data a little |>
  # Comment out when we're ready for all data (including pipe above)
  filter((
    Office == "US Senator" |
      Office == "US Representative" |
      Office == "State Senator" | Office == "State Representative"
  )
  ) |> filter(Year >= 2016)
  #
  # End test filter
  #------------------------------------------------------------------------

  candidate_meta <- in_load_candidate_meta2() |> distinct()

  print("Combining data and meta data...")
  all_rows <- combine_meta(election_results, candidate_meta)

  # test
  all_rows <- propagate_meta(all_rows, candidate_meta)
  # test

  t <- all_rows |>
    pivot_wider(names_from = Election, values_from = Votes) |>
    group_by(Country, State, Year, Office, District) |>
    mutate(MaxVotes = max(general)) |>
    arrange(Country, State, Year, Office, District, desc(general)) |>
    mutate(Elected = (general == MaxVotes))

  t$MaxVotes = NULL
  t$Elected <- if_else(is.na(t$Elected), FALSE, t$Elected)

  with_winners <- ungroup(t)

  zz <-  with_winners |>
    mutate(Party = if_else(
      ((Party != "Republican") & (Party != "Democratic")), "Other", Party))

  zz |> arrange(Gender, Party)
}

results <- main()

t <- filter(results, Country == "USA", State == "IN") |>
  mutate(Year = factor(Year)) |>
  select(Year, Party, Gender) |> count(Year, Party, Gender)





t$combined <- paste(t$Gender, t$Party)

combine_levels <- c("F Other", "F Democratic", "F Republican", "M Republican", "M Democratic", "M Other")

t$combined <-
  factor(t$combined, levels = combine_levels)



p <- ggplot(t, aes(x=Year, y = n, fill=combined)) + geom_col()
q <- ggplot(t, aes(x=combined, y = n, fill=Year)) + geom_col()

u <- ddply(t, "combined", summarise, total = sum(n)) |> select(combined, total)
u$combined <- factor(u$combined, levels = c('M Republican', 'M Democratic', 'F Democratic', 'F Republican', 'M Other', 'F Other'))
u <- arrange(u, combined)

r <- waffle(u, rows = 35, title = "2016-2022 Indiana US And State Legislator Candidates")

print("Change all democratic to democrat?")

write_csv(results |> get_missing_gender(),
          "~/LWV/output/missing_gender.csv",
          na = "")
