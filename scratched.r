
# in_primary2022 |>
#   filter(!is.na(County))

# in_primary2022 |>
#   select(Candidate, Office, Party, District) |>
#   mutate(gender = NA, .before=Candidate)

# need_gender <- in_primary2022 |>
#   select(Candidate, Office, Party, District) |>
#   mutate(gender = NA, .before=Candidate)

# gender2022 |>
#   filter(Office!="Convention Delegate") |>
#   filter(Office != "Precinct Committeeman") |>
#   count(Candidate, Office, District) |>
#   filter(n > 1)
# 

