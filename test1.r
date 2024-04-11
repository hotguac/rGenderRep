library(tidyverse)
library(ggplot2)

source(file = "sources.r")

test_files()

all_rows <- get_selection()
all_years <- unique(all_rows$Year)

for (data_year in 1:length(all_years)) {
  target_year <- all_years[data_year]
  print(paste("Generating for year ", target_year))

  year_data <- all_rows |> filter(Year == target_year)
  all_offices <- unique(year_data$Office)
  for (office_year in 1:length(all_offices)) {
    target_office <- all_offices[office_year]
    print(paste("Generating for office ", target_office))
    #
    # browser()
    #
    p <-
      generate_plot(.year = target_year,
                    .state = "IN",
                    .office = target_office)

    output_filename <-
      gsub(" ",
           "_",
           paste(target_year, "_IN_", target_office, ".png"))
    output_filename <- gsub("/", "_", output_filename)
    ggsave(paste("plots/", output_filename),
           plot = p, width = 7, height = 7)
  }
  rm(target_office)
  rm(output_filename)
  rm(office_year)
  rm(p)
  rm(all_offices)
  gc()
}

rm(target_year)
rm(data_year)
rm(year_data)
rm(all_rows)
rm(all_years)
gc()

rm(candidate_gender)
rm(election_results)
gc()
