library(tidyverse)
library(ggplot2)
library(svglite)


#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
generate_plot <- function(.year, .state, .office) {
  gender_summary <- summarize_gender(.year = .year, .state = .state, .office = .office)

  write_excel_csv(gender_summary,
                  file = paste("output/gender_Summary_", .office, ".csv"))

  # with values on bars
  new_data <- gender_summary |>
    filter(Seats > 0) |>
    mutate(Category = factor(Category)) |>
    group_by(Category, Gender) |>
    summarise(n = sum(Seats, na.rm = TRUE)) |>
    mutate(pct = prop.table(n))

  new_data |>
    ggplot(aes(Category, pct, fill = Gender)) +
    geom_col() +
    geom_text(aes(label = scales::percent(pct, accuracy = .1)),
              position = position_stack(vjust = .5)) +
    scale_fill_brewer(palette = "Pastel2") +
    theme(
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 16, hjust = 0.5)
    )  +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = .office, y = "Percent", title = "2022 Indiana")
}
