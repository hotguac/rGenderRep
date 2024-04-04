library(tidyverse)
library(ggplot2)
library(svglite)


#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
generate_plot <- function(.office) {
  gender_summary <- summarize_gender(.office)
  
  write_excel_csv(gender_summary,
                  file = paste("LWV/gender_Summary", .office, ".csv"))
  
  pos = c(0.2, 0.4, 0.6, 0.8)
  
  # current
  my_plot <-
    ggplot(data = gender_summary,
           aes(x = Category, y = Seats, fill = Gender)) +
    geom_col(position = "fill", colour = "black") +
    labs(x = .office, y = "Percent") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = 4) +
    theme(plot.background = element_rect(fill = 'transparent', colour = NA))
  #    geom_text(aes(label=Gender, y = 0.5), size = 3 )
  
  # with values on bars
  new_data <- my_plot$data |>
    filter(Seats > 0) |>
    mutate(Category = factor(Category)) |>
    group_by(Category, Gender) |>
    summarise(n = sum(Seats, na.rm = TRUE)) |>
    mutate(pct = prop.table(n))
  
  new_data |>
    ggplot(aes(pct, Category, fill = Gender)) +
    geom_col() +
    geom_text(aes(label = scales::percent(pct, accuracy = .1)),
              position = position_stack(vjust = .5)) +
    labs(y = .office, x = "Percent")
  
  new_data |>
    ggplot(aes(Category, pct, fill = Gender)) +
    geom_col() +
    geom_text(aes(label = scales::percent(pct, accuracy = .1)),
              position = position_stack(vjust = .5)) +
    labs(y = .office, x = "Percent")
  
  
}
