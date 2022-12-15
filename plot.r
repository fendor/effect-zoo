library("ggplot2")
options(scipen = 20)

plot_by_scenario <- function(data) {
  ggplot(data,
    aes(x = Scenario, y = Mean, fill = Implementation),
    environment = environment()
  ) +
    geom_bar(
      stat = "identity",
      colour = "black",
      position = position_dodge()
    ) +
    geom_errorbar(aes(ymin = MeanL, ymax = MeanU),
      position = position_dodge(0.9),
      width = 0.2
    ) +
    facet_wrap(~Scenario, scales = "free") +
    labs(y = "Execution time (s)") +
    theme_linedraw() +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

plot_by_implementation <- function(data) {
  ggplot(data, aes(x = Implementation, y = Mean, fill = Scenario)) +
    geom_bar(
      stat = "identity",
      colour = "black",
      position = position_dodge()
    ) +
    geom_errorbar(aes(ymin = MeanL, ymax = MeanU),
      position = position_dodge(0.9),
      width = 0.2
    ) +
    theme_linedraw() +
    facet_wrap(~Implementation, scales = "free") +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(y = "Execution time (s)")
}

load_data <- function(file) {
  data <-
    read.csv(
      file,
      header = TRUE,
      col.names = c(
        "Implementation",
        "Mean", "MeanL", "MeanU", "Stddev", "StddevLB", "StddevUB"
      )
    )
  # data$Scenario <- as.character(data$Implementation)
  # data$Scenario <-
  #   factor(data$Scenario, level = unique(data$Scenario))
  data
}


big_stack <- load_data("big-stack.csv")
plot_by_scenario(big_stack) +
  labs(title = "BigStack (by scenario)")
plot_by_implementation(big_stack) +
  labs(title = "BigStack (by implementation)")

countdown <- load_data("countdown.csv")
plot_by_scenario(countdown) +
  labs(title = "Countdown (by n)", x = "n")
plot_by_implementation(countdown) +
  labs(title = "Countdown (by implementation)")

file_sizes <- load_data("file-sizes.csv")
plot_by_scenario(file_sizes) +
  labs(title = "FileSizes (by scenario)")
plot_by_implementation(file_sizes) +
  labs(title = "FileSizes (by implementation)")

reinterpretation <- load_data("reinterpretation.csv")
plot_by_scenario(reinterpretation) +
  labs(title = "Reinterpretation (by scenario)")
plot_by_implementation(reinterpretation) +
  labs(title = "Reinterpretation (by implementation)")
