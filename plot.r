library("ggplot2")
library("tidyr")
library("dplyr")
library("xtable")
options(scipen = 20)

plot_by_scenario <- function(data) {
  ggplot(data,
    aes(x = Size, y = Mean, fill = Name)
  ) +
    geom_bar(
      stat = "identity",
      position = "dodge"
    ) +

    geom_errorbar(aes(ymin = MeanL, ymax = MeanU),
                  position = position_dodge(0.9),
                  width = 0.2) +
    facet_wrap( ~ Size, scales = "free") +
    scale_y_continuous() +
    labs(y = "Execution time (μs)") +
    theme_linedraw() +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

plot_by_implementation <- function(data) {
  ggplot(data, aes(x = Implementation, y = Mean)) +
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
    labs(y = "Execution time (μs)")
}

load_data <- function(file) {
  df <-
    read.csv(
      file,
      header = TRUE,
      col.names = c(
        "Implementation",
        "Mean", "MeanL", "MeanU", "Stddev", "StddevLB", "StddevUB"
      )

    )
  for (column in c("Mean", "MeanL", "MeanU", "Stddev", "StddevLB", "StddevUB")) {
    df[column] = df[column] * 1e6 # Turn seconds into microseconds
  }
  df2 <- separate(data = df, col = Implementation, into = c("Name", "Size"), sep = "/")
  df3 <- filter(df2, Name != "simple-effects")
  df3
}

print_latex <- function(df) {
  # Only take a subset of columns
  df2 <- df[c("Name", "Size", "Mean", "Stddev")]
  df2 <- df2[order(df2$Size),]
  x <- xtable(df2, digits = 3)
  align(x) <- xalign(x)
  display(x) <- xdisplay(x)
  print(x, include.rownames = FALSE)
}

countdown <- load_data("countdown.csv")
plot_by_scenario(countdown) +
  labs(title = "Countdown (by n)", x = "n")
# plot_by_implementation(countdown) +
#   labs(title = "Countdown (by implementation)")
ggsave("countdown_scenario.png", width=1920, units="px")
print_latex(countdown)

big_stack <- load_data("big-stack.csv")
plot_by_scenario(big_stack) +
  labs(title = "BigStack (by scenario)")
ggsave("bigstack_scenario.png", width=1920, units="px")
print_latex(big_stack)

big_stack2 <- filter(big_stack, Name %in% c("freer-simple", "mtl"))
plot_by_scenario(big_stack2) +
  labs(title = "BigStack without preff (by scenario)")
ggsave("bigstack_scenario2.png", width=1920, units="px")
# plot_by_implementation(big_stack) +
#   labs(title = "BigStack (by implementation)")

file_sizes <- load_data("file-sizes.csv")
plot_by_scenario(file_sizes) +
  labs(title = "FileSizes (by scenario)")
# plot_by_implementation(file_sizes) +
#   labs(title = "FileSizes (by implementation)")
ggsave("file-sizes_scenario.png", width=1920, units="px")
print_latex(file_sizes)

reinterpretation <- load_data("reinterpretation.csv")
plot_by_scenario(reinterpretation) +
  labs(title = "Reinterpretation (by scenario)")
# plot_by_implementation(reinterpretation) +
#   labs(title = "Reinterpretation (by implementation)")
ggsave("reinterpretation_scenario.png", width=1920, units="px")
print_latex(reinterpretation)

