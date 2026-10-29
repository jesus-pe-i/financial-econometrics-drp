library(ggplot2)

# Output directory
OUT <- "figures/01"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# Generate plots
p1 <- ggplot(economics, aes(date, unemploy)) +
  geom_line() +
  theme_minimal()

# Save figures
saveRDS(p1, file.path(OUT, "example.rds"))