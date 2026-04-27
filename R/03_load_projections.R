# =============================================================================
# 03_load_projections.R — load BC Stats annual estimates + projections to 2046
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(here)
})

proc_dir <- here("data", "processed")
path <- here("data", "raw", "temp",
             "1971-table_2023_estimates_-_2024-2046_projections.xlsx")

raw <- read_excel(path, sheet = "Table 1", col_names = FALSE, skip = 5)

projections <- raw %>%
  rename(status = `...1`, year = `...2`, total_pop = `...3`,
         median_age = `...4`, sex_ratio = `...5`) %>%
  select(status, year, total_pop, median_age, sex_ratio) %>%
  mutate(
    year = suppressWarnings(as.integer(year)),
    total_pop = suppressWarnings(as.numeric(total_pop)),
    median_age = suppressWarnings(as.numeric(median_age))
  ) %>%
  filter(!is.na(year), status %in% c("Estimate", "Projection"))

write_csv(projections, file.path(proc_dir, "bc_stats_projections.csv"))
message("Wrote: ", file.path(proc_dir, "bc_stats_projections.csv"))
