# =============================================================================
# 08_world_bank.R — pull World Bank population indicators for US, China, India
# Indicators:
#   SP.POP.TOTL              Total population
#   SP.POP.1564.TO.ZS        Working-age (15-64), % of total
#   SP.POP.65UP.TO.ZS        Population 65+, % of total
# =============================================================================

suppressPackageStartupMessages({
  library(wbstats)
  library(tidyverse)
  library(here)
})

proc_dir <- here("data", "processed")

countries <- c("USA", "CHN", "IND", "CAN")

wb <- wb_data(
  indicator = c(
    pop_total  = "SP.POP.TOTL",
    pct_15_64  = "SP.POP.1564.TO.ZS",
    pct_65_up  = "SP.POP.65UP.TO.ZS"
  ),
  country = countries,
  start_date = 2000, end_date = 2026
) %>%
  select(iso3c, country, year = date,
         pop_total, pct_working_age = pct_15_64, pct_seniors = pct_65_up)

write_csv(wb, file.path(proc_dir, "world_bank_pop.csv"))
message("Wrote: ", file.path(proc_dir, "world_bank_pop.csv"))
