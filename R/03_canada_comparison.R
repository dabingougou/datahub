# =============================================================================
# 03_canada_comparison.R — Canada-level data for BC vs Canada comparisons
#
# Reads the same .rds files cached by 01_fetch_bc_population.R and
# 02_fetch_age_and_industry.R, then filters to Canada totals.
# Outputs:
#   data/processed/canada_population_quarterly.csv
#   data/processed/canada_industry_change_2024_to_latest.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(here)
})

raw_dir <- here("data", "raw")
proc_dir <- here("data", "processed")

# Re-read cached pulls (run 01 and 02 first if these don't exist)
pop_q   <- readRDS(file.path(raw_dir, "17_10_0009_01.rds"))
ind_emp <- readRDS(file.path(raw_dir, "14_10_0355_01.rds"))

# ---------------------------------------------------------------------------
# Canada population, quarterly
# ---------------------------------------------------------------------------
canada_pop <- pop_q %>%
  filter(GEO == "Canada") %>%
  mutate(date = ymd(paste0(REF_DATE, "-01"))) %>%
  select(date, val = VALUE) %>%
  arrange(date)

write_csv(canada_pop, file.path(proc_dir, "canada_population_quarterly.csv"))

# ---------------------------------------------------------------------------
# Canada industry employment change, January 2024 to latest
# ---------------------------------------------------------------------------
target_industries <- c(
  "Total employed, all industries",
  "Construction",
  "Manufacturing",
  "Wholesale and retail trade",
  "Transportation and warehousing",
  "Accommodation and food services",
  "Health care and social assistance",
  "Educational services",
  "Professional, scientific and technical services",
  "Business, building and other support services",
  "Information, culture and recreation",
  "Other services (except public administration)",
  "Public administration",
  "Agriculture",
  "Forestry, fishing, mining, quarrying, oil and gas"
)

canada_ind <- ind_emp %>%
  filter(GEO == "Canada",
         `North American Industry Classification System (NAICS)`
           %in% target_industries,
         Statistics == "Estimate",
         `Data type` == "Seasonally adjusted") %>%
  mutate(date = ymd(paste0(REF_DATE, "-01"))) %>%
  filter(date %in% c(ymd("2024-01-01"), max(date))) %>%
  select(date,
         industry = `North American Industry Classification System (NAICS)`,
         emp = VALUE) %>%
  pivot_wider(names_from = date, values_from = emp) %>%
  rename(emp_2024_01 = 2, emp_latest = 3) %>%
  mutate(pct_change = (emp_latest - emp_2024_01) / emp_2024_01)

write_csv(canada_ind,
          file.path(proc_dir, "canada_industry_change_2024_to_latest.csv"))

message("Canada comparison data written to: ", proc_dir)
