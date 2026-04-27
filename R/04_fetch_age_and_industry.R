# =============================================================================
# 04_fetch_age_and_industry.R
#
# Pull additional data:
#   - BC Stats age cohorts 1971-2046 (Table 3 of projections xlsx)
#   - CANSIM 14-10-0355-01 — Employment by industry, monthly (BC, SA)
# =============================================================================

suppressPackageStartupMessages({
  library(cansim)
  library(readxl)
  library(tidyverse)
  library(lubridate)
  library(here)
})

raw_dir <- here("data", "raw")
proc_dir <- here("data", "processed")

cache_or_fetch <- function(table_id, raw_dir) {
  path <- file.path(raw_dir, paste0(gsub("-", "_", table_id), ".rds"))
  if (file.exists(path)) {
    message("Using cached: ", path)
    readRDS(path)
  } else {
    message("Fetching: ", table_id)
    df <- get_cansim(table_id)
    saveRDS(df, path)
    df
  }
}

# -----------------------------------------------------------------------------
# Age cohorts from BC Stats projections file
# -----------------------------------------------------------------------------
proj_path <- here("data", "raw", "temp",
                  "1971-table_2023_estimates_-_2024-2046_projections.xlsx")

t3_raw <- read_excel(proj_path, sheet = "Table 3", col_names = FALSE)

# Header is in row 6
hdr <- unlist(t3_raw[6, ]) %>% as.character()
# Build a clean dataframe with named columns
ages <- t3_raw %>%
  slice(-(1:6)) %>%
  set_names(hdr) %>%
  rename(status = Statistic, year = Year) %>%
  mutate(
    year = suppressWarnings(as.integer(year)),
    across(c(`0 to 14`, `15 to 24`, `25 to 44`, `45 to 64`, `65+`, `15 to 64`,
             Ages),
           ~ suppressWarnings(as.numeric(.)))
  ) %>%
  filter(!is.na(year), status %in% c("Estimate", "Projection")) %>%
  select(status, year,
         age_0_14 = `0 to 14`,
         age_15_24 = `15 to 24`,
         age_25_44 = `25 to 44`,
         age_45_64 = `45 to 64`,
         age_65p   = `65+`,
         age_15_64 = `15 to 64`,
         total_pop = Ages) %>%
  mutate(
    dep_youth = age_0_14 / age_15_64,
    dep_old   = age_65p  / age_15_64,
    dep_total = (age_0_14 + age_65p) / age_15_64,
    pct_working_age = age_15_64 / total_pop,
    pct_seniors = age_65p / total_pop
  )

write_csv(ages, file.path(proc_dir, "bc_age_cohorts.csv"))

# -----------------------------------------------------------------------------
# CANSIM 14-10-0355-01 — Employment by industry, monthly
# -----------------------------------------------------------------------------
ind_raw <- cache_or_fetch("14-10-0355-01", raw_dir)

# Inspect column names — Sex/Gender may be present
target_industries <- c(
  "Total employed, all industries",
  "Goods-producing sector",
  "Services-producing sector",
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

bc_ind <- ind_raw %>%
  filter(GEO == "British Columbia",
         `North American Industry Classification System (NAICS)`
           %in% target_industries,
         Statistics == "Estimate",
         `Data type` == "Seasonally adjusted") %>%
  mutate(date = ymd(paste0(REF_DATE, "-01"))) %>%
  filter(date >= ymd("2019-01-01")) %>%
  select(date,
         industry = `North American Industry Classification System (NAICS)`,
         employment_thousands = VALUE)

# Compute change from Jan 2024 to Mar 2026 (most recent) per industry
ind_change <- bc_ind %>%
  filter(date %in% c(ymd("2024-01-01"), max(date))) %>%
  pivot_wider(names_from = date, values_from = employment_thousands) %>%
  rename(emp_2024_01 = 2, emp_latest = 3) %>%
  mutate(
    abs_change_thousands = emp_latest - emp_2024_01,
    pct_change = (emp_latest - emp_2024_01) / emp_2024_01
  ) %>%
  arrange(pct_change)

write_csv(bc_ind,        file.path(proc_dir, "bc_industry_employment.csv"))
write_csv(ind_change,    file.path(proc_dir, "bc_industry_change_2024_to_latest.csv"))

message("Done. Files in: ", proc_dir)
