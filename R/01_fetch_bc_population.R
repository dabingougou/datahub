# =============================================================================
# 01_fetch_bc_population.R
#
# Pull BC quarterly population and the components of demographic growth from
# Statistics Canada (NDM tables) via the `cansim` package.
#
# Output: data/raw/*.rds (cached pulls), data/processed/*.csv (analysis tables)
# =============================================================================

suppressPackageStartupMessages({
  library(cansim)
  library(tidyverse)
  library(lubridate)
  library(here)
})

# Paths -----------------------------------------------------------------------
raw_dir <- here("data", "raw")
proc_dir <- here("data", "processed")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)

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
# Table 17-10-0009-01 — Quarterly population estimates by province
# -----------------------------------------------------------------------------
pop_quarterly <- cache_or_fetch("17-10-0009-01", raw_dir)

bc_pop <- pop_quarterly %>%
  filter(GEO == "British Columbia") %>%
  mutate(date = ymd(paste0(REF_DATE, "-01"))) %>%
  select(date, REF_DATE, GEO, val_norm = VALUE) %>%
  arrange(date)

# -----------------------------------------------------------------------------
# Table 17-10-0008-01 — Components of demographic growth (annual)
#   REF_DATE format: "YYYY/YYYY" representing the demographic year (Jul–Jun)
# -----------------------------------------------------------------------------
components_annual <- cache_or_fetch("17-10-0008-01", raw_dir)

bc_components <- components_annual %>%
  filter(GEO == "British Columbia") %>%
  mutate(year_end = as.integer(substr(REF_DATE, nchar(REF_DATE) - 3,
                                       nchar(REF_DATE)))) %>%
  select(REF_DATE, year_end, component = `Components of population growth`,
         val = VALUE) %>%
  filter(year_end >= 2020, year_end <= 2025)

# Aggregate components into the four buckets used in the public article:
#   Natural increase   = Births - Deaths
#   Net interprov      = Net interprovincial migration
#   Net international (PR) = Immigrants + Returning emigrants
#                          - Emigrants - Net temporary emigration
#   Net NPR            = Net non-permanent residents
bc_components_net <- bc_components %>%
  filter(component %in% c(
    "Births", "Deaths",
    "Net interprovincial migration",
    "Immigrants", "Emigrants",
    "Net non-permanent residents",
    "Returning emigrants", "Net temporary emigration"
  )) %>%
  mutate(
    contribution = case_when(
      component == "Births" ~ val,
      component == "Deaths" ~ -val,
      component == "Net interprovincial migration" ~ val,
      component == "Immigrants" ~ val,
      component == "Emigrants" ~ -val,
      component == "Returning emigrants" ~ val,
      component == "Net temporary emigration" ~ -val,
      component == "Net non-permanent residents" ~ val,
      TRUE ~ NA_real_
    ),
    bucket = case_when(
      component %in% c("Births", "Deaths") ~ "Natural increase",
      component == "Net interprovincial migration" ~ "Net interprovincial",
      component %in% c("Immigrants", "Emigrants",
                       "Returning emigrants",
                       "Net temporary emigration") ~ "Net international (PR)",
      component == "Net non-permanent residents" ~ "Net NPR"
    )
  ) %>%
  group_by(year_end, bucket) %>%
  summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop")

# Save processed -------------------------------------------------------------
write_csv(bc_pop,            file.path(proc_dir, "bc_population_quarterly.csv"))
write_csv(bc_components_net, file.path(proc_dir, "bc_components_net.csv"))

message("Done. Processed files written to: ", proc_dir)
