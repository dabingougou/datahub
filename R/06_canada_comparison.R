# =============================================================================
# 06_canada_comparison.R — pull Canada totals from already-cached tables
# =============================================================================

suppressPackageStartupMessages({
  library(cansim)
  library(tidyverse)
  library(lubridate)
  library(here)
})

raw_dir <- here("data", "raw")
proc_dir <- here("data", "processed")

# All four tables already cached as .rds — re-read them
pop_q   <- readRDS(file.path(raw_dir, "17_10_0009_01.rds"))
comp_a  <- readRDS(file.path(raw_dir, "17_10_0008_01.rds"))
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
# Canada components (annual)
# ---------------------------------------------------------------------------
canada_components <- comp_a %>%
  filter(GEO == "Canada") %>%
  mutate(year_end = as.integer(substr(REF_DATE, nchar(REF_DATE) - 3,
                                       nchar(REF_DATE)))) %>%
  select(REF_DATE, year_end, component = `Components of population growth`,
         val = VALUE) %>%
  filter(year_end >= 2020, year_end <= 2025)

# Same component buckets as for BC (skip interprov — nets to zero at Canada level)
canada_comp_net <- canada_components %>%
  filter(component %in% c(
    "Births", "Deaths",
    "Immigrants", "Emigrants",
    "Net non-permanent residents",
    "Returning emigrants", "Net temporary emigration"
  )) %>%
  mutate(
    contribution = case_when(
      component == "Births" ~ val,
      component == "Deaths" ~ -val,
      component == "Immigrants" ~ val,
      component == "Emigrants" ~ -val,
      component == "Returning emigrants" ~ val,
      component == "Net temporary emigration" ~ -val,
      component == "Net non-permanent residents" ~ val,
      TRUE ~ NA_real_
    ),
    bucket = case_when(
      component %in% c("Births", "Deaths") ~ "Natural increase",
      component %in% c("Immigrants", "Emigrants",
                       "Returning emigrants",
                       "Net temporary emigration") ~ "Net international (PR)",
      component == "Net non-permanent residents" ~ "Net NPR"
    )
  ) %>%
  group_by(year_end, bucket) %>%
  summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop")

write_csv(canada_comp_net, file.path(proc_dir, "canada_components_net.csv"))

# ---------------------------------------------------------------------------
# Canada industry employment change Jan 2024 → latest
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

# ---------------------------------------------------------------------------
# Canada population by 5-year age groups for dependency ratio
# Use Table 17-10-0005-01 (population by age and sex, annual)
# ---------------------------------------------------------------------------
pop_age_path <- file.path(raw_dir, "17_10_0005_01.rds")
if (!file.exists(pop_age_path)) {
  message("Fetching: 17-10-0005-01")
  pop_age <- get_cansim("17-10-0005-01")
  saveRDS(pop_age, pop_age_path)
} else {
  pop_age <- readRDS(pop_age_path)
}

# Age categories — pick "0 to 14 years", "15 to 64 years", "65 years and older"
canada_ages <- pop_age %>%
  filter(GEO == "Canada",
         Gender %in% c("Total - gender", "Both sexes"),
         `Age group` %in% c("0 to 14 years", "15 to 64 years",
                            "65 years and older")) %>%
  mutate(year = as.integer(REF_DATE)) %>%
  select(year, age_group = `Age group`, val = VALUE) %>%
  pivot_wider(names_from = age_group, values_from = val) %>%
  rename(age_0_14 = `0 to 14 years`,
         age_15_64 = `15 to 64 years`,
         age_65p   = `65 years and older`) %>%
  mutate(
    total = age_0_14 + age_15_64 + age_65p,
    pct_working_age = age_15_64 / total,
    pct_seniors = age_65p / total,
    dep_old = age_65p / age_15_64
  ) %>%
  filter(year >= 2000, year <= 2026)

write_csv(canada_ages, file.path(proc_dir, "canada_age_cohorts.csv"))

message("Canada comparison data written to: ", proc_dir)
