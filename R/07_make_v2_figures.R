# =============================================================================
# 07_make_v2_figures.R — figures with BC vs Canada comparisons (v2 of brief)
#
# Output: deliverables/figures_v2/*.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(here)
  library(lubridate)
})

fig_dir <- here("deliverables", "figures_v2")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
proc_dir <- here("data", "processed")

theme_briefing <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(color = "grey30", size = base_size - 1),
      plot.caption = element_text(color = "grey40", size = base_size - 2,
                                   hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1)
    )
}

# =============================================================================
# Figure 1 — BC components (kept as is) with Canada totals annotated
# =============================================================================
bc_comp <- read_csv(file.path(proc_dir, "bc_components_net.csv"),
                    show_col_types = FALSE) %>%
  mutate(period = paste0(year_end - 1, "/", substr(year_end, 3, 4)),
         bucket = factor(bucket, levels = c(
           "Net NPR", "Net international (PR)",
           "Net interprovincial", "Natural increase")))

ca_comp <- read_csv(file.path(proc_dir, "canada_components_net.csv"),
                    show_col_types = FALSE) %>%
  group_by(year_end) %>%
  summarise(canada_total = sum(contribution, na.rm = TRUE) / 1000) %>%
  mutate(period = paste0(year_end - 1, "/", substr(year_end, 3, 4)))

palette_components <- c(
  "Net NPR" = "#d62728",
  "Net international (PR)" = "#1f77b4",
  "Net interprovincial" = "#2ca02c",
  "Natural increase" = "#9467bd"
)

p1 <- ggplot(bc_comp,
             aes(x = period, y = contribution / 1000, fill = bucket)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = palette_components) +
  scale_y_continuous(labels = label_comma(),
                     breaks = pretty_breaks(8)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, y = "Thousand persons",
    caption = "Source: Statistics Canada, Table 17-10-0008-01."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig1_components.png"), p1,
       width = 7.0, height = 3.6, dpi = 300, bg = "white")

# =============================================================================
# Figure 2 — Population trajectory, BC vs Canada (indexed Q1 2019 = 100)
# =============================================================================
bc_pop <- read_csv(file.path(proc_dir, "bc_population_quarterly.csv"),
                   show_col_types = FALSE) %>%
  filter(date >= ymd("2019-01-01")) %>%
  mutate(geo = "BC")

ca_pop <- read_csv(file.path(proc_dir, "canada_population_quarterly.csv"),
                   show_col_types = FALSE) %>%
  filter(date >= ymd("2019-01-01")) %>%
  rename(val_norm = val) %>%
  mutate(geo = "Canada")

both_pop <- bind_rows(
  bc_pop %>% select(date, val_norm, geo),
  ca_pop
) %>%
  group_by(geo) %>%
  arrange(date) %>%
  mutate(idx = val_norm / first(val_norm) * 100) %>%
  ungroup()

p2 <- ggplot(both_pop, aes(x = date, y = idx, color = geo)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("BC" = "#1f77b4", "Canada" = "grey50")) +
  scale_y_continuous(breaks = pretty_breaks(6)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Population trajectory: BC vs Canada, Q1 2019 = 100",
    subtitle = "BC's growth ran slightly above Canada's, then turned earlier. Both peaked in early 2025 and have edged down since.",
    x = NULL, y = "Index (Q1 2019 = 100)",
    caption = "Source: Statistics Canada, Table 17-10-0009-01."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig2_population_level.png"), p2,
       width = 7.0, height = 3.0, dpi = 300, bg = "white")

# =============================================================================
# Figure 4 — Working-age and senior shares, BC vs Canada
# =============================================================================
bc_age <- read_csv(file.path(proc_dir, "bc_age_cohorts.csv"),
                   show_col_types = FALSE) %>%
  filter(year >= 2000, year <= 2025) %>%
  transmute(year, geo = "BC",
            wa_pct = pct_working_age * 100,
            sr_pct = pct_seniors * 100)

ca_age <- read_csv(file.path(proc_dir, "canada_age_cohorts.csv"),
                   show_col_types = FALSE) %>%
  filter(year >= 2000, year <= 2025) %>%
  transmute(year, geo = "Canada",
            wa_pct = pct_working_age * 100,
            sr_pct = pct_seniors * 100)

age_long <- bind_rows(bc_age, ca_age) %>%
  pivot_longer(c(wa_pct, sr_pct), names_to = "metric", values_to = "pct") %>%
  mutate(metric = recode(metric,
                         wa_pct = "Working-age share (15-64)",
                         sr_pct = "Senior share (65+)"))

p4 <- ggplot(age_long, aes(x = year, y = pct, color = geo, linetype = metric)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("BC" = "#1f77b4", "Canada" = "grey45")) +
  scale_linetype_manual(values = c("Working-age share (15-64)" = "solid",
                                   "Senior share (65+)" = "dashed")) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     breaks = pretty_breaks(8)) +
  labs(
    title = "Age structure: BC vs Canada, 2000-2025",
    subtitle = "BC has a slightly higher working-age share and lower senior share than Canada, but both are converging.",
    x = NULL, y = "Share of total population",
    caption = "Sources: BC Stats (Table 3); Statistics Canada Table 17-10-0005-01."
  ) +
  theme_briefing() +
  theme(legend.position = "right")

ggsave(file.path(fig_dir, "fig4_age_structure.png"), p4,
       width = 7.2, height = 3.2, dpi = 300, bg = "white")

# =============================================================================
# Figure 5 — Sectoral employment change, BC vs Canada (paired bars)
# =============================================================================
bc_ind <- read_csv(file.path(proc_dir, "bc_industry_change_2024_to_latest.csv"),
                   show_col_types = FALSE) %>%
  transmute(industry, geo = "BC", pct_change)

ca_ind <- read_csv(file.path(proc_dir,
                              "canada_industry_change_2024_to_latest.csv"),
                   show_col_types = FALSE) %>%
  transmute(industry, geo = "Canada", pct_change)

drop_aggs <- c("Total employed, all industries",
               "Goods-producing sector",
               "Services-producing sector")

both_ind <- bind_rows(bc_ind, ca_ind) %>%
  filter(!industry %in% drop_aggs) %>%
  mutate(industry = recode(industry,
    "Forestry, fishing, mining, quarrying, oil and gas" = "Forestry/mining/oil",
    "Professional, scientific and technical services" = "Professional & technical",
    "Business, building and other support services" = "Business support",
    "Other services (except public administration)" = "Other services",
    "Wholesale and retail trade" = "Wholesale & retail",
    "Information, culture and recreation" = "Information & culture",
    "Accommodation and food services" = "Accommodation & food",
    "Health care and social assistance" = "Health & social services",
    "Transportation and warehousing" = "Transport & warehousing",
    "Educational services" = "Education",
    "Public administration" = "Public administration"
  ))

# Order industries by BC's pct_change
order_levels <- both_ind %>%
  filter(geo == "BC") %>%
  arrange(pct_change) %>%
  pull(industry)

both_ind <- both_ind %>%
  mutate(industry = factor(industry, levels = order_levels))

p5 <- ggplot(both_ind,
             aes(x = pct_change, y = industry, fill = geo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     breaks = pretty_breaks(8)) +
  scale_fill_manual(values = c("BC" = "#1f77b4", "Canada" = "grey55")) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Percent change in employment", y = NULL,
    caption = "Source: Statistics Canada, Table 14-10-0355-01."
  ) +
  theme_briefing() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"))

ggsave(file.path(fig_dir, "fig5_sectoral_change.png"), p5,
       width = 7.4, height = 4.0, dpi = 300, bg = "white")

# =============================================================================
# Figure 6 — Old-age dependency: BC vs Canada
# =============================================================================
bc_dep <- read_csv(file.path(proc_dir, "bc_age_cohorts.csv"),
                   show_col_types = FALSE) %>%
  filter(year >= 2000, year <= 2025) %>%
  transmute(year, geo = "BC", dep_pct = dep_old * 100)

ca_dep <- read_csv(file.path(proc_dir, "canada_age_cohorts.csv"),
                   show_col_types = FALSE) %>%
  filter(year >= 2000, year <= 2025) %>%
  transmute(year, geo = "Canada", dep_pct = dep_old * 100)

dep <- bind_rows(bc_dep, ca_dep)

p6 <- ggplot(dep, aes(x = year, y = dep_pct, color = geo)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("BC" = "#1f77b4", "Canada" = "grey45")) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     breaks = pretty_breaks(6)) +
  labs(
    title = "Old-age dependency: BC vs Canada, 2000-2025",
    subtitle = "Seniors per 100 working-age (15-64). BC tracks just below the Canada average; both rising.",
    x = NULL, y = "Seniors per 100 working-age",
    caption = "Sources: BC Stats Table 3; Statistics Canada Table 17-10-0005-01."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig6_old_age_dependency.png"), p6,
       width = 7.0, height = 2.8, dpi = 300, bg = "white")

message("v2 figures saved to: ", fig_dir)
