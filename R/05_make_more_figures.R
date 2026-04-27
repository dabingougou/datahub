# =============================================================================
# 05_make_more_figures.R — additional figures for the expanded briefing
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(here)
  library(lubridate)
})

fig_dir <- here("deliverables", "figures")
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
# Figure 4 — Working-age share and senior share, 2000–2046
# =============================================================================
ages <- read_csv(file.path(proc_dir, "bc_age_cohorts.csv"),
                 show_col_types = FALSE)

ages_long <- ages %>%
  filter(year >= 2000, year <= 2046) %>%
  mutate(
    `Working-age (15-64)` = pct_working_age * 100,
    `Seniors (65+)` = pct_seniors * 100,
    `Children (0-14)` = age_0_14 / total_pop * 100
  ) %>%
  select(year, status, `Working-age (15-64)`, `Seniors (65+)`, `Children (0-14)`) %>%
  pivot_longer(-c(year, status), names_to = "cohort", values_to = "pct")

est_long <- ages_long %>% filter(status == "Estimate")
prj_long <- ages_long %>% filter(status == "Projection")
bridge_long <- est_long %>% group_by(cohort) %>%
  slice_max(year, n = 1) %>% ungroup() %>% mutate(status = "Projection")
prj_long_line <- bind_rows(bridge_long, prj_long)

palette_age <- c(
  "Working-age (15-64)" = "#1f77b4",
  "Seniors (65+)"       = "#d62728",
  "Children (0-14)"     = "#2ca02c"
)

p4 <- ggplot() +
  geom_line(data = est_long, aes(x = year, y = pct, color = cohort),
            linewidth = 0.8) +
  geom_line(data = prj_long_line, aes(x = year, y = pct, color = cohort),
            linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = 2024.5, color = "grey60", linetype = "dotted") +
  annotate("text", x = 2025, y = 12, label = "Projection →",
           hjust = 0, size = 3, color = "grey40") +
  scale_color_manual(values = palette_age) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     limits = c(10, 75), breaks = pretty_breaks(8)) +
  scale_x_continuous(breaks = seq(2000, 2046, 5)) +
  labs(
    title = "BC age structure: working-age share peaked, seniors rising sharply",
    subtitle = "Share of total BC population, 2000–2046; dashed = BC Stats projection from 2025",
    x = NULL, y = "Share of total population",
    caption = "Source: BC Stats, 2023 Estimates and 2024–2046 Projections (Table 3)."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig4_age_structure.png"), p4,
       width = 7.0, height = 3.2, dpi = 300, bg = "white")

# =============================================================================
# Figure 5 — Sectoral employment change, January 2024 to latest (BC)
# =============================================================================
ind_change <- read_csv(file.path(proc_dir,
                                  "bc_industry_change_2024_to_latest.csv"),
                       show_col_types = FALSE)

# Drop aggregates that overlap with sub-sectors to avoid double labeling
to_drop <- c("Total employed, all industries",
             "Goods-producing sector",
             "Services-producing sector")

ind_plot <- ind_change %>%
  filter(!industry %in% to_drop) %>%
  mutate(
    industry = recode(industry,
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
    ),
    direction = if_else(pct_change >= 0, "Gained", "Lost"),
    industry = fct_reorder(industry, pct_change)
  )

p5 <- ggplot(ind_plot,
             aes(x = pct_change, y = industry, fill = direction)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f%%", pct_change * 100),
                hjust = if_else(pct_change >= 0, -0.1, 1.1)),
            size = 2.8, color = "grey20") +
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(mult = c(0.15, 0.18))) +
  scale_fill_manual(values = c("Gained" = "#2ca02c", "Lost" = "#d62728"),
                    guide = "none") +
  labs(
    title = "BC employment by industry: change from Jan 2024 to early 2026",
    subtitle = "Seasonally adjusted, percent change. Construction and goods sectors absorbed displaced labour; retail, transport, and info-culture cooled.",
    x = "Percent change in employment", y = NULL,
    caption = "Source: Statistics Canada, Table 14-10-0355-01."
  ) +
  theme_briefing() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90"))

ggsave(file.path(fig_dir, "fig5_sectoral_change.png"), p5,
       width = 7.2, height = 4.0, dpi = 300, bg = "white")

# =============================================================================
# Figure 6 — Old-age dependency ratio, 2000–2046
# =============================================================================
dep <- ages %>%
  filter(year >= 2000) %>%
  mutate(dep_old_pct = dep_old * 100)

dep_est <- dep %>% filter(status == "Estimate")
dep_prj <- dep %>% filter(status == "Projection")
# Bridge so the line is continuous
bridge <- dep_est %>% slice_max(year, n = 1) %>% mutate(status = "Projection")
dep_prj_line <- bind_rows(bridge, dep_prj)

p6 <- ggplot() +
  geom_line(data = dep_est, aes(x = year, y = dep_old_pct),
            linewidth = 0.9, color = "#d62728") +
  geom_line(data = dep_prj_line, aes(x = year, y = dep_old_pct),
            linewidth = 0.9, color = "#d62728", linetype = "dashed") +
  geom_vline(xintercept = 2024.5, color = "grey60", linetype = "dotted") +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     breaks = pretty_breaks(6)) +
  scale_x_continuous(breaks = seq(2000, 2046, 5)) +
  labs(
    title = "BC old-age dependency ratio: 24% (2024) → 36% (2046)",
    subtitle = "Seniors (65+) per 100 working-age (15-64); dashed from 2025 = projection",
    x = NULL, y = "Seniors per 100 working-age adults",
    caption = "Source: BC Stats, 2023 Estimates and 2024–2046 Projections (Table 3)."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig6_old_age_dependency.png"), p6,
       width = 7.0, height = 2.6, dpi = 300, bg = "white")

message("More figures saved to: ", fig_dir)
