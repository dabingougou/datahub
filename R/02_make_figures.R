# =============================================================================
# 02_make_figures.R — produce charts for the BC population briefing note
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

# Theme ------------------------------------------------------------------------
theme_briefing <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(color = "grey30", size = base_size - 1),
      plot.caption = element_text(color = "grey40", size = base_size - 2,
                                   hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(size = base_size - 1),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1)
    )
}

# =============================================================================
# Figure 1 — Components of BC population change, demographic years 2019/20–2024/25
# =============================================================================
components <- read_csv(file.path(proc_dir, "bc_components_net.csv"),
                       show_col_types = FALSE) %>%
  mutate(
    bucket = factor(bucket, levels = c(
      "Net NPR",
      "Net international (PR)",
      "Net interprovincial",
      "Natural increase"
    )),
    period = paste0(year_end - 1, "/", substr(year_end, 3, 4))
  )

palette_components <- c(
  "Net NPR"                = "#d62728",
  "Net international (PR)" = "#1f77b4",
  "Net interprovincial"    = "#2ca02c",
  "Natural increase"       = "#9467bd"
)

p1 <- ggplot(components,
             aes(x = period, y = contribution / 1000, fill = bucket)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = palette_components) +
  scale_y_continuous(labels = label_comma(),
                     breaks = pretty_breaks(8)) +
  labs(
    title = "British Columbia: components of population change, 2019/20–2024/25",
    subtitle = "Annual contribution to net population change, thousands of persons (demographic year ending June 30)",
    x = NULL, y = "Thousand persons",
    caption = "Source: Statistics Canada, Table 17-10-0008-01."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig1_components.png"), p1,
       width = 7.0, height = 3.6, dpi = 300, bg = "white")

# =============================================================================
# Figure 2 — BC population: history + BC Stats projections through 2035
# =============================================================================
proj <- read_csv(file.path(proc_dir, "bc_stats_projections.csv"),
                 show_col_types = FALSE) %>%
  filter(year >= 2015, year <= 2035) %>%
  mutate(date = ymd(paste0(year, "-07-01")),
         pop_M = total_pop / 1000)  # file is in thousands

est <- proj %>% filter(status == "Estimate")
prj <- proj %>% filter(status == "Projection")

# Bridge point so the line connects continuously across estimate -> projection
bridge <- est %>% slice_max(year, n = 1) %>% mutate(status = "Projection")
prj_line <- bind_rows(bridge, prj)

p2 <- ggplot() +
  geom_line(data = est, aes(x = date, y = pop_M),
            linewidth = 0.9, color = "#1f77b4") +
  geom_line(data = prj_line, aes(x = date, y = pop_M),
            linewidth = 0.9, color = "#1f77b4", linetype = "dashed") +
  geom_point(data = filter(proj, year %in% c(2024, 2026, 2030, 2035)),
             aes(x = date, y = pop_M),
             color = "#1f77b4", size = 2) +
  geom_text(data = filter(proj, year %in% c(2024, 2026, 2030)),
            aes(x = date, y = pop_M,
                label = format(round(total_pop), big.mark = ",")),
            vjust = -0.9, size = 3, color = "grey20") +
  geom_text(data = filter(proj, year == 2035),
            aes(x = date, y = pop_M,
                label = format(round(total_pop), big.mark = ",")),
            hjust = 1.15, vjust = -0.4, size = 3, color = "grey20") +
  annotate("rect",
           xmin = ymd("2025-01-01"), xmax = ymd("2027-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.35) +
  annotate("text", x = ymd("2026-01-01"), y = 5.20,
           label = "Near-term\ncorrection", size = 3, color = "grey30") +
  scale_y_continuous(labels = label_number(accuracy = 0.1,
                                           suffix = "M"),
                     breaks = pretty_breaks(6)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               limits = c(ymd("2015-01-01"), ymd("2036-06-01")),
               expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    title = "BC population: 2015–2024 estimates and BC Stats projections to 2035",
    subtitle = "Annual, millions of persons. Solid = estimate; dashed = BC Stats P.75 projection.",
    x = NULL, y = "Population (millions)",
    caption = "Sources: BC Stats (estimates 2015–2024 and projections 2025–2035, July 1 each year)."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig2_population_level.png"), p2,
       width = 7.0, height = 3.0, dpi = 300, bg = "white")

# =============================================================================
# Figure 3 — Unemployment rate, BC, 2019–present (LFS, seasonally adjusted)
# =============================================================================
lfs <- read_csv(file.path(proc_dir, "bc_lfs_monthly.csv"),
                show_col_types = FALSE)

ur <- lfs %>% filter(indicator == "Unemployment rate")

p3 <- ggplot(ur, aes(x = date, y = value)) +
  geom_line(linewidth = 0.8, color = "#1f77b4") +
  geom_smooth(se = FALSE, color = "grey60", linetype = "dashed",
              span = 0.4, linewidth = 0.5) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "BC unemployment rate, 2019–2026",
    subtitle = "Seasonally adjusted, monthly",
    x = NULL, y = "Unemployment rate",
    caption = "Source: Statistics Canada, Table 14-10-0287-01."
  ) +
  theme_briefing()

ggsave(file.path(fig_dir, "fig3_unemployment.png"), p3,
       width = 7.0, height = 3.0, dpi = 300, bg = "white")

message("Figures saved to: ", fig_dir)
