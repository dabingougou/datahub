# datahub

Data analyses and visualizations on topics in economics, demography, and labour markets. Each project is self-contained, reproducible, and published as a Quarto page.

## Repo structure

```
datahub/
├── pages/                  # Quarto source for each published page
├── docs/                   # Rendered HTML site (GitHub Pages output)
├── R/                      # R scripts that build the data pipeline
├── data/
│   ├── raw/                # Cached API pulls (gitignored)
│   └── processed/          # Small derived CSVs used in figures
└── renv/                   # Project-local R library (lockfile committed)
```

## Pipeline

```
R/01_fetch_bc_population.R        Statistics Canada CANSIM 17-10-0008-01,
                                  17-10-0009-01 → BC population & components
R/02_fetch_age_and_industry.R     BC Stats projections (Table 3) +
                                  CANSIM 14-10-0355-01 → age cohorts,
                                  industry employment
R/03_canada_comparison.R          Canada-level filters from cached pulls
R/04_world_bank.R                 World Bank Open Data (US, China, India)
                                  → cross-country age and population
```

Each script writes CSVs to `data/processed/`. Pages in `pages/` read from those CSVs.

## Reproducibility

R packages are managed with [`renv`](https://rstudio.github.io/renv/). To restore:

```r
renv::restore()
```

The BC Stats projections workbook (used by `R/02`) is a public xlsx file from BC Stats; the script expects it at `data/raw/temp/1971-table_2023_estimates_-_2024-2046_projections.xlsx`. Download from [BC Stats](https://www2.gov.bc.ca/gov/content/data/statistics/people-population-community/population/population-projections).

## Projects

- **How Canada's federal immigration policy reshaped British Columbia's population, 2019–2025** — `pages/bc_population_2019_2025.qmd` ([live](https://dabingougou.github.io/datahub/bc_population_2019_2025.html))
- **2021 US County Income Map** — `pages/2021_county_income.qmd` ([live](https://dabingougou.github.io/datahub/2021_county_income.html))
- **2021 US County Wage Map** — `pages/2021_county_wage.qmd` ([live](https://dabingougou.github.io/datahub/2021_county_wage.html))

## Contact

huibin.chang@berkeley.edu
