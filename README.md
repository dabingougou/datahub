# datahub

Data analyses and visualizations on topics in economics, demography, and labour markets. Each project is self-contained, reproducible, and (where appropriate) published as an interactive page.

## Repo structure

```
datahub/
├── pages/                  # Quarto source for each published page
├── docs/                   # Rendered HTML site (GitHub Pages output)
├── R/                      # Reusable R helpers (StatsCan, plotting, etc.)
├── data/
│   ├── raw/                # Cached API pulls (gitignored)
│   └── processed/          # Small derived CSVs used in figures
├── deliverables/           # Static outputs (PDFs, briefing notes)
└── renv/                   # Project-local R library (lockfile committed)
```

## Reproducibility

R packages are managed with [`renv`](https://rstudio.github.io/renv/). To restore the environment:

```r
renv::restore()
```

## Projects

- **2021 US County Income Map** — `pages/2021_county_income.qmd`
- **2021 US County Wage Map** — `pages/2021_county_wage.qmd`
- **BC Population Drivers, 2019–2025** — `pages/bc_population_2019_2025.qmd` (in progress)

## Contact

huibin.chang@berkeley.edu
