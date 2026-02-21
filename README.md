# BRIDGE-ABR Policy Tension Explorer

This app visualizes **policy-objective interactions only** at National Action Plan level:

- `AMR-AMR`
- `SDG-SDG`
- `AMR-SDG`

It does **not** model AMR outcomes in this version.

## Objective model

- AMR side: **5 WHO Global Action Plan objectives** (`AMR-01` to `AMR-05`)
- SDG side: **17 UN SDG goals** (`SDG-01` to `SDG-17`)

## Included example

The sample dataset includes the requested structure, including cases like:

- `AMR-03` and `SDG-08` in `Country XXX`
- Theme: `Agriculture & Food Systems`

## Files

- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/app.R`
- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/data/policy_nodes.csv`
- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/data/policy_interactions.csv`
- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/www/logo-bridge-abr.svg`

## Data schema

### `policy_nodes.csv`

Required columns:

- `id`
- `label`
- `short_label`
- `node_group`
- `node_type`
- `source_framework`
- `description`

### `policy_interactions.csv`

Required columns:

- `edge_id`
- `from`
- `to`
- `interaction_family` (`AMR-AMR`, `SDG-SDG`, `AMR-SDG`)
- `country`
- `theme`
- `effect` (`Tension (trade-off)`, `Synergy (co-benefit)`, `Mixed / context-dependent`)
- `direct_or_indirect`
- `bidirectional`
- `context_dependent`
- `interaction_summary`
- `policy_tension`
- `evidence_level`
- `reference`

## Features

- Branded navbar with logo
- Network visualization of policy objective interactions
- Filters by family, country, theme, effect, evidence level, and objective focus
- Search on policy tension and references
- Filtered interaction table and country summary table
- CSV export of current filtered view

## R package requirements

```r
install.packages(c("shiny", "dplyr", "readr", "stringr", "DT", "visNetwork"))
```

## Run

```r
shiny::runApp("/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/app.R")
```
