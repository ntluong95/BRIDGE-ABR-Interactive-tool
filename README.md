# A Goal Conflict Explorer

A modern Shiny dashboard for visualizing National Action Plan interactions between:

- `AMR-AMR`
- `SDG-SDG`
- `AMR-SDG`

with effect categories:

- `Synergy (co-benefit)`
- `Tension (trade-off)`
- `Mixed / context-dependent`

## Scope model

- AMR nodes: 5 WHO GAP objectives (`AMR-01` to `AMR-05`)
- SDG nodes: 17 UN SDGs (`SDG-01` to `SDG-17`)
- Interaction record level: country + theme + evidence metadata

## App structure

- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/app.R`
- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/www/styles.css`
- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/www/logo-bridge-abr.png`
- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/data/policy_nodes.csv`
- `/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/data/policy_interactions.csv`

## UI redesign highlights

- Sticky modern navbar with branded logo and multi-tab navigation
- Dashboard two-panel layout with collapsible filter sidebar
- Accordion-based filters with searchable multi-select pickers
- Apply-filter workflow with clear-filters action
- KPI metrics strip (Synergy, Trade-off, Mixed, Total)
- Redesigned network card with SDG palette coloring and AMR diamond nodes
- Animated right-side node details drawer on node click
- Horizontal pill-based legend
- Loader animations and responsive mobile behavior

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
- `interaction_type`
- `country`
- `theme`
- `effect`
- `direct_or_indirect`
- `bidirectional`
- `context_dependent`
- `interaction_summary`
- `policy_tension`
- `evidence_level`
- `reference`

## R package requirements

```r
install.packages(c(
  "shiny", "dplyr", "readr", "stringr", "DT", "visNetwork",
  "bslib", "bsicons", "shinyWidgets", "shinycssloaders"
))
```

## Run

```r
shiny::runApp("/Users/luongnguyen/Library/CloudStorage/OneDrive-SharedLibraries-Uppsalauniversitet/ReAct - BRIDGE ABR project/03 Workshop/BRIDGE-ABR Interactive tool/app.R")
```
