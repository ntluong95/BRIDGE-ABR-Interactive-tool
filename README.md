# BRIDGE-ABR AMR-SDG Interactive Explorer

A production-oriented Shiny app prototype for the BRIDGE-ABR workshop to visualize:

- AMR-SDG interactions
- AMR-AMR interactions
- SDG-SDG interactions
- SDG-to-AMR outcome pathways

## Files

- `/app.R`: Shiny application
- `/data/amr_sdg_nodes.csv`: node dictionary (SDGs, AMR drivers/responses, AMR outcomes)
- `/data/amr_sdg_edges.csv`: interaction evidence table

## Features

- Network explorer with filters for:
  - interaction family
  - theme
  - effect on AMR risk
  - evidence level
  - context dependency and bidirectionality
  - free text search
- Detailed interaction and reference tables
- Dedicated outcome-pathway tab to trace upstream links into a selected AMR outcome
- Data and methods tab with node dictionary and interaction summary
- CSV downloads for filtered interactions and outcome pathways

## Data schema

### `amr_sdg_nodes.csv`
Required columns:

- `id`
- `label`
- `short_label`
- `node_type`
- `domain`
- `theme`
- `description`

### `amr_sdg_edges.csv`
Required columns:

- `edge_id`
- `from`
- `to`
- `interaction_family`
- `theme`
- `effect`
- `direct_or_indirect`
- `bidirectional`
- `context_dependent`
- `mechanism`
- `policy_tension`
- `evidence_level`
- `reference`

## R package requirements

```r
install.packages(c("shiny", "dplyr", "readr", "stringr", "DT", "visNetwork"))
```

## Run

```r
shiny::runApp("app.R")
```

## How to move from prototype to production data

1. Replace `/data/amr_sdg_nodes.csv` with your reviewed node list.
2. Replace `/data/amr_sdg_edges.csv` with curated evidence-coded interactions.
3. Keep required column names exactly the same.
4. Use `evidence_level` and `reference` to preserve traceability for policy dialogue.
