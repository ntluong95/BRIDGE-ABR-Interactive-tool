# Preparation ------------------------------------------------------------

pacman::p_load(
  shiny,
  readr,
  stringr,
  DT,
  visNetwork,
  bslib,
  bsicons,
  shinyWidgets,
  shinycssloaders,
  tidyverse
)


data_dir <- "data"
nodes_path <- file.path(data_dir, "policy_nodes_description.csv")
interactions_path <- file.path(data_dir, "policy_interaction_claude.csv")

if (!file.exists(nodes_path) || !file.exists(interactions_path)) {
  stop(
    "Missing data files. Expected data/policy_nodes_description.csv and data/policy_interaction_claude.csv"
  )
}

nodes <- read_csv(nodes_path, show_col_types = FALSE)
interactions <- read_csv(interactions_path, show_col_types = FALSE)

required_node_cols <- c(
  "id",
  "label",
  "short_label",
  "node_group",
  "node_type",
  "source_framework",
  "description"
)
required_interaction_cols <- c(
  "edge_id",
  "from",
  "to",
  "interaction_type",
  "country",
  "theme",
  "effect",
  "direct_or_indirect",
  "bidirectional",
  "interaction_summary",
  "policy_tension",
  "reference"
)

if (!all(required_node_cols %in% names(nodes))) {
  stop("data/policy_nodes_description.csv is missing required columns")
}
if (!all(required_interaction_cols %in% names(interactions))) {
  stop("data/policy_interaction_claude.csv is missing required columns")
}

derive_interaction_type <- function(from_id, to_id) {
  from_amr <- str_detect(from_id, "^amr")
  to_amr <- str_detect(to_id, "^amr")

  case_when(
    from_amr & to_amr ~ "AMR-AMR",
    !from_amr & !to_amr ~ "SDG-SDG",
    TRUE ~ "AMR-SDG"
  )
}

normalize_effect <- function(effect_value) {
  case_when(
    effect_value %in%
      c("Align", "Synergy (co-benefit)") ~ "Synergy (co-benefit)",
    effect_value %in%
      c("Conflict", "Tension (trade-off)") ~ "Tension (trade-off)",
    effect_value %in%
      c(
        "Independent",
        "Mixed / context-dependent"
      ) ~ "Mixed / context-dependent",
    TRUE ~ effect_value
  )
}

nodes <- nodes %>%
  mutate(
    across(where(is.character), str_squish),
    id = str_to_lower(id)
  )

interactions <- interactions %>%
  mutate(
    across(where(is.character), str_squish),
    from = str_to_lower(from),
    to = str_to_lower(to),
    interaction_type = derive_interaction_type(from, to),
    effect_raw = effect,
    effect = normalize_effect(effect),
    theme = if_else(is.na(theme) | theme == "", "Unclassified", theme),
    bidirectional = as.logical(bidirectional)
  )

# Add optional columns with defaults when absent from the dataset
if (!"context_dependent" %in% names(interactions)) {
  interactions$context_dependent <- FALSE
} else {
  interactions$context_dependent <- as.logical(interactions$context_dependent)
}
if (!"evidence_level" %in% names(interactions)) {
  interactions$evidence_level <- "Not specified"
}
# Ensure new enriched policy-text columns exist (filled NA if absent)
for (.col in c(
  "policy_strategic_objective_1", "policy_strategic_objective_2",
  "policy_specific_actions_1",    "policy_specific_actions_2"
)) {
  if (!.col %in% names(interactions)) interactions[[.col]] <- NA_character_
}
rm(.col)

if (anyDuplicated(nodes$id) > 0) {
  stop("Duplicate node ids found in data/policy_nodes_description.csv")
}
if (anyDuplicated(interactions$edge_id) > 0) {
  stop("Duplicate edge ids found in data/policy_interactions.csv")
}
if (
  any(!interactions$from %in% nodes$id) || any(!interactions$to %in% nodes$id)
) {
  stop(
    "Interaction table contains node ids not found in policy_nodes_description.csv"
  )
}

node_lookup <- nodes %>%
  select(id, label, short_label, node_group, node_type, description)

interactions_enriched <- interactions %>%
  left_join(node_lookup, by = c("from" = "id")) %>%
  rename(
    from_label = label,
    from_short = short_label,
    from_group = node_group,
    from_type = node_type,
    from_description = description
  ) %>%
  left_join(node_lookup, by = c("to" = "id")) %>%
  rename(
    to_label = label,
    to_short = short_label,
    to_group = node_group,
    to_type = node_type,
    to_description = description
  ) %>%
  mutate(
    search_blob = str_to_lower(
      paste(
        edge_id,
        country,
        theme,
        interaction_type,
        effect,
        source,
        from_short,
        to_short,
        interaction_summary,
        policy_tension,
        evidence_level,
        reference,
        if_else(is.na(policy_strategic_objective_1), "", policy_strategic_objective_1),
        if_else(is.na(policy_strategic_objective_2), "", policy_strategic_objective_2),
        if_else(is.na(policy_specific_actions_1),    "", policy_specific_actions_1),
        if_else(is.na(policy_specific_actions_2),    "", policy_specific_actions_2)
      )
    )
  )

type_choices <- c("AMR-AMR", "SDG-SDG", "AMR-SDG")
type_choices <- intersect(
  type_choices,
  sort(unique(interactions_enriched$interaction_type))
)
country_choices <- sort(unique(interactions_enriched$country))
theme_choices <- sort(unique(interactions_enriched$theme))
effect_choices <- c(
  "Tension (trade-off)",
  "Synergy (co-benefit)",
  "Mixed / context-dependent"
)
effect_choices <- intersect(
  effect_choices,
  sort(unique(interactions_enriched$effect))
)
evidence_priority <- c("High", "Medium", "Low", "Emerging")
evidence_present <- sort(unique(interactions_enriched$evidence_level))
evidence_choices <- c(
  evidence_priority[evidence_priority %in% evidence_present],
  sort(setdiff(evidence_present, evidence_priority))
)
objective_choices <- setNames(
  nodes$id,
  paste0(nodes$short_label, " - ", nodes$label)
)
network_focus_choices <- c("All objectives" = "", objective_choices)

sdg_palette <- c(
  "SDG-01" = "#E5243B",
  "SDG-02" = "#DDA63A",
  "SDG-03" = "#4C9F38",
  "SDG-04" = "#C5192D",
  "SDG-05" = "#FF3A21",
  "SDG-06" = "#26BDE2",
  "SDG-07" = "#FCC30B",
  "SDG-08" = "#A21942",
  "SDG-09" = "#FD6925",
  "SDG-10" = "#DD1367",
  "SDG-11" = "#FD9D24",
  "SDG-12" = "#BF8B2E",
  "SDG-13" = "#3F7E44",
  "SDG-14" = "#0A97D9",
  "SDG-15" = "#56C02B",
  "SDG-16" = "#00689D",
  "SDG-17" = "#19486A"
)

amr_color <- "#031816"
effect_palette <- c(
  "Tension (trade-off)" = "#C62828",
  "Synergy (co-benefit)" = "#2E7D32",
  "Mixed / context-dependent" = "#F9A825"
)

build_sdg_logo_map <- function(www_dir = "www") {
  logo_files <- list.files(
    www_dir,
    pattern = "(?i)\\.(png|svg|jpg|jpeg|webp)$"
  )

  if (length(logo_files) == 0) {
    return(stats::setNames(character(), character()))
  }

  logo_num <- stringr::str_match(
    logo_files,
    "(?i)(?:sdg|goal)[^0-9]*([0-9]{1,2})"
  )[, 2]
  logo_num <- suppressWarnings(as.integer(logo_num))
  valid <- !is.na(logo_num) & logo_num >= 1 & logo_num <= 17

  if (!any(valid)) {
    return(stats::setNames(character(), character()))
  }

  logo_df <- data.frame(
    file = logo_files[valid],
    num = logo_num[valid],
    stringsAsFactors = FALSE
  )
  ext <- tolower(tools::file_ext(logo_df$file))
  logo_df$ext_rank <- dplyr::case_when(
    ext == "svg" ~ 1L,
    ext == "png" ~ 2L,
    ext %in% c("jpg", "jpeg") ~ 3L,
    ext == "webp" ~ 4L,
    TRUE ~ 5L
  )
  logo_df$file_len <- nchar(logo_df$file)
  logo_df <- logo_df[
    order(logo_df$num, logo_df$ext_rank, logo_df$file_len, logo_df$file),
  ]
  logo_df <- logo_df[!duplicated(logo_df$num), , drop = FALSE]

  logo_id <- sprintf("sdg%02d", logo_df$num)
  stats::setNames(logo_df$file, logo_id)
}

sdg_logo_map <- build_sdg_logo_map("www")
missing_sdg_logo <- setdiff(sprintf("sdg%02d", 1:17), names(sdg_logo_map))
if (length(missing_sdg_logo) > 0) {
  warning(
    paste(
      "SDG logo files not found for:",
      paste(missing_sdg_logo, collapse = ", ")
    ),
    call. = FALSE
  )
}

partner_links <- list(
  uu = "https://www.uu.se/en",
  react = "https://www.reactgroup.org/",
  src = "https://www.stockholmresilience.org/"
)

project_description_text <- paste(
  "The BRIDGE-ABR project is part of the Uppsala University Conflicting Objectives Nexus (UUniCORN).",
  "Led by ReAct Europe in collaboration with the Stockholm Resilience Centre,",
  "the project systematically maps and analyzes synergies and trade-offs between",
  "antibiotic resistance (ABR) mitigation objectives and the Sustainable Development Goals (SDGs).",
  "It also aims to strengthen the research-to-policy interface and support the development of",
  "integrated, context-appropriate solutions, with a particular focus on low- and middle-income countries (LMICs)."
)

partner_logo <- function(src, alt, href, image_class = "partner-logo-img") {
  tags$a(
    href = href,
    target = "_blank",
    rel = "noopener noreferrer",
    class = "partner-logo-link",
    tags$img(src = src, alt = alt, class = image_class)
  )
}

metric_card <- function(output_id, label, class_name = "") {
  div(
    class = paste("metric-card", class_name),
    div(class = "metric-label", label),
    div(
      class = "metric-value",
      textOutput(output_id, container = span, inline = TRUE)
    )
  )
}

filter_title <- function(icon_name, title_text) {
  tagList(bs_icon(icon_name), span(title_text))
}

picker_options <- list(
  `actions-box` = TRUE,
  `live-search` = TRUE,
  `selected-text-format` = "count > 2",
  size = 8
)

# ui.R -------------------------------------------------------------------

ui <- page_navbar(
  title = div(
    class = "app-brand",
    tags$img(
      src = "logo-bridge-abr.png",
      alt = "BRIDGE-ABR logo",
      class = "app-brand-logo"
    ),
    div(
      class = "app-brand-text",
      tags$span(class = "app-brand-kicker", "Policy Intelligence"),
      tags$span(class = "app-brand-title", "Policy Tension Explorer")
    )
  ),
  id = "main_nav",
  window_title = "Policy Tension Explorer",
  navbar_options = navbar_options(collapsible = TRUE),
  theme = bs_theme(
    version = 5,
    bg = "#F5F7FA",
    fg = "#111827",
    primary = "#A4343A",
    secondary = "#7F2A2F",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=6")
  ),

  nav_panel(
    "Policy Explorer",
    div(
      class = "page-wrap",
      layout_sidebar(
        sidebar = sidebar(
          id = "filters_sidebar",
          open = "desktop",
          width = 310,
          class = "dashboard-sidebar",

          div(
            class = "filter-header",
            div(
              class = "filter-header-left",
              bs_icon("funnel"),
              span("Filter controls")
            ),
            actionLink("clear_filters", "Clear filters", class = "clear-link")
          ),

          accordion(
            id = "filter_accordion",
            multiple = TRUE,
            open = c("Types"),

            accordion_panel(
              title = filter_title("diagram-3", "Interaction Types"),
              value = "Types",
              pickerInput(
                "type_filter",
                label = NULL,
                choices = type_choices,
                selected = type_choices,
                multiple = TRUE,
                options = picker_options
              )
            ),

            accordion_panel(
              title = filter_title("geo-alt", "Country / NAP context"),
              value = "Country",
              pickerInput(
                "country_filter",
                label = NULL,
                choices = country_choices,
                selected = country_choices,
                multiple = TRUE,
                options = picker_options
              )
            ),

            accordion_panel(
              title = filter_title("grid-1x2", "Themes"),
              value = "Theme",
              pickerInput(
                "theme_filter",
                label = NULL,
                choices = theme_choices,
                selected = theme_choices,
                multiple = TRUE,
                options = picker_options
              )
            ),

            accordion_panel(
              title = filter_title("activity", "Interaction effect"),
              value = "Effect",
              pickerInput(
                "effect_filter",
                label = NULL,
                choices = effect_choices,
                selected = effect_choices,
                multiple = TRUE,
                options = picker_options
              )
            ),

            accordion_panel(
              title = filter_title("patch-check", "Evidence level"),
              value = "Evidence",
              pickerInput(
                "evidence_filter",
                label = NULL,
                choices = evidence_choices,
                selected = evidence_choices,
                multiple = TRUE,
                options = picker_options
              )
            ),

            accordion_panel(
              title = filter_title("sliders", "Advanced filters"),
              value = "Advanced",
              prettySwitch(
                "context_only",
                label = "Context-dependent only",
                value = FALSE,
                status = "info",
                fill = TRUE
              ),
              prettySwitch(
                "bidirectional_only",
                label = "Bidirectional only",
                value = FALSE,
                status = "info",
                fill = TRUE
              ),
              pickerInput(
                "focus_nodes",
                label = "All policies",
                choices = objective_choices,
                selected = character(0),
                multiple = TRUE,
                options = picker_options
              ),
              textInput(
                "search_text",
                "Search policy tension / source",
                value = "",
                placeholder = "Type keyword"
              )
            )
          ),

          div(
            class = "sidebar-actions",
            actionButton(
              "apply_filters",
              "Apply filters",
              class = "btn btn-primary btn-apply"
            ),
            actionButton(
              "reset_filters_btn",
              "Reset",
              class = "btn btn-outline-primary btn-reset"
            ),
            downloadButton(
              "download_filtered_sidebar",
              "Download filtered",
              class = "btn btn-outline-primary btn-download"
            )
          )
        ),

        div(
          class = "main-content",
          uiOutput("dynamic_title"),

          div(
            class = "metrics-grid",
            metric_card("metric_synergy", "Synergies", "metric-synergy"),
            metric_card("metric_tradeoff", "Trade-offs", "metric-tradeoff"),
            metric_card("metric_mixed", "Mixed", "metric-mixed"),
            metric_card("metric_total", "Total interactions", "metric-total")
          ),

          div(
            class = "network-card",
            div(
              class = "network-toolbar",
              div(
                class = "network-toolbar-controls",
                div(
                  class = "network-filter-group",
                  tags$label(
                    class = "network-filter-label",
                    `for` = "source_view",
                    "Policy layer"
                  ),
                  selectInput(
                    "source_view",
                    label = NULL,
                    choices = c(
                      "Objective only" = "Objective",
                      "Implementation only" = "Implementation",
                      "All layers" = "All"
                    ),
                    selected = "Objective",
                    width = "100%"
                  )
                ),
                div(
                  class = "network-filter-group",
                  tags$label(
                    class = "network-filter-label",
                    `for` = "network_focus_node",
                    "Focus objective"
                  ),
                  selectInput(
                    "network_focus_node",
                    label = NULL,
                    choices = network_focus_choices,
                    selected = "",
                    width = "100%"
                  )
                )
              )
            ),
            div(
              class = "network-stage",
              shinycssloaders::withSpinner(
                visNetworkOutput("interaction_network", height = "620px"),
                type = 4,
                color = "#A4343A"
              ),
              uiOutput("node_drawer")
            )
          ),

          div(
            class = "legend-pills",
            span(class = "legend-pill legend-line-solid", "Direct"),
            span(class = "legend-pill legend-line-dashed", "Indirect"),
            span(class = "legend-pill legend-synergy", "Synergy"),
            span(class = "legend-pill legend-tradeoff", "Trade-off"),
            span(class = "legend-pill legend-mixed", "Mixed"),
            span(class = "legend-pill legend-amr", "AMR objective"),
            span(class = "legend-pill legend-sdg", "SDG goal")
          ),

          div(
            class = "data-card",
            h4("Filtered policy interactions"),
            shinycssloaders::withSpinner(
              DTOutput("interaction_table"),
              type = 4,
              color = "#A4343A"
            )
          ),

          div(
            class = "data-card",
            h4("Country summary in current filtered view"),
            shinycssloaders::withSpinner(
              DTOutput("country_summary_table"),
              type = 4,
              color = "#A4343A"
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "Objective Dictionary",
    div(
      class = "page-wrap dict-page",

      # ── Hero banner ──────────────────────────────────────────────
      div(
        class = "dict-hero",
        div(
          class = "dict-hero-inner",
          div(class = "dict-hero-eyebrow", bs_icon("journal-bookmark-fill"), " Reference"),
          h1(class = "dict-hero-title", "Objective Dictionary"),
          p(class = "dict-hero-subtitle",
            "A complete reference for all 5 WHO Global Action Plan AMR objectives and 17 UN Sustainable Development Goals used in this tool."
          )
        )
      ),

      # ── Stat chips ───────────────────────────────────────────────
      div(
        class = "dict-stats",
        div(
          class = "dict-stat",
          div(class = "dict-stat-num", "5"),
          div(class = "dict-stat-label", "AMR Objectives"),
          div(class = "dict-stat-sub", "WHO Global Action Plan")
        ),
        div(
          class = "dict-stat",
          div(class = "dict-stat-num", "17"),
          div(class = "dict-stat-label", "SDG Goals"),
          div(class = "dict-stat-sub", "UN 2030 Agenda")
        ),
        div(
          class = "dict-stat",
          div(class = "dict-stat-num", "22"),
          div(class = "dict-stat-label", "Total Objectives"),
          div(class = "dict-stat-sub", "Mapped in this tool")
        )
      ),

      # ── Objectives table ─────────────────────────────────────────
      div(
        class = "dict-section",
        div(
          class = "dict-section-header",
          div(
            div(class = "dict-section-label", bs_icon("list-columns-reverse"), " Objectives"),
            h2(class = "dict-section-title", "Full objective dictionary")
          ),
          p(class = "dict-section-desc", "Browse all AMR and SDG objectives with their codes, framework source, and full descriptions.")
        ),
        div(
          class = "dict-table-card",
          shinycssloaders::withSpinner(
            DTOutput("node_table"),
            type = 4,
            color = "#A4343A"
          )
        )
      ),

      # ── Summary table ────────────────────────────────────────────
      div(
        class = "dict-section",
        div(
          class = "dict-section-header",
          div(
            div(class = "dict-section-label", bs_icon("bar-chart-line-fill"), " Summary"),
            h2(class = "dict-section-title", "Interaction counts by country and effect")
          ),
          p(class = "dict-section-desc", "Aggregated counts of interactions across countries, types, and effect categories in the full dataset.")
        ),
        div(
          class = "dict-table-card",
          shinycssloaders::withSpinner(
            DTOutput("summary_table"),
            type = 4,
            color = "#A4343A"
          )
        )
      )
    )
  ),

  nav_panel(
    "User Guide",
    div(
      class = "page-wrap guide-page",

      # ── Hero section ──────────────────────────────────────────────────
      div(
        class = "g-hero",
        div(
          class = "g-hero-inner",
          div(class = "g-hero-eyebrow", bs_icon("globe2"), " BRIDGE-ABR Policy Intelligence"),
          h1(class = "g-hero-title", "Welcome to the Policy Tension Explorer"),
          p(class = "g-hero-subtitle",
            "Map synergies and trade-offs between AMR objectives and Sustainable Development Goals across national action plans."
          ),
          div(
            class = "g-hero-cta",
            span(class = "g-hero-start", bs_icon("arrow-down-circle"), " Scroll to get started"),
            div(
              class = "g-hero-pills",
              span(class = "g-pill g-pill-amr",   bs_icon("circle-fill"), " AMR \u2194 AMR"),
              span(class = "g-pill g-pill-sdg",   bs_icon("square-fill"), " SDG \u2194 SDG"),
              span(class = "g-pill g-pill-cross", bs_icon("shuffle"),     " AMR \u2194 SDG")
            )
          )
        )
      ),

      # ── What you can do — feature highlights ──────────────────────────
      div(
        class = "g-features",
        div(
          class = "g-feature",
          div(class = "g-feature-icon g-feature-icon-1", bs_icon("diagram-3-fill")),
          h3("Network View"),
          p("Interactive visual map. Nodes are objectives; edges show how they relate. Click, zoom, and drag.")
        ),
        div(
          class = "g-feature",
          div(class = "g-feature-icon g-feature-icon-2", bs_icon("speedometer2")),
          h3("Live Metrics"),
          p("Synergy, trade-off, and mixed counts update instantly as you filter.")
        ),
        div(
          class = "g-feature",
          div(class = "g-feature-icon g-feature-icon-3", bs_icon("table")),
          h3("Evidence Table"),
          p("Full records: country, layer, effect, source documents, and extracted policy text.")
        ),
        div(
          class = "g-feature",
          div(class = "g-feature-icon g-feature-icon-4", bs_icon("journal-bookmark-fill")),
          h3("Objective Dictionary"),
          p("Reference for all 5 WHO GAP AMR objectives and 17 UN SDGs with full descriptions.")
        )
      ),

      # ── Quick Start — 5-step onboarding ───────────────────────────────
      div(
        class = "g-section",
        div(class = "g-section-label", bs_icon("rocket-takeoff-fill"), " Quick Start"),
        h2(class = "g-section-title", "Get exploring in five steps"),
        p(class = "g-section-desc", "Follow this guided sequence the first time you use the app."),

        div(
          class = "g-timeline",

          div(
            class = "g-tl-item",
            div(class = "g-tl-badge", "1"),
            div(class = "g-tl-connector"),
            div(
              class = "g-tl-card",
              div(class = "g-tl-card-icon", bs_icon("geo-alt-fill")),
              div(
                class = "g-tl-card-body",
                h4("Choose a country"),
                p("Use the ", tags$strong("Country / NAP context"), " filter. Start with one country to keep the network readable.")
              )
            )
          ),

          div(
            class = "g-tl-item",
            div(class = "g-tl-badge", "2"),
            div(class = "g-tl-connector"),
            div(
              class = "g-tl-card",
              div(class = "g-tl-card-icon", bs_icon("layers-fill")),
              div(
                class = "g-tl-card-body",
                h4("Select a policy layer"),
                p(tags$em("Objective"), " shows strategic intent. ", tags$em("Implementation"), " shows operational practice. Or view both together.")
              )
            )
          ),

          div(
            class = "g-tl-item",
            div(class = "g-tl-badge", "3"),
            div(class = "g-tl-connector"),
            div(
              class = "g-tl-card",
              div(class = "g-tl-card-icon", bs_icon("funnel-fill")),
              div(
                class = "g-tl-card-body",
                h4("Filter by type, theme, and effect"),
                p("Narrow to specific interaction families, policy themes, or effect types, then press ", tags$strong("Apply filters"), ".")
              )
            )
          ),

          div(
            class = "g-tl-item",
            div(class = "g-tl-badge", "4"),
            div(class = "g-tl-connector"),
            div(
              class = "g-tl-card",
              div(class = "g-tl-card-icon", bs_icon("diagram-3-fill")),
              div(
                class = "g-tl-card-body",
                h4("Read the network"),
                p("Click any node to open its detail drawer. Use ", tags$strong("Focus objective"), " to spotlight one node and its connections.")
              )
            )
          ),

          div(
            class = "g-tl-item",
            div(class = "g-tl-badge", "5"),
            div(
              class = "g-tl-card",
              div(class = "g-tl-card-icon", bs_icon("table")),
              div(
                class = "g-tl-card-body",
                h4("Inspect the evidence table"),
                p("Scroll below the network to see interaction details: source documents, policy summaries, and original references.")
              )
            )
          )
        )
      ),

      # ── Visual Legend ─────────────────────────────────────────────────
      div(
        class = "g-section",
        div(class = "g-section-label", bs_icon("palette-fill"), " Visual Legend"),
        h2(class = "g-section-title", "How to read the network"),
        p(class = "g-section-desc", "Every visual element in the network carries meaning. Here's your decoder ring."),

        div(
          class = "g-legend-grid",

          # Nodes column
          div(
            class = "g-legend-panel",
            h3(class = "g-legend-panel-title", "Nodes"),
            div(
              class = "g-legend-item",
              div(class = "g-legend-swatch g-swatch-amr", "AMR"),
              div(
                class = "g-legend-detail",
                tags$strong("AMR Objectives"),
                p("Dark teal circles. Each represents one of the 5 WHO Global Action Plan objectives (AMR-01 to AMR-05).")
              )
            ),
            div(
              class = "g-legend-item",
              div(class = "g-legend-swatch g-swatch-sdg", "SDG"),
              div(
                class = "g-legend-detail",
                tags$strong("SDG Goals"),
                p("Coloured squares. One per UN Sustainable Development Goal (SDG-01 to SDG-17).")
              )
            )
          ),

          # Edge colours column
          div(
            class = "g-legend-panel",
            h3(class = "g-legend-panel-title", "Edge Colours"),
            div(
              class = "g-legend-item",
              div(class = "g-legend-line g-line-synergy"),
              div(
                class = "g-legend-detail",
                tags$strong("Synergy"),
                p("The two objectives reinforce each other.")
              )
            ),
            div(
              class = "g-legend-item",
              div(class = "g-legend-line g-line-tradeoff"),
              div(
                class = "g-legend-detail",
                tags$strong("Trade-off"),
                p("The two objectives are in tension.")
              )
            ),
            div(
              class = "g-legend-item",
              div(class = "g-legend-line g-line-mixed"),
              div(
                class = "g-legend-detail",
                tags$strong("Mixed"),
                p("Context-dependent; both reinforcing and conflicting.")
              )
            )
          ),

          # Line styles column
          div(
            class = "g-legend-panel",
            h3(class = "g-legend-panel-title", "Line Styles"),
            div(
              class = "g-legend-item",
              div(class = "g-legend-linestyle",
                tags$svg(width = "48", height = "12", viewBox = "0 0 48 12",
                  tags$line(x1 = "0", y1 = "6", x2 = "48", y2 = "6",
                    stroke = "#64748b", `stroke-width` = "2.5")
                )
              ),
              div(
                class = "g-legend-detail",
                tags$strong("Solid"),
                p("Direct interaction \u2014 explicitly linked in the policy document.")
              )
            ),
            div(
              class = "g-legend-item",
              div(class = "g-legend-linestyle",
                tags$svg(width = "48", height = "12", viewBox = "0 0 48 12",
                  tags$line(x1 = "0", y1 = "6", x2 = "48", y2 = "6",
                    stroke = "#64748b", `stroke-width` = "2.5", `stroke-dasharray` = "6 4")
                )
              ),
              div(
                class = "g-legend-detail",
                tags$strong("Dashed"),
                p("Indirect \u2014 mediated through a third factor.")
              )
            ),
            div(
              class = "g-legend-item",
              div(class = "g-legend-linestyle",
                tags$svg(width = "48", height = "12", viewBox = "0 0 48 12",
                  tags$line(x1 = "4", y1 = "6", x2 = "44", y2 = "6",
                    stroke = "#64748b", `stroke-width` = "2.5",
                    `marker-start` = "url(#arrowL)", `marker-end` = "url(#arrowR)"),
                  tags$defs(
                    tags$marker(id = "arrowR", markerWidth = "6", markerHeight = "6",
                      refX = "5", refY = "3", orient = "auto",
                      tags$path(d = "M0,0 L6,3 L0,6", fill = "#64748b")),
                    tags$marker(id = "arrowL", markerWidth = "6", markerHeight = "6",
                      refX = "1", refY = "3", orient = "auto",
                      tags$path(d = "M6,0 L0,3 L6,6", fill = "#64748b"))
                  )
                )
              ),
              div(
                class = "g-legend-detail",
                tags$strong("Bidirectional"),
                p("Both objectives influence each other.")
              )
            )
          )
        )
      ),

      # ── Policy Layers — accordion-style ────────────────────────────────
      div(
        class = "g-section",
        div(class = "g-section-label", bs_icon("stack"), " Reference"),
        h2(class = "g-section-title", "Understanding policy layers"),
        p(class = "g-section-desc", "Every interaction record is tagged to a policy layer. Switch layers with the dropdown above the network."),

        div(
          class = "g-layers",

          div(
            class = "g-layer-card g-layer-obj",
            div(class = "g-layer-marker"),
            div(
              class = "g-layer-content",
              div(class = "g-layer-header",
                span(class = "g-layer-tag g-tag-obj", "Objective"),
                h4("Strategic objectives layer")
              ),
              p("How high-level goals in AMR national action plans and SDG frameworks align, conflict, or interact at a conceptual level. Best for understanding overall policy architecture.")
            )
          ),

          div(
            class = "g-layer-card g-layer-impl",
            div(class = "g-layer-marker"),
            div(
              class = "g-layer-content",
              div(class = "g-layer-header",
                span(class = "g-layer-tag g-tag-impl", "Implementation"),
                h4("Implementation measures layer")
              ),
              p("How concrete interventions, institutions, and operational actions interact in practice. Captures on-the-ground realities and resource tensions.")
            )
          ),

          div(
            class = "g-layer-card g-layer-all",
            div(class = "g-layer-marker"),
            div(
              class = "g-layer-content",
              div(class = "g-layer-header",
                span(class = "g-layer-tag g-tag-all", "All layers"),
                h4("Combined view")
              ),
              p("Merges both layers into one network. Useful for full coverage but can be denser. Start with a single layer first.")
            )
          )
        ),

        div(
          class = "g-info-banner",
          bs_icon("info-circle-fill"),
          div(
            tags$strong("Interaction families"),
            p(
              tags$span(class = "g-inline-chip g-chip-amr", "AMR\u2013AMR"), " internal co-benefits or trade-offs within AMR. ",
              tags$span(class = "g-inline-chip g-chip-sdg", "SDG\u2013SDG"), " interactions between SDGs. ",
              tags$span(class = "g-inline-chip g-chip-cross", "AMR\u2013SDG"), " cross-framework interactions \u2014 the primary focus of this tool."
            )
          )
        )
      ),

      # ── Interpreting results — callout section ─────────────────────────
      div(
        class = "g-section",
        div(class = "g-section-label", bs_icon("shield-check"), " Interpretation"),
        h2(class = "g-section-title", "Interpreting results carefully"),
        p(class = "g-section-desc", "This tool is for exploration, not final conclusions. Keep these principles in mind."),

        div(
          class = "g-caution-banner",
          div(class = "g-caution-icon", bs_icon("exclamation-triangle-fill")),
          p("Always cross-check interaction summaries and source references in the evidence table before drawing conclusions.")
        ),

        div(
          class = "g-principles",
          div(
            class = "g-principle",
            div(class = "g-principle-num", "01"),
            p("A visible edge means at least one record links those objectives under current filters \u2014 it does not imply a universal or causal relationship.")
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "02"),
            p("The same pair may show different effects by country, layer, or theme. Use filters to isolate contexts.")
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "03"),
            p("Policy tension labels reflect the interpretation at time of analysis. Read them alongside the full interaction summary.")
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "04"),
            p("Bidirectional edges reinforce each other, but each direction may differ in strength \u2014 check the individual records.")
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "05"),
            p("Absence of an edge does not mean no interaction exists \u2014 it may simply not be captured in the current dataset.")
          )
        )
      ),

      # ── Tips & FAQ ────────────────────────────────────────────────────
      div(
        class = "g-section g-section-last",
        div(class = "g-section-label", bs_icon("lightbulb-fill"), " Tips"),
        h2(class = "g-section-title", "Power user tips"),

        div(
          class = "g-tips",
          div(
            class = "g-tip-card",
            div(class = "g-tip-icon-wrap g-tip-c1", bs_icon("funnel-fill")),
            tags$strong("Combine filters"),
            p("Country + theme + effect together for highly focused views.")
          ),
          div(
            class = "g-tip-card",
            div(class = "g-tip-icon-wrap g-tip-c2", bs_icon("search")),
            tags$strong("Free-text search"),
            p("Matches across summaries, tensions, countries, and references.")
          ),
          div(
            class = "g-tip-card",
            div(class = "g-tip-icon-wrap g-tip-c3", bs_icon("download")),
            tags$strong("Export CSV"),
            p("Download your filtered view for offline analysis.")
          ),
          div(
            class = "g-tip-card",
            div(class = "g-tip-icon-wrap g-tip-c4", bs_icon("hand-index-thumb")),
            tags$strong("Drag nodes"),
            p("Reposition nodes freely; use nav buttons to zoom or reset.")
          ),
          div(
            class = "g-tip-card",
            div(class = "g-tip-icon-wrap g-tip-c5", bs_icon("arrow-left-right")),
            tags$strong("Bidirectional only"),
            p("Toggle in Advanced filters for mutual-influence edges.")
          ),
          div(
            class = "g-tip-card",
            div(class = "g-tip-icon-wrap g-tip-c6", bs_icon("book-half")),
            tags$strong("Objective Dictionary"),
            p("Look up full names and descriptions for all 22 objectives.")
          )
        )
      )

    )
  ),

  nav_spacer(),
  nav_item(
    div(
      class = "navbar-actions",
      actionButton(
        "open_project_about",
        label = "About the project",
        icon = bs_icon("info-circle"),
        class = "about-trigger-btn",
        style = paste(
          "--bs-btn-color:#F5F7FA !important;",
          "--bs-btn-bg:#7F2A2F !important;",
          "--bs-btn-border-color:#7F2A2F !important;",
          "--bs-btn-hover-color:#F5F7FA !important;",
          "--bs-btn-hover-bg:#7F2A2F !important;",
          "--bs-btn-hover-border-color:#7F2A2F !important;",
          "--bs-btn-active-color:#F5F7FA !important;",
          "--bs-btn-active-bg:#7F2A2F !important;",
          "--bs-btn-active-border-color:#7F2A2F !important;",
          "color:#F5F7FA !important;",
          "background-color:#7F2A2F !important;",
          "border-color:#7F2A2F !important;"
        )
      )
    )
  )
)

# server.R ---------------------------------------------------------------

server <- function(input, output, session) {
  format_count <- function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  safely_update_network <- function(expr) {
    tryCatch(
      force(expr),
      error = function(e) invisible(NULL)
    )
  }

  normalize_node_selection <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return(NULL)
    }

    parsed <- value
    if (is.list(parsed)) {
      if (!is.null(parsed$id)) {
        parsed <- parsed$id
      } else if (!is.null(parsed$nodes) && length(parsed$nodes) > 0) {
        parsed <- parsed$nodes[[1]]
      } else {
        parsed <- unlist(parsed, recursive = TRUE, use.names = FALSE)
      }
    }

    parsed <- as.character(parsed[[1]])
    if (is.na(parsed)) {
      return(NULL)
    }

    parsed <- str_squish(parsed)
    if (!nzchar(parsed)) {
      return(NULL)
    }

    parsed
  }

  compact_selection <- function(values, all_values, limit = 3) {
    if (length(values) == 0) {
      return("None selected")
    }
    if (length(values) == length(all_values)) {
      return("All")
    }
    if (length(values) <= limit) {
      return(paste(values, collapse = ", "))
    }
    paste0(
      paste(head(values, limit), collapse = ", "),
      " +",
      length(values) - limit
    )
  }

  source_view <- reactive({
    current_value <- input$source_view
    if (is.null(current_value) || !nzchar(current_value)) {
      "Objective"
    } else {
      current_value
    }
  })

  default_filters <- list(
    type = type_choices,
    country = country_choices,
    theme = theme_choices,
    effect = effect_choices,
    evidence = evidence_choices,
    focus_nodes = character(0),
    context_only = FALSE,
    bidirectional_only = FALSE,
    search_text = ""
  )

  applied_filters <- reactiveValues(
    type = default_filters$type,
    country = default_filters$country,
    theme = default_filters$theme,
    effect = default_filters$effect,
    evidence = default_filters$evidence,
    focus_nodes = default_filters$focus_nodes,
    context_only = default_filters$context_only,
    bidirectional_only = default_filters$bidirectional_only,
    search_text = default_filters$search_text
  )

  selected_node <- reactiveVal(NULL)

  observeEvent(input$open_project_about, {
    showModal(
      modalDialog(
        class = "project-modal",
        size = "l",
        easyClose = TRUE,
        title = div(
          class = "project-modal-title",
          bs_icon("info-circle"),
          span("About BRIDGE-ABR")
        ),
        div(
          class = "project-modal-body",
          p(class = "project-description", project_description_text),
          div(
            class = "modal-logo-row",
            partner_logo(
              "uu_logo.png",
              "Uppsala University logo",
              partner_links$uu,
              "modal-logo-img"
            ),
            partner_logo(
              "ReAct_logo.png",
              "ReAct Europe logo",
              partner_links$react,
              "modal-logo-img"
            ),
            partner_logo(
              "SRC_logo.png",
              "Stockholm Resilience Centre logo",
              partner_links$src,
              "modal-logo-img"
            )
          )
        ),
        footer = modalButton("Close")
      )
    )
  })

  observeEvent(input$network_node_selected, {
    node_id <- normalize_node_selection(input$network_node_selected)
    selected_node(node_id)
    updateSelectInput(
      session,
      "network_focus_node",
      selected = if (is.null(node_id)) "" else node_id
    )
  })

  observeEvent(input$clear_selected_node, {
    selected_node(NULL)
    updateSelectInput(session, "network_focus_node", selected = "")
    safely_update_network({
      visNetworkProxy("interaction_network") %>% visUnselectAll() %>% visFit()
    })
  })

  observeEvent(
    input$network_focus_node,
    {
      node_id <- normalize_node_selection(input$network_focus_node)
      if (!is.null(node_id) && !node_id %in% nodes$id) {
        node_id <- NULL
      }

      selected_node(node_id)

      safely_update_network({
        proxy <- visNetworkProxy("interaction_network")
        if (is.null(node_id)) {
          proxy %>% visUnselectAll() %>% visFit()
        } else {
          proxy %>%
            visSelectNodes(id = node_id) %>%
            visFocus(
              id = node_id,
              scale = 1.08,
              animation = list(duration = 300)
            )
        }
      })
    },
    ignoreInit = FALSE
  )

  observeEvent(input$source_view, {
    selected_node(NULL)
    updateSelectInput(session, "network_focus_node", selected = "")
  })

  reset_filter_state <- function() {
    updatePickerInput(
      session,
      "type_filter",
      selected = default_filters$type
    )
    updatePickerInput(
      session,
      "country_filter",
      selected = default_filters$country
    )
    updatePickerInput(session, "theme_filter", selected = default_filters$theme)
    updatePickerInput(
      session,
      "effect_filter",
      selected = default_filters$effect
    )
    updatePickerInput(
      session,
      "evidence_filter",
      selected = default_filters$evidence
    )
    updatePickerInput(
      session,
      "focus_nodes",
      selected = default_filters$focus_nodes
    )
    updatePrettySwitch(
      session,
      "context_only",
      value = default_filters$context_only
    )
    updatePrettySwitch(
      session,
      "bidirectional_only",
      value = default_filters$bidirectional_only
    )
    updateTextInput(session, "search_text", value = default_filters$search_text)
    updateSelectInput(session, "source_view", selected = "Objective")
    updateSelectInput(session, "network_focus_node", selected = "")

    applied_filters$type <- default_filters$type
    applied_filters$country <- default_filters$country
    applied_filters$theme <- default_filters$theme
    applied_filters$effect <- default_filters$effect
    applied_filters$evidence <- default_filters$evidence
    applied_filters$focus_nodes <- default_filters$focus_nodes
    applied_filters$context_only <- default_filters$context_only
    applied_filters$bidirectional_only <- default_filters$bidirectional_only
    applied_filters$search_text <- default_filters$search_text
    selected_node(NULL)
  }

  observeEvent(input$apply_filters, {
    applied_filters$type <- input$type_filter
    applied_filters$country <- input$country_filter
    applied_filters$theme <- input$theme_filter
    applied_filters$effect <- input$effect_filter
    applied_filters$evidence <- input$evidence_filter
    applied_filters$focus_nodes <- input$focus_nodes
    applied_filters$context_only <- isTRUE(input$context_only)
    applied_filters$bidirectional_only <- isTRUE(input$bidirectional_only)
    applied_filters$search_text <- input$search_text
    selected_node(NULL)
  })

  observeEvent(input$clear_filters, {
    reset_filter_state()
  })

  observeEvent(input$reset_filters_btn, {
    reset_filter_state()
  })

  filtered_interactions <- reactive({
    df <- interactions_enriched

    if (length(applied_filters$type) > 0) {
      df <- df %>% filter(interaction_type %in% applied_filters$type)
    } else {
      df <- df[0, ]
    }

    if (length(applied_filters$country) > 0) {
      df <- df %>% filter(country %in% applied_filters$country)
    } else {
      df <- df[0, ]
    }

    if (length(applied_filters$theme) > 0) {
      df <- df %>% filter(theme %in% applied_filters$theme)
    } else {
      df <- df[0, ]
    }

    if (length(applied_filters$effect) > 0) {
      df <- df %>% filter(effect %in% applied_filters$effect)
    } else {
      df <- df[0, ]
    }

    if (length(applied_filters$evidence) > 0) {
      df <- df %>% filter(evidence_level %in% applied_filters$evidence)
    } else {
      df <- df[0, ]
    }

    if (!identical(source_view(), "All")) {
      df <- df %>% filter(source == source_view())
    }

    if (isTRUE(applied_filters$context_only)) {
      df <- df %>% filter(context_dependent)
    }

    if (isTRUE(applied_filters$bidirectional_only)) {
      df <- df %>% filter(bidirectional)
    }

    if (length(applied_filters$focus_nodes) > 0) {
      df <- df %>%
        filter(
          from %in%
            applied_filters$focus_nodes |
            to %in% applied_filters$focus_nodes
        )
    }

    search_term <- str_to_lower(str_trim(ifelse(
      is.null(applied_filters$search_text),
      "",
      applied_filters$search_text
    )))
    if (nchar(search_term) > 0) {
      df <- df %>% filter(str_detect(search_blob, fixed(search_term)))
    }

    df
  })

  output$dynamic_title <- renderUI({
    country_label <- compact_selection(applied_filters$country, country_choices)
    type_label <- compact_selection(applied_filters$type, type_choices)
    source_label <- if (identical(source_view(), "All")) {
      "Objective + Implementation"
    } else {
      source_view()
    }

    div(
      class = "title-block",
      h2("Policy interactions in national action plans"),
      p(
        tags$strong("Countries:"),
        paste(country_label),
        tags$span(" | "),
        tags$strong("Types:"),
        paste(type_label),
        tags$span(" | "),
        tags$strong("Source:"),
        source_label
      )
    )
  })

  output$metric_synergy <- renderText({
    format_count(sum(filtered_interactions()$effect == "Synergy (co-benefit)"))
  })

  output$metric_tradeoff <- renderText({
    format_count(sum(filtered_interactions()$effect == "Tension (trade-off)"))
  })

  output$metric_mixed <- renderText({
    format_count(sum(
      filtered_interactions()$effect == "Mixed / context-dependent"
    ))
  })

  output$metric_total <- renderText({
    format_count(nrow(filtered_interactions()))
  })

  edge_color_with_alpha <- function(effect_vec, mode_vec) {
    effect_norm <- ifelse(
      is.na(effect_vec),
      "Mixed / context-dependent",
      effect_vec
    )
    mode_norm <- ifelse(is.na(mode_vec), "Direct", mode_vec)
    base_cols <- unname(effect_palette[effect_norm])
    base_cols[is.na(base_cols)] <- "#94A3B8"
    alphas <- ifelse(mode_norm == "Indirect", 0.4, 0.9)
    mapply(
      function(col, alpha_val) grDevices::adjustcolor(col, alpha.f = alpha_val),
      base_cols,
      alphas,
      USE.NAMES = FALSE
    )
  }

  output$interaction_network <- renderVisNetwork({
    net <- tryCatch(
      {
        df <- filtered_interactions()
        active_nodes <- unique(c(df$from, df$to, applied_filters$focus_nodes))
        selected_id <- normalize_node_selection(selected_node())
        selected_id_safe <- if (is.null(selected_id)) {
          "__none__"
        } else {
          selected_id
        }

        plot_nodes <- nodes %>%
          mutate(
            is_sdg = node_group == "SDG",
            sdg_logo = unname(sdg_logo_map[id]),
            has_sdg_logo = is_sdg & !is.na(sdg_logo) & nzchar(sdg_logo),
            label_plot = short_label,
            title = paste0(
              "<b>",
              short_label,
              "</b><br>",
              label,
              "<br>",
              "Group: ",
              node_group,
              "<br>",
              "Framework: ",
              source_framework,
              "<br>",
              description
            ),
            active = id %in% active_nodes,
            is_selected = id == selected_id_safe,
            sdg_bg = unname(sdg_palette[id]),
            base_bg = ifelse(
              is_sdg,
              ifelse(is.na(sdg_bg), "#334155", sdg_bg),
              amr_color
            ),
            color.background = ifelse(active, base_bg, "#DDE4ED"),
            color.border = ifelse(
              is_selected,
              "#A4343A",
              ifelse(active, "#334155", "#A8B4C3")
            ),
            font.color = case_when(
              has_sdg_logo ~ "rgba(0,0,0,0)",
              active ~ "#FFFFFF",
              TRUE ~ "#6B7280"
            ),
            shape = case_when(
              node_group == "AMR" ~ "circle",
              has_sdg_logo ~ "image",
              TRUE ~ "square"
            ),
            image = ifelse(has_sdg_logo, sdg_logo, NA_character_),
            size = case_when(
              is_selected ~ ifelse(node_group == "AMR", 49, 45),
              node_group == "AMR" & active ~ 43,
              node_group == "AMR" ~ 37,
              active ~ 33,
              TRUE ~ 31
            )
          ) %>%
          transmute(
            id,
            label = label_plot,
            title,
            group = node_group,
            shape,
            image,
            size,
            color.background,
            color.border,
            font.color
          )

        if (nrow(df) > 0) {
          plot_edges <- df %>%
            mutate(
              effect_safe = ifelse(
                is.na(effect),
                "Mixed / context-dependent",
                effect
              ),
              direct_or_indirect_safe = ifelse(
                is.na(direct_or_indirect),
                "Direct",
                direct_or_indirect
              )
            ) %>%
            mutate(
              edge_color = edge_color_with_alpha(
                effect_safe,
                direct_or_indirect_safe
              ),
              dashes = case_when(
                effect_safe == "Mixed / context-dependent" ~ TRUE,
                direct_or_indirect_safe == "Indirect" ~ TRUE,
                TRUE ~ FALSE
              ),
              base_width = case_when(
                effect_safe == "Tension (trade-off)" ~ 3.2,
                effect_safe == "Synergy (co-benefit)" ~ 2.6,
                TRUE ~ 2.0
              ),
              width = ifelse(
                direct_or_indirect_safe == "Indirect",
                pmax(1.2, base_width - 1.0),
                base_width
              ),
              edge_title = paste0(
                "<b>",
                from_short,
                " -> ",
                to_short,
                "</b><br>",
                "Country: ",
                country,
                "<br>",
                "Theme: ",
                theme,
                "<br>",
                "type: ",
                interaction_type,
                "<br>",
                "Effect: ",
                effect,
                "<br>",
                "Policy tension: ",
                policy_tension,
                "<br>",
                "Summary: ",
                interaction_summary,
                "<br>",
                "Evidence: ",
                evidence_level,
                "<br>",
                "Reference: ",
                reference
              )
            ) %>%
            transmute(
              id = edge_id,
              from,
              to,
              title = edge_title,
              dashes,
              arrows = ifelse(bidirectional, "to;from", "to"),
              color = edge_color,
              width
            )
        } else {
          plot_edges <- data.frame(
            id = character(),
            from = character(),
            to = character(),
            title = character(),
            dashes = logical(),
            arrows = character(),
            color = character(),
            width = numeric()
          )
        }

        visNetwork(plot_nodes, plot_edges, width = "100%", height = "620px") %>%
          visNodes(
            borderWidth = 1.2,
            borderWidthSelected = 2.4,
            shadow = list(enabled = TRUE, size = 8, x = 1, y = 2),
            font = list(face = "Inter", size = 14),
            shapeProperties = list(
              useImageSize = FALSE,
              useBorderWithImage = TRUE
            ),
            chosen = list(
              label = htmlwidgets::JS(
                "function(values, id, selected, hovering) {
                  if (hovering) {
                    values.color = '#111827';
                    values.strokeColor = 'rgba(255, 255, 255, 0.92)';
                    values.strokeWidth = 5;
                  }
                }"
              )
            )
          ) %>%
          visEdges(
            smooth = list(enabled = TRUE, type = "dynamic", roundness = 0.25)
          ) %>%
          visLayout(randomSeed = 42, improvedLayout = TRUE) %>%
          visOptions(
            highlightNearest = list(enabled = TRUE, hover = TRUE)
          ) %>%
          visInteraction(
            hover = TRUE,
            navigationButtons = TRUE,
            tooltipDelay = 100
          ) %>%
          visEvents(
            selectNode = "function(params){ if(params.nodes.length){ Shiny.setInputValue('network_node_selected', params.nodes[0], {priority: 'event'}); } }",
            deselectNode = "function(params){ Shiny.setInputValue('network_node_selected', null, {priority: 'event'}); }"
          ) %>%
          visPhysics(
            solver = "forceAtlas2Based",
            stabilization = list(enabled = TRUE, iterations = 250, fit = TRUE)
          )
      },
      error = function(e) {
        e
      }
    )

    if (inherits(net, "error")) {
      validate(need(
        FALSE,
        paste("Unable to render network:", conditionMessage(net))
      ))
    }

    net
  })

  output$node_drawer <- renderUI({
    node_id <- selected_node()

    if (is.null(node_id) || !node_id %in% nodes$id) {
      return(
        div(
          class = "node-drawer",
          div(
            class = "drawer-placeholder",
            "Click any AMR or SDG node to inspect objective details and related interactions."
          )
        )
      )
    }

    node_info <- nodes %>% filter(id == node_id)
    related <- filtered_interactions() %>%
      filter(from == node_id | to == node_id) %>%
      arrange(desc(evidence_level))

    top_refs <- related %>%
      count(reference, sort = TRUE) %>%
      slice_head(n = 3)

    top_context <- related %>%
      count(country, theme, sort = TRUE) %>%
      slice_head(n = 4)

    tensions <- unique(related$policy_tension)
    tensions <- tensions[!is.na(tensions) & nzchar(tensions)]
    tensions <- head(tensions, 4)

    evidence_mix <- related %>%
      count(evidence_level, sort = TRUE)

    related_preview <- related %>%
      transmute(
        relation = paste0(from_short, " -> ", to_short),
        context = paste0(country, " | ", theme),
        evidence_level
      ) %>%
      slice_head(n = 6)

    div(
      class = "node-drawer open",
      div(
        class = "drawer-header drawer-header-brand",
        div(
          class = "drawer-title-wrap",
          span(class = "drawer-chip drawer-chip-brand", node_info$short_label),
          h4(node_info$label)
        ),
        actionLink("clear_selected_node", "Close", class = "drawer-close-link")
      ),
      p(class = "drawer-description", node_info$description),
      div(
        class = "drawer-stats",
        div(class = "drawer-stat", span("Interactions"), strong(nrow(related))),
        div(
          class = "drawer-stat",
          span("Countries"),
          strong(dplyr::n_distinct(related$country))
        ),
        div(
          class = "drawer-stat",
          span("Themes"),
          strong(dplyr::n_distinct(related$theme))
        )
      ),
      div(
        class = "drawer-section",
        h5("Top country context"),
        if (nrow(top_context) == 0) {
          p("No interactions under current filters.")
        } else {
          tags$ul(
            class = "drawer-list",
            lapply(seq_len(nrow(top_context)), function(i) {
              tags$li(paste0(
                top_context$country[i],
                " - ",
                top_context$theme[i],
                " (",
                top_context$n[i],
                ")"
              ))
            })
          )
        }
      ),
      div(
        class = "drawer-section",
        h5("Policy tensions in view"),
        if (length(tensions) == 0) {
          p("No policy tension text in current filters.")
        } else {
          tags$ul(
            class = "drawer-list",
            lapply(tensions, tags$li)
          )
        }
      ),
      div(
        class = "drawer-section",
        h5("Evidence mix"),
        if (nrow(evidence_mix) == 0) {
          p("No evidence labels in current filters.")
        } else {
          div(
            class = "evidence-chip-wrap",
            lapply(seq_len(nrow(evidence_mix)), function(i) {
              ev_raw <- evidence_mix$evidence_level[i]
              ev_cls <- paste0(
                "evidence-",
                tolower(gsub("[^A-Za-z]", "", ev_raw))
              )
              span(
                class = paste("evidence-chip", ev_cls),
                paste0(ev_raw, ": ", evidence_mix$n[i])
              )
            })
          )
        }
      ),
      div(
        class = "drawer-section",
        h5("Top references"),
        if (nrow(top_refs) == 0) {
          p("No references in current filters.")
        } else {
          tags$ul(
            class = "drawer-list",
            lapply(seq_len(nrow(top_refs)), function(i) {
              tags$li(paste0(top_refs$reference[i], " (", top_refs$n[i], ")"))
            })
          )
        }
      ),
      div(
        class = "drawer-section",
        h5("Related interactions"),
        if (nrow(related_preview) == 0) {
          p("No related interactions under current filters.")
        } else {
          div(
            class = "interaction-list",
            lapply(seq_len(nrow(related_preview)), function(i) {
              ev_raw <- related_preview$evidence_level[i]
              ev_cls <- paste0(
                "evidence-",
                tolower(gsub("[^A-Za-z]", "", ev_raw))
              )
              div(
                class = "interaction-item",
                div(class = "interaction-line", related_preview$relation[i]),
                div(
                  class = "interaction-meta",
                  span(
                    class = "interaction-context",
                    related_preview$context[i]
                  ),
                  span(class = paste("evidence-chip", ev_cls), ev_raw)
                )
              )
            })
          )
        }
      )
    )
  })

  output$interaction_table <- renderDT({
    tbl <- filtered_interactions() %>%
      transmute(
        `Edge ID` = edge_id,
        Country = country,
        Theme = theme,
        `Interaction Type` = interaction_type,
        `From Objective` = paste0(from_short, " - ", from_label),
        `To Objective` = paste0(to_short, " - ", to_label),
        Effect = effect,
        `Direct or Indirect` = direct_or_indirect,
        Bidirectional = ifelse(bidirectional, "Yes", "No"),
        `Policy Tension` = policy_tension,
        `Interaction Summary` = interaction_summary,
        `Policy Objective 1` = if_else(is.na(policy_strategic_objective_1), "", policy_strategic_objective_1),
        `Policy Objective 2` = if_else(is.na(policy_strategic_objective_2), "", policy_strategic_objective_2),
        `Specific Actions 1` = if_else(is.na(policy_specific_actions_1),    "", policy_specific_actions_1),
        `Specific Actions 2` = if_else(is.na(policy_specific_actions_2),    "", policy_specific_actions_2),
        Reference = reference
      )

    datatable(
      tbl,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$country_summary_table <- renderDT({
    tbl <- filtered_interactions() %>%
      count(country, interaction_type, effect, theme, sort = TRUE) %>%
      rename(
        Country = country,
        `Interaction Type` = interaction_type,
        Effect = effect,
        Theme = theme,
        Count = n
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = 8, lengthMenu = c(8, 20, 40), scrollX = TRUE)
    )
  })

  output$node_table <- renderDT({
    tbl <- nodes %>%
      transmute(
        `Node ID` = id,
        `Objective Code` = short_label,
        `Objective Label` = label,
        `Node Group` = node_group,
        `Node Type` = node_type,
        Framework = source_framework,
        Description = description
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(
        pageLength = 12,
        lengthMenu = c(12, 25, 50),
        scrollX = TRUE
      )
    )
  })

  output$summary_table <- renderDT({
    tbl <- interactions_enriched %>%
      count(country, interaction_type, effect, sort = TRUE) %>%
      rename(
        Country = country,
        `Interaction Type` = interaction_type,
        Effect = effect,
        `Number of Interactions` = n
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(
        pageLength = 12,
        lengthMenu = c(12, 25, 50),
        scrollX = TRUE
      )
    )
  })

  download_filtered_content <- function(file) {
    out <- filtered_interactions() %>%
      transmute(
        edge_id,
        country,
        theme,
        interaction_type,
        from,
        from_short,
        from_label,
        to,
        to_short,
        to_label,
        effect,
        direct_or_indirect,
        bidirectional,
        policy_tension,
        interaction_summary,
        policy_strategic_objective_1,
        policy_strategic_objective_2,
        policy_specific_actions_1,
        policy_specific_actions_2,
        reference
      )
    write_csv(out, file)
  }

  output$download_filtered_sidebar <- downloadHandler(
    filename = function() {
      paste0("policy_tension_filtered_", Sys.Date(), ".csv")
    },
    content = download_filtered_content
  )

  output$download_filtered_tab <- downloadHandler(
    filename = function() {
      paste0("policy_tension_filtered_", Sys.Date(), ".csv")
    },
    content = download_filtered_content
  )

  output$download_all_interactions <- downloadHandler(
    filename = function() {
      paste0("policy_tension_all_interactions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(interactions, file)
    }
  )

  output$download_all_nodes <- downloadHandler(
    filename = function() {
      paste0("policy_tension_objective_dictionary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(nodes, file)
    }
  )
}

# App Launching ----------------------------------------------------------

shinyApp(ui = ui, server = server)
