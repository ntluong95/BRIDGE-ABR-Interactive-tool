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
      c("Align", "Synergy") ~ "Synergy",
    effect_value %in%
      c("Conflict", "Tension (trade-off)") ~ "Conflict",
    effect_value %in% c("Independent") ~ "Independent",
    effect_value %in% c("Both synergy and conflict") ~
      "Both synergy and conflict",
    TRUE ~ effect_value
  )
}

classify_network_effect <- function(effect_value) {
  effect_norm <- normalize_effect(effect_value)
  has_synergy <- any(effect_norm %in% c("Synergy"), na.rm = TRUE)
  has_non_synergy <- any(
    effect_norm %in% c("Conflict", "Independent"),
    na.rm = TRUE
  )
  has_explicit_mixed <- any(
    effect_norm %in% c("Both synergy and conflict"),
    na.rm = TRUE
  )

  case_when(
    has_explicit_mixed ~ "Both synergy and conflict",
    has_synergy & has_non_synergy ~ "Both synergy and conflict",
    has_synergy ~ "Synergy",
    has_non_synergy ~ "Non-synergy",
    TRUE ~ "Both synergy and conflict"
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
    theme = str_replace_all(theme, " and ", " & "),
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
  "policy_strategic_objective_1",
  "policy_strategic_objective_2",
  "policy_specific_actions_1",
  "policy_specific_actions_2"
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
        if_else(
          is.na(policy_strategic_objective_1),
          "",
          policy_strategic_objective_1
        ),
        if_else(
          is.na(policy_strategic_objective_2),
          "",
          policy_strategic_objective_2
        ),
        if_else(
          is.na(policy_specific_actions_1),
          "",
          policy_specific_actions_1
        ),
        if_else(is.na(policy_specific_actions_2), "", policy_specific_actions_2)
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
  "Synergy",
  "Conflict",
  "Independent",
  "Both synergy and conflict"
)
effect_choices <- intersect(
  effect_choices,
  sort(unique(interactions_enriched$effect))
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
  "Non-synergy" = "#C62828",
  "Synergy" = "#2E7D32",
  "Both synergy and conflict" = "#F9A825"
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

build_amr_logo_map <- function(www_dir = "www") {
  logo_files <- list.files(
    www_dir,
    pattern = "(?i)\\.(png|svg|jpg|jpeg|webp)$"
  )
  logo_num <- stringr::str_match(
    logo_files,
    "(?i)AMR[^0-9]*([0-9]{1,2})"
  )[, 2]
  logo_num <- suppressWarnings(as.integer(logo_num))
  valid <- !is.na(logo_num) & logo_num >= 1 & logo_num <= 5
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
  logo_df <- logo_df[order(logo_df$num, logo_df$ext_rank), ]
  logo_df <- logo_df[!duplicated(logo_df$num), , drop = FALSE]
  stats::setNames(logo_df$file, sprintf("amr%02d", logo_df$num))
}

amr_logo_map <- build_amr_logo_map("www")

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
      tags$span(class = "app-brand-kicker", "BRIDGE-ABR"),
      tags$span(class = "app-brand-title", "ABR Goal Conflict Explorer")
    )
  ),
  id = "main_nav",
  window_title = "ABR Goal Conflict Explorer",
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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=15")
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
            open = c("NetworkView", "Theme", "Types"),

            accordion_panel(
              title = filter_title("diagram-2", "Network view"),
              value = "NetworkView",
              div(
                class = "network-filter-group",
                tags$label(
                  class = "network-filter-label",
                  `for` = "source_view",
                  "Policy layer"
                ),
                pickerInput(
                  "source_view",
                  label = NULL,
                  choices = c(
                    "Objective only" = "Objective",
                    "Implementation only" = "Implementation",
                    "All layers" = "All"
                  ),
                  selected = character(0),
                  multiple = FALSE,
                  options = modifyList(
                    picker_options,
                    list(title = "All layers (default)")
                  ),
                  width = "100%"
                )
              )
            ),

            accordion_panel(
              title = filter_title("grid-1x2", "Themes"),
              value = "Theme",
              pickerInput(
                "theme_filter",
                label = NULL,
                choices = theme_choices,
                selected = character(0),
                multiple = TRUE,
                options = picker_options
              )
            ),

            accordion_panel(
              title = filter_title("diagram-3", "Interaction types & effect"),
              value = "Types",
              tags$label(class = "network-filter-label", "Interaction types"),
              pickerInput(
                "type_filter",
                label = NULL,
                choices = type_choices,
                selected = character(0),
                multiple = TRUE,
                options = picker_options
              ),
              tags$label(
                class = "network-filter-label",
                style = "margin-top:8px;",
                "Interaction effect"
              ),
              pickerInput(
                "effect_filter",
                label = NULL,
                choices = effect_choices,
                selected = character(0),
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
                selected = character(0),
                multiple = TRUE,
                options = picker_options
              )
            ),
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
            metric_card("metric_tradeoff", "Conflicts", "metric-tradeoff"),
            metric_card(
              "metric_mixed",
              "Independent",
              "metric-independent"
            ),
            metric_card("metric_total", "Total interactions", "metric-total")
          ),

          div(
            class = "legend-pills",
            span(class = "legend-pill legend-synergy", "Synergy"),
            span(class = "legend-pill legend-tradeoff", "Non-synergy edge"),
            span(
              class = "legend-pill legend-mixed",
              "Mixed edge"
            ),
            span(class = "legend-pill legend-amr", "AMR objective"),
            span(class = "legend-pill legend-sdg", "SDG goal")
          ),

          div(
            class = "network-card",
            div(
              class = "network-stage",
              shinycssloaders::withSpinner(
                visNetworkOutput("interaction_network", height = "750px"),
                type = 4,
                color = "#A4343A"
              ),
              uiOutput("node_drawer")
            )
          ),

          conditionalPanel(
            condition = "output.edges_visible",
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
          div(
            class = "dict-hero-eyebrow",
            bs_icon("journal-bookmark-fill"),
            " Reference"
          ),
          h1(class = "dict-hero-title", "Objective Dictionary"),
          p(
            class = "dict-hero-subtitle",
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
            div(
              class = "dict-section-label",
              bs_icon("list-columns-reverse"),
              " Objectives"
            ),
            h2(class = "dict-section-title", "Full objective dictionary")
          ),
          p(
            class = "dict-section-desc",
            "Browse all AMR and SDG objectives with their codes, framework source, and full descriptions."
          )
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
            div(
              class = "dict-section-label",
              bs_icon("bar-chart-line-fill"),
              " Summary"
            ),
            h2(
              class = "dict-section-title",
              "Interaction counts by country and effect"
            )
          ),
          p(
            class = "dict-section-desc",
            "Aggregated counts of interactions across countries, types, and effect categories in the full dataset."
          )
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
          div(
            class = "g-hero-eyebrow",
            bs_icon("globe2"),
            " BRIDGE-ABR BRIDGE-ABR"
          ),
          h1(
            class = "g-hero-title",
            "Welcome to the ABR Goal Conflict Explorer"
          ),
          p(
            class = "g-hero-subtitle",
            "Map synergies and conflicts between AMR objectives and Sustainable Development Goals across national action plans."
          ),
          div(
            class = "g-hero-cta",
            span(
              class = "g-hero-start",
              bs_icon("arrow-down-circle"),
              " Scroll to get started"
            ),
            div(
              class = "g-hero-pills",
              span(
                class = "g-pill g-pill-amr",
                bs_icon("circle-fill"),
                " AMR \u2194 AMR"
              ),
              span(
                class = "g-pill g-pill-sdg",
                bs_icon("square-fill"),
                " SDG \u2194 SDG"
              ),
              span(
                class = "g-pill g-pill-cross",
                bs_icon("shuffle"),
                " AMR \u2194 SDG"
              )
            )
          )
        )
      ),

      # ── What you can do — feature highlights ──────────────────────────
      div(
        class = "g-features",
        div(
          class = "g-feature",
          div(
            class = "g-feature-icon g-feature-icon-1",
            bs_icon("diagram-3-fill")
          ),
          h3("Network View"),
          p(
            "Interactive visual map. Nodes are objectives; edges show how they relate. Click, zoom, and drag."
          )
        ),
        div(
          class = "g-feature",
          div(
            class = "g-feature-icon g-feature-icon-2",
            bs_icon("speedometer2")
          ),
          h3("Live Metrics"),
          p(
            "Synergy, conflict, and both-synergy-and-conflict counts update instantly as you apply filters."
          )
        ),
        div(
          class = "g-feature",
          div(class = "g-feature-icon g-feature-icon-3", bs_icon("table")),
          h3("Evidence Table"),
          p(
            "Full records: country, layer, effect, source documents, and extracted policy text."
          )
        ),
        div(
          class = "g-feature",
          div(
            class = "g-feature-icon g-feature-icon-4",
            bs_icon("journal-bookmark-fill")
          ),
          h3("Objective Dictionary"),
          p(
            "Reference for all 5 WHO GAP AMR objectives and 17 UN SDGs with full descriptions."
          )
        )
      ),

      # ── Quick Start — 5-step onboarding ───────────────────────────────
      div(
        class = "g-section",
        div(
          class = "g-section-label",
          bs_icon("rocket-takeoff-fill"),
          " Quick Start"
        ),
        h2(class = "g-section-title", "Get exploring in five steps"),
        p(
          class = "g-section-desc",
          "Follow this guided sequence the first time you use the app."
        ),

        div(
          class = "g-timeline",

          div(
            class = "g-tl-item",
            div(class = "g-tl-badge", "1"),
            div(class = "g-tl-connector"),
            div(
              class = "g-tl-card",
              div(class = "g-tl-card-icon", bs_icon("layers-fill")),
              div(
                class = "g-tl-card-body",
                h4("Select a policy layer"),
                p(
                  "Open the ",
                  tags$strong("Network view"),
                  " panel in the left sidebar. Choose ",
                  tags$em("Objective only"),
                  " for strategic intent, ",
                  tags$em("Implementation only"),
                  " for operational practice, or leave blank to see all layers."
                )
              )
            )
          ),

          div(
            class = "g-tl-item",
            div(class = "g-tl-badge", "2"),
            div(class = "g-tl-connector"),
            div(
              class = "g-tl-card",
              div(class = "g-tl-card-icon", bs_icon("grid-1x2-fill")),
              div(
                class = "g-tl-card-body",
                h4("Pick a theme"),
                p(
                  "Use the ",
                  tags$strong("Themes"),
                  " filter to focus on a policy area such as Agriculture, Health Systems, or Environment."
                )
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
                h4("Filter by interaction type and effect"),
                p(
                  "Use the combined ",
                  tags$strong("Interaction types & effect"),
                  " panel to narrow by family (AMR\u2013SDG, etc.) and effect (Synergy, Conflict, Independent, or Both synergy and conflict), then press ",
                  tags$strong("Apply filters"),
                  "."
                )
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
                p(
                  "Click any node to open its detail drawer and see all related interactions. Drag nodes to rearrange; use the navigation buttons to zoom or fit the view."
                )
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
                p(
                  "Scroll below the network to see interaction details: source documents, policy summaries, and original references."
                )
              )
            )
          )
        )
      ),

      # ── Visual Legend ─────────────────────────────────────────────────
      div(
        class = "g-section",
        div(
          class = "g-section-label",
          bs_icon("palette-fill"),
          " Visual Legend"
        ),
        h2(class = "g-section-title", "How to read the network"),
        p(
          class = "g-section-desc",
          "Every visual element in the network carries meaning. Here's your decoder ring."
        ),

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
                p(
                  "Dark circular icons with a number and name. Each represents one of the 5 WHO Global Action Plan objectives."
                )
              )
            ),
            div(
              class = "g-legend-item",
              div(class = "g-legend-swatch g-swatch-sdg", "SDG"),
              div(
                class = "g-legend-detail",
                tags$strong("SDG Goals"),
                p(
                  "Official coloured SDG icons. One per UN Sustainable Development Goal (SDG-01 to SDG-17)."
                )
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
                tags$strong("Non-synergy edge"),
                p(
                  "Only conflict and/or independent relationships are present in the current view."
                )
              )
            ),
            div(
              class = "g-legend-item",
              div(class = "g-legend-line g-line-mixed"),
              div(
                class = "g-legend-detail",
                tags$strong("Mixed edge"),
                p(
                  "At least one synergy and one non-synergy relationship are present in the current view."
                )
              )
            )
          ),

          div(
            class = "g-legend-panel",
            h3(class = "g-legend-panel-title", "Connections"),
            div(
              class = "g-legend-item",
              div(
                class = "g-legend-line g-line-guide",
                tags$svg(
                  width = "28",
                  height = "12",
                  viewBox = "0 0 28 12",
                  tags$line(
                    x1 = "2",
                    y1 = "6",
                    x2 = "26",
                    y2 = "6",
                    stroke = "#64748b",
                    `stroke-width` = "2.5",
                    `stroke-dasharray` = "6 4",
                    `stroke-linecap` = "round"
                  )
                )
              ),
              div(
                class = "g-legend-detail",
                tags$strong("Objective layer"),
                p(
                  "Dashed edges represent objective-level interactions."
                )
              )
            ),
            div(
              class = "g-legend-item",
              div(
                class = "g-legend-line g-line-guide",
                tags$svg(
                  width = "28",
                  height = "12",
                  viewBox = "0 0 28 12",
                  tags$line(
                    x1 = "2",
                    y1 = "6",
                    x2 = "26",
                    y2 = "6",
                    stroke = "#64748b",
                    `stroke-width` = "2.5",
                    `stroke-linecap` = "round"
                  )
                )
              ),
              div(
                class = "g-legend-detail",
                tags$strong("Implementation layer"),
                p(
                  "Solid edges represent implementation-level interactions. "
                )
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
        p(
          class = "g-section-desc",
          "Every interaction record is tagged to a policy layer. Switch layers using the Policy layer dropdown in the left sidebar under Network view."
        ),

        div(
          class = "g-layers",

          div(
            class = "g-layer-card g-layer-obj",
            div(class = "g-layer-marker"),
            div(
              class = "g-layer-content",
              div(
                class = "g-layer-header",
                span(class = "g-layer-tag g-tag-obj", "Objective"),
                h4("Strategic objectives layer")
              ),
              p(
                "How high-level goals in AMR national action plans and SDG frameworks align, conflict, or interact at a conceptual level. Best for understanding overall policy architecture."
              )
            )
          ),

          div(
            class = "g-layer-card g-layer-impl",
            div(class = "g-layer-marker"),
            div(
              class = "g-layer-content",
              div(
                class = "g-layer-header",
                span(class = "g-layer-tag g-tag-impl", "Implementation"),
                h4("Implementation measures layer")
              ),
              p(
                "How concrete interventions, institutions, and operational actions interact in practice. Captures on-the-ground realities and resource tensions."
              )
            )
          ),

          div(
            class = "g-layer-card g-layer-all",
            div(class = "g-layer-marker"),
            div(
              class = "g-layer-content",
              div(
                class = "g-layer-header",
                span(class = "g-layer-tag g-tag-all", "All layers"),
                h4("Combined view")
              ),
              p(
                "Merges both layers into one network. Useful for full coverage but can be denser. Start with a single layer first."
              )
            )
          )
        ),

        div(
          class = "g-info-banner",
          bs_icon("info-circle-fill"),
          div(
            tags$strong("Interaction families"),
            p(
              tags$span(class = "g-inline-chip g-chip-amr", "AMR\u2013AMR"),
              " internal synergies or conflicts within AMR. ",
              tags$span(class = "g-inline-chip g-chip-sdg", "SDG\u2013SDG"),
              " interactions between SDGs. ",
              tags$span(class = "g-inline-chip g-chip-cross", "AMR\u2013SDG"),
              " cross-framework interactions \u2014 the primary focus of this tool."
            )
          )
        )
      ),

      # ── Interpreting results — callout section ─────────────────────────
      div(
        class = "g-section",
        div(
          class = "g-section-label",
          bs_icon("shield-check"),
          " Interpretation"
        ),
        h2(class = "g-section-title", "Interpreting results carefully"),
        p(
          class = "g-section-desc",
          "This tool is for exploration, not final conclusions. Keep these principles in mind."
        ),

        div(
          class = "g-caution-banner",
          div(class = "g-caution-icon", bs_icon("exclamation-triangle-fill")),
          p(
            "Always cross-check interaction summaries and source references in the evidence table before drawing conclusions."
          )
        ),

        div(
          class = "g-principles",
          div(
            class = "g-principle",
            div(class = "g-principle-num", "01"),
            p(
              "A visible edge means at least one record links those objectives under current filters \u2014 it does not imply a universal or causal relationship."
            )
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "02"),
            p(
              "The same pair may show different effects by country, layer, or theme. Use filters to isolate contexts."
            )
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "03"),
            p(
              "Policy tension labels reflect the interpretation at time of analysis. Read them alongside the full interaction summary."
            )
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "04"),
            p(
              "Line style shows policy layer: dashed edges are objective-level interactions and solid edges are implementation-level interactions."
            )
          ),
          div(
            class = "g-principle",
            div(class = "g-principle-num", "05"),
            p(
              "Absence of an edge does not mean no interaction exists \u2014 it may simply not be captured in the current dataset."
            )
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
            div(
              class = "g-tip-icon-wrap g-tip-c4",
              bs_icon("hand-index-thumb")
            ),
            tags$strong("Drag nodes"),
            p("Reposition nodes freely; use nav buttons to zoom or reset.")
          ),
          div(
            class = "g-tip-card",
            div(
              class = "g-tip-icon-wrap g-tip-c5",
              bs_icon("geo-alt-fill")
            ),
            tags$strong("Compare countries"),
            p(
              "Apply a country filter, note the metrics, then switch to another country to compare patterns."
            )
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
    if (length(values) == 0 || length(values) == length(all_values)) {
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
    if (
      is.null(current_value) ||
        length(current_value) == 0 ||
        !nzchar(current_value)
    ) {
      "All"
    } else {
      current_value
    }
  })

  default_filters <- list(
    type = character(0),
    country = character(0),
    theme = character(0),
    effect = character(0)
  )

  applied_filters <- reactiveValues(
    type = default_filters$type,
    country = default_filters$country,
    theme = default_filters$theme,
    effect = default_filters$effect
  )

  selected_node <- reactiveVal(NULL)
  active_interaction_summary <- reactiveVal(NULL)
  show_edges <- reactiveVal(FALSE)

  output$edges_visible <- reactive({
    show_edges()
  })
  outputOptions(output, "edges_visible", suspendWhenHidden = FALSE)

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
              "react_logo.png",
              "ReAct Europe logo",
              partner_links$react,
              "modal-logo-img"
            ),
            partner_logo(
              "src_logo.png",
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
    active_interaction_summary(NULL)
    updateSelectInput(
      session,
      "network_focus_node",
      selected = if (is.null(node_id)) "" else node_id
    )
  })

  observeEvent(input$clear_selected_node, {
    selected_node(NULL)
    active_interaction_summary(NULL)
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
      active_interaction_summary(NULL)

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
    active_interaction_summary(NULL)
    updateSelectInput(session, "network_focus_node", selected = "")
  })

  observeEvent(
    input$drawer_summary_info,
    {
      selected_key <- input$drawer_summary_info
      current_key <- active_interaction_summary()
      if (identical(selected_key, current_key)) {
        active_interaction_summary(NULL)
      } else {
        active_interaction_summary(selected_key)
      }
    },
    ignoreInit = TRUE
  )

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
    updatePickerInput(session, "source_view", selected = character(0))
    updateSelectInput(session, "network_focus_node", selected = "")

    applied_filters$type <- default_filters$type
    applied_filters$country <- default_filters$country
    applied_filters$theme <- default_filters$theme
    applied_filters$effect <- default_filters$effect
    selected_node(NULL)
    active_interaction_summary(NULL)
    show_edges(FALSE)
  }

  observeEvent(input$apply_filters, {
    applied_filters$type <- input$type_filter
    applied_filters$country <- input$country_filter
    applied_filters$theme <- input$theme_filter
    applied_filters$effect <- input$effect_filter
    selected_node(NULL)
    active_interaction_summary(NULL)
    show_edges(TRUE)
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
    }

    if (length(applied_filters$country) > 0) {
      df <- df %>% filter(country %in% applied_filters$country)
    }

    if (length(applied_filters$theme) > 0) {
      df <- df %>% filter(theme %in% applied_filters$theme)
    }

    if (length(applied_filters$effect) > 0) {
      df <- df %>% filter(effect %in% applied_filters$effect)
    }

    if (!identical(source_view(), "All")) {
      df <- df %>% filter(source == source_view())
    }

    df
  })

  displayed_interactions <- reactive({
    if (!show_edges()) filtered_interactions()[0, ] else filtered_interactions()
  })

  table_interactions <- reactive({
    df <- displayed_interactions()
    node_id <- normalize_node_selection(selected_node())

    if (is.null(node_id) || !node_id %in% nodes$id) {
      return(df)
    }

    df %>% filter(from == node_id | to == node_id)
  })

  output$dynamic_title <- renderUI({
    chips <- list()

    sv <- source_view()
    if (!identical(sv, "All")) {
      layer_label <- switch(
        sv,
        "Objective" = "Objective only",
        "Implementation" = "Implementation only",
        sv
      )
      chips[["Policy layer"]] <- layer_label
    }
    if (length(applied_filters$theme) > 0) {
      chips[["Theme"]] <- compact_selection(
        applied_filters$theme,
        theme_choices
      )
    }
    if (length(applied_filters$type) > 0) {
      chips[["Type"]] <- compact_selection(applied_filters$type, type_choices)
    }
    if (length(applied_filters$effect) > 0) {
      chips[["Effect"]] <- compact_selection(
        applied_filters$effect,
        effect_choices
      )
    }
    if (length(applied_filters$country) > 0) {
      chips[["Country"]] <- compact_selection(
        applied_filters$country,
        country_choices
      )
    }

    subtitle <- if (length(chips) == 0) {
      div(
        class = "filter-summary-none",
        "No filters applied \u2014 showing no interactions"
      )
    } else {
      chip_tags <- lapply(names(chips), function(k) {
        span(
          class = "filter-chip",
          span(class = "filter-chip-key", k),
          span(class = "filter-chip-val", chips[[k]])
        )
      })
      div(class = "filter-chips-row", chip_tags)
    }

    div(
      class = "title-block",
      h2("Policy interactions in National Action Plans"),
      subtitle
    )
  })

  output$metric_synergy <- renderText({
    format_count(sum(displayed_interactions()$effect == "Synergy"))
  })

  output$metric_tradeoff <- renderText({
    format_count(sum(displayed_interactions()$effect == "Conflict"))
  })

  output$metric_mixed <- renderText({
    format_count(sum(displayed_interactions()$effect == "Independent"))
  })

  output$metric_total <- renderText({
    format_count(nrow(displayed_interactions()))
  })

  edge_color_with_alpha <- function(effect_vec) {
    effect_norm <- ifelse(
      is.na(effect_vec),
      "Both synergy and conflict",
      effect_vec
    )
    base_cols <- unname(effect_palette[effect_norm])
    base_cols[is.na(base_cols)] <- "#94A3B8"
    unname(grDevices::adjustcolor(base_cols, alpha.f = 0.9))
  }

  output$interaction_network <- renderVisNetwork({
    net <- tryCatch(
      {
        df <- filtered_interactions()
        active_nodes <- if (!show_edges()) {
          nodes$id
        } else {
          unique(c(df$from, df$to))
        }
        selected_id <- normalize_node_selection(selected_node())
        selected_id_safe <- if (is.null(selected_id)) {
          "__none__"
        } else {
          selected_id
        }
        connected_nodes <- if (!is.null(selected_id) && nrow(df) > 0) {
          unique(c(
            selected_id,
            df$to[df$from == selected_id],
            df$from[df$to == selected_id]
          ))
        } else {
          active_nodes
        }

        plot_nodes <- nodes %>%
          mutate(
            is_sdg = node_group == "SDG",
            is_amr = node_group == "AMR",
            sdg_logo = unname(sdg_logo_map[id]),
            has_sdg_logo = is_sdg & !is.na(sdg_logo) & nzchar(sdg_logo),
            amr_logo = unname(amr_logo_map[id]),
            has_amr_logo = is_amr & !is.na(amr_logo) & nzchar(amr_logo),
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
            is_connected = id %in% connected_nodes,
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
              has_sdg_logo | has_amr_logo ~ "rgba(0,0,0,0)",
              active ~ "#FFFFFF",
              TRUE ~ "#6B7280"
            ),
            opacity = case_when(
              is.null(selected_id) & active ~ 1,
              is.null(selected_id) ~ 0.28,
              is_selected ~ 1,
              is_connected ~ 1,
              active ~ 0.18,
              TRUE ~ 0.08
            ),
            shape = case_when(
              has_amr_logo ~ "circularImage",
              has_sdg_logo ~ "image",
              TRUE ~ "square"
            ),
            image = case_when(
              has_amr_logo ~ amr_logo,
              has_sdg_logo ~ sdg_logo,
              TRUE ~ NA_character_
            ),
            size = case_when(
              is_selected ~ 45,
              active ~ 38,
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
            opacity,
            color.background,
            color.border,
            font.color
          )

        if (nrow(df) > 0) {
          collapse_unique <- function(x) {
            x <- x[!is.na(x) & nzchar(x)]
            x <- unique(x)
            if (length(x) == 0) "Not specified" else paste(x, collapse = ", ")
          }

          plot_edges <- df %>%
            group_by(from, to, source) %>%
            summarise(
              from_short = dplyr::first(from_short),
              to_short = dplyr::first(to_short),
              countries = collapse_unique(country),
              themes = collapse_unique(theme),
              interaction_types = collapse_unique(interaction_type),
              network_effect = classify_network_effect(effect),
              effect_breakdown = paste(
                names(sort(table(effect), decreasing = TRUE)),
                " (",
                as.integer(sort(table(effect), decreasing = TRUE)),
                ")",
                sep = "",
                collapse = ", "
              ),
              relationship_count = dplyr::n(),
              .groups = "drop"
            ) %>%
            mutate(
              edge_color = edge_color_with_alpha(network_effect),
              width = 2.6,
              dashes = !is.na(source) & source == "Objective",
              edge_title = paste0(
                "<b>",
                from_short,
                " -> ",
                to_short,
                "</b><br>",
                "Countries: ",
                countries,
                "<br>",
                "Themes: ",
                themes,
                "<br>",
                "Type: ",
                interaction_types,
                "<br>",
                "Policy layer: ",
                ifelse(is.na(source) | !nzchar(source), "Not specified", source),
                "<br>",
                "Edge colour class: ",
                network_effect,
                "<br>",
                "Effects in current view: ",
                effect_breakdown,
                "<br>",
                "Relationships in current view: ",
                relationship_count
              )
            ) %>%
            transmute(
              id = paste(from, to, source, sep = "::"),
              from,
              to,
              title = edge_title,
              dashes,
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
            color = character(),
            width = numeric()
          )
        }

        displayed_edges <- if (show_edges()) plot_edges else plot_edges[0, ]

        visNetwork(
          plot_nodes,
          displayed_edges,
          width = "100%",
          height = "750px"
        ) %>%
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
            highlightNearest = list(
              enabled = TRUE,
              degree = 1,
              hover = FALSE
            )
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
    node_group <- node_info$node_group[[1]]
    node_short_label <- node_info$short_label[[1]]
    node_color <- if (identical(node_group, "SDG")) {
      unname(sdg_palette[node_short_label])
    } else {
      amr_color
    }
    if (is.na(node_color) || !nzchar(node_color)) {
      node_color <- "#334155"
    }
    node_icon <- if (identical(node_group, "SDG")) {
      unname(sdg_logo_map[node_id])
    } else {
      unname(amr_logo_map[node_id])
    }
    has_node_icon <- !is.na(node_icon) && nzchar(node_icon)
    rgb_vals <- grDevices::col2rgb(node_color)
    luminance <- (0.299 * rgb_vals[1, 1]) +
      (0.587 * rgb_vals[2, 1]) +
      (0.114 * rgb_vals[3, 1])
    header_text_color <- if (luminance > 160) "#0f172a" else "#ffffff"
    chip_bg <- if (luminance > 160) {
      "rgba(255, 255, 255, 0.34)"
    } else {
      "rgba(255, 255, 255, 0.22)"
    }

    related <- filtered_interactions() %>%
      filter(from == node_id | to == node_id) %>%
      arrange(desc(evidence_level))

    top_context <- related %>%
      count(country, theme, sort = TRUE) %>%
      slice_head(n = 4)

    tensions <- unique(related$policy_tension)
    tensions <- tensions[!is.na(tensions) & nzchar(tensions)]
    tensions <- head(tensions, 4)

    related_preview <- related %>%
      transmute(
        summary_key = paste(edge_id, source, sep = "::"),
        relation = paste0(from_short, " -> ", to_short),
        context = paste0(country, " | ", theme),
        source = ifelse(
          is.na(source) | !nzchar(source),
          "Not specified",
          source
        ),
        interaction_summary = ifelse(
          is.na(interaction_summary) | !nzchar(interaction_summary),
          "No interaction summary is available for this record.",
          interaction_summary
        )
      ) %>%
      slice_head(n = 6)

    div(
      class = "node-drawer open",
      div(
        class = "drawer-header drawer-header-brand",
        style = paste0(
          "background:",
          node_color,
          ";color:",
          header_text_color,
          ";"
        ),
        div(
          class = "drawer-title-wrap",
          div(
            class = "drawer-title-copy",
            div(
              class = "drawer-title-meta",
              if (has_node_icon) {
                img(
                  class = "drawer-node-icon",
                  src = node_icon,
                  alt = paste(node_short_label, "icon")
                )
              },
              span(
                class = "drawer-chip drawer-chip-brand",
                style = paste0(
                  "background:",
                  chip_bg,
                  ";color:",
                  header_text_color,
                  ";"
                ),
                node_short_label
              )
            ),
            h4(
              style = paste0("color:", header_text_color, ";"),
              node_info$label
            )
          )
        ),
        actionLink(
          "clear_selected_node",
          "Close",
          class = "drawer-close-link",
          style = paste0("color:", header_text_color, " !important;")
        )
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
        h5("Related interactions"),
        if (nrow(related_preview) == 0) {
          p("No related interactions under current filters.")
        } else {
          div(
            class = "interaction-list",
            lapply(seq_len(nrow(related_preview)), function(i) {
              layer_raw <- related_preview$source[i]
              summary_key <- related_preview$summary_key[i]
              is_open <- identical(active_interaction_summary(), summary_key)
              layer_cls <- case_when(
                layer_raw == "Objective" ~ "interaction-layer-objective",
                layer_raw ==
                  "Implementation" ~ "interaction-layer-implementation",
                TRUE ~ "interaction-layer-unspecified"
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
                  tags$button(
                    type = "button",
                    class = paste(
                      "interaction-info-button",
                      layer_cls,
                      if (is_open) "interaction-info-button-open"
                    ),
                    title = paste(layer_raw, "summary"),
                    `aria-label` = paste(layer_raw, "summary"),
                    onclick = sprintf(
                      "Shiny.setInputValue('drawer_summary_info', '%s', {priority: 'event'});",
                      summary_key
                    ),
                    span(class = "interaction-info-icon", "i"),
                    span(class = "interaction-info-label", layer_raw)
                  )
                ),
                if (is_open) {
                  div(
                    class = "interaction-summary-popup",
                    div(
                      class = "interaction-summary-body",
                      related_preview$interaction_summary[i]
                    )
                  )
                }
              )
            })
          )
        }
      )
    )
  })

  output$interaction_table <- renderDT({
    text_value <- function(x, missing = "Not specified") {
      if (is.na(x)) {
        return(missing)
      }
      x <- stringr::str_squish(as.character(x))
      if (nzchar(x)) x else missing
    }

    html_value <- function(x) {
      htmltools::htmlEscape(text_value(x))
    }

    attr_value <- function(x) {
      htmltools::htmlEscape(text_value(x), attribute = TRUE)
    }

    block_html <- function(
      title,
      value,
      extra_class = "",
      role_class = ""
    ) {
      text <- text_value(value, missing = "")
      if (!nzchar(text)) {
        content_html <- "<div class='it-details-empty'>Not available</div>"
      } else {
        list_parts <- if (stringr::str_detect(text, "•")) {
          stringr::str_split(text, "\\s*•\\s*")[[1]]
        } else if (stringr::str_detect(text, "\\r?\\n")) {
          stringr::str_split(text, "\\s*\\r?\\n\\s*")[[1]]
        } else if (nchar(text) > 220 && stringr::str_detect(text, ";")) {
          stringr::str_split(text, "\\s*;\\s*")[[1]]
        } else {
          character()
        }

        list_parts <- stringr::str_squish(list_parts)
        list_parts <- list_parts[nzchar(list_parts)]

        if (length(list_parts) >= 2) {
          content_html <- sprintf(
            "<ul class='it-details-list'>%s</ul>",
            paste0(
              sprintf("<li>%s</li>", htmltools::htmlEscape(list_parts)),
              collapse = ""
            )
          )
        } else {
          paragraph_parts <- stringr::str_split(text, "\\s*\\r?\\n\\s*")[[1]]
          paragraph_parts <- stringr::str_squish(paragraph_parts)
          paragraph_parts <- paragraph_parts[nzchar(paragraph_parts)]
          content_html <- paste0(
            sprintf(
              "<p class='it-details-paragraph'>%s</p>",
              htmltools::htmlEscape(paragraph_parts)
            ),
            collapse = ""
          )
        }
      }

      block_classes <- stringr::str_squish(
        paste("it-details-block", extra_class, role_class)
      )

      sprintf(
        "<div class='%s'><div class='it-details-title'>%s</div><div class='it-details-text'>%s</div></div>",
        block_classes,
        htmltools::htmlEscape(title),
        content_html
      )
    }

    pair_html <- function(from_short, from_label, to_short, to_label) {
      sprintf(
        paste0(
          "<div class='it-pair'>",
          "<div class='it-pair-code'>%s <span class='it-arrow'>&rarr;</span> %s</div>",
          "<div class='it-pair-label'>%s <span class='it-pair-sep'>&middot;</span> %s</div>",
          "</div>"
        ),
        html_value(from_short),
        html_value(to_short),
        html_value(from_label),
        html_value(to_label)
      )
    }

    context_html <- function(country, theme, interaction_type) {
      sprintf(
        paste0(
          "<div class='it-context'>",
          "<div class='it-context-main'>%s</div>",
          "<div class='it-context-sub'>%s <span class='it-pair-sep'>&middot;</span> %s</div>",
          "</div>"
        ),
        html_value(country),
        html_value(theme),
        html_value(interaction_type)
      )
    }

    badge_html <- function(text, class_name) {
      sprintf(
        "<span class='it-badge %s'>%s</span>",
        class_name,
        html_value(text)
      )
    }

    preview_html <- function(text, class_name) {
      sprintf(
        "<div class='%s' title='%s'>%s</div>",
        class_name,
        attr_value(text),
        html_value(text)
      )
    }

    details_html <- function(
      summary,
      tension,
      obj1,
      obj2,
      act1,
      act2,
      reference,
      from_short,
      to_short,
      source
    ) {
      layer <- text_value(source)
      layer_key <- stringr::str_to_lower(layer)
      panel_class <- dplyr::case_when(
        identical(layer_key, "objective") ~ "it-details-panel-layer-objective",
        identical(layer_key, "implementation") ~
          "it-details-panel-layer-implementation",
        TRUE ~ ""
      )
      obj_class <- if (identical(layer_key, "objective")) "it-details-block-layer-objective" else ""
      act_class <- if (identical(layer_key, "implementation")) "it-details-block-layer-implementation" else ""

      sprintf(
        paste0(
          "<div class='it-details-panel %s'>",
          "<div class='it-details-hero'>",
          "%s",
          "</div>",
          "<div class='it-details-grid'>",
          "%s%s%s%s%s",
          "</div>",
          "</div>"
        ),
        panel_class,
        block_html(
          "Interaction summary",
          summary,
          role_class = "it-details-block-summary"
        ),
        block_html(
          paste0(text_value(from_short), " – objective"),
          obj1,
          obj_class,
          "it-details-block-objective"
        ),
        block_html(
          paste0(text_value(to_short), " – objective"),
          obj2,
          obj_class,
          "it-details-block-objective"
        ),
        block_html(
          paste0(text_value(from_short), " – specific actions"),
          act1,
          act_class,
          "it-details-block-actions"
        ),
        block_html(
          paste0(text_value(to_short), " – specific actions"),
          act2,
          act_class,
          "it-details-block-actions"
        ),
        block_html(
          "Reference",
          reference,
          role_class = "it-details-block-reference"
        )
      )
    }

    tbl <- table_interactions() %>%
      mutate(
        Layer = purrr::map2_chr(source, source, function(x, .y) {
          x <- text_value(x)
          dplyr::case_when(
            identical(x, "Objective") ~ "it-badge-layer-objective",
            identical(x, "Implementation") ~ "it-badge-layer-implementation",
            TRUE ~ "it-badge-layer-unspecified"
          )
        }),
        EffectClass = purrr::map_chr(effect, function(x) {
          x <- text_value(x)
          dplyr::case_when(
            identical(x, "Conflict") ~ "it-badge-effect-conflict",
            identical(x, "Synergy") ~ "it-badge-effect-synergy",
            identical(x, "Independent") ~ "it-badge-effect-independent",
            TRUE ~ "it-badge-effect-mixed"
          )
        })
      ) %>%
      transmute(
        Pair = purrr::pmap_chr(
          list(from_short, from_label, to_short, to_label),
          pair_html
        ),
        Context = purrr::pmap_chr(
          list(country, theme, interaction_type),
          context_html
        ),
        Layer = purrr::map2_chr(source, Layer, badge_html),
        Effect = purrr::map2_chr(effect, EffectClass, badge_html),
        Tension = purrr::map_chr(
          policy_tension,
          ~ preview_html(.x, "it-preview it-preview-tight")
        ),
        Summary = purrr::map_chr(
          interaction_summary,
          ~ preview_html(.x, "it-preview it-preview-wide")
        ),
        Details = "<button type='button' class='it-details-btn' aria-expanded='false'><span class='it-details-btn-label'>Details</span></button>",
        DetailsHtml = purrr::pmap_chr(
          list(
            interaction_summary,
            policy_tension,
            policy_strategic_objective_1,
            policy_strategic_objective_2,
            policy_specific_actions_1,
            policy_specific_actions_2,
            reference,
            from_short,
            to_short,
            source
          ),
          details_html
        )
      )

    datatable(
      tbl,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      filter = "none",
      class = "compact stripe hover interaction-table",
      callback = DT::JS(
        "table.on('click', 'button.it-details-btn', function () {",
        "  var btn = $(this);",
        "  var tr = btn.closest('tr');",
        "  var row = table.row(tr);",
        "  var details = row.data()[7];",
        "  table.rows('.shown').every(function(){",
        "    if (this.index() !== row.index()) {",
        "      $(this.node()).removeClass('shown');",
        "      $(this.node()).find('button.it-details-btn').attr('aria-expanded', 'false').html(\"<span class='it-details-btn-label'>Details</span>\");",
        "      this.child.hide();",
        "    }",
        "  });",
        "  if (row.child.isShown()) {",
        "    row.child.hide();",
        "    tr.removeClass('shown');",
        "    btn.attr('aria-expanded', 'false').html(\"<span class='it-details-btn-label'>Details</span>\");",
        "  } else {",
        "    row.child(details).show();",
        "    tr.addClass('shown');",
        "    btn.attr('aria-expanded', 'true').html(\"<span class='it-details-btn-label'>Hide</span>\");",
        "  }",
        "});"
      ),
      options = list(
        pageLength = 8,
        lengthMenu = c(8, 20, 40),
        autoWidth = FALSE,
        scrollX = TRUE,
        order = list(list(0, "asc")),
        columnDefs = list(
          list(visible = FALSE, targets = c(4, 7)),
          list(orderable = FALSE, targets = c(6, 7)),
          list(width = "22%", targets = 0),
          list(width = "22%", targets = 1),
          list(width = "13%", targets = 2),
          list(width = "13%", targets = 3),
          list(width = "22%", targets = 5),
          list(width = "8%", targets = 6)
        ),
        dom = "<'it-toolbar'lf>tip"
      )
    )
  })

  output$country_summary_table <- renderDT({
    tbl <- displayed_interactions() %>%
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
