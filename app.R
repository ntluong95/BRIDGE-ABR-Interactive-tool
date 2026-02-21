required_packages <- c(
  "shiny", "dplyr", "readr", "stringr", "DT", "visNetwork",
  "bslib", "bsicons", "shinyWidgets", "shinycssloaders"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install_cmd <- paste0(
    "install.packages(c(\"",
    paste(missing_packages, collapse = "\", \""),
    "\"))"
  )
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install them with ",
      install_cmd
    )
  )
}

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(stringr)
  library(DT)
  library(visNetwork)
  library(bslib)
  library(bsicons)
  library(shinyWidgets)
  library(shinycssloaders)
})

data_dir <- "data"
nodes_path <- file.path(data_dir, "policy_nodes.csv")
interactions_path <- file.path(data_dir, "policy_interactions.csv")

if (!file.exists(nodes_path) || !file.exists(interactions_path)) {
  stop("Missing data files. Expected data/policy_nodes.csv and data/policy_interactions.csv")
}

nodes <- read_csv(nodes_path, show_col_types = FALSE)
interactions <- read_csv(interactions_path, show_col_types = FALSE)

required_node_cols <- c(
  "id", "label", "short_label", "node_group", "node_type", "source_framework", "description"
)
required_interaction_cols <- c(
  "edge_id", "from", "to", "interaction_family", "country", "theme", "effect",
  "direct_or_indirect", "bidirectional", "context_dependent", "interaction_summary",
  "policy_tension", "evidence_level", "reference"
)

if (!all(required_node_cols %in% names(nodes))) {
  stop("data/policy_nodes.csv is missing required columns")
}
if (!all(required_interaction_cols %in% names(interactions))) {
  stop("data/policy_interactions.csv is missing required columns")
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
    bidirectional = as.logical(bidirectional),
    context_dependent = as.logical(context_dependent)
  )

if (anyDuplicated(nodes$id) > 0) {
  stop("Duplicate node ids found in data/policy_nodes.csv")
}
if (anyDuplicated(interactions$edge_id) > 0) {
  stop("Duplicate edge ids found in data/policy_interactions.csv")
}
if (any(!interactions$from %in% nodes$id) || any(!interactions$to %in% nodes$id)) {
  stop("Interaction table contains node ids not found in policy_nodes.csv")
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
        interaction_family,
        effect,
        from_short,
        to_short,
        interaction_summary,
        policy_tension,
        evidence_level,
        reference
      )
    )
  )

family_choices <- c("AMR-AMR", "SDG-SDG", "AMR-SDG")
family_choices <- intersect(family_choices, sort(unique(interactions_enriched$interaction_family)))
country_choices <- sort(unique(interactions_enriched$country))
theme_choices <- sort(unique(interactions_enriched$theme))
effect_choices <- c("Tension (trade-off)", "Synergy (co-benefit)", "Mixed / context-dependent")
effect_choices <- intersect(effect_choices, sort(unique(interactions_enriched$effect)))
evidence_choices <- c("High", "Medium", "Emerging")
evidence_choices <- intersect(evidence_choices, sort(unique(interactions_enriched$evidence_level)))
objective_choices <- setNames(nodes$id, paste0(nodes$short_label, " - ", nodes$label))

sdg_palette <- c(
  "sdg01" = "#E5243B",
  "sdg02" = "#DDA63A",
  "sdg03" = "#4C9F38",
  "sdg04" = "#C5192D",
  "sdg05" = "#FF3A21",
  "sdg06" = "#26BDE2",
  "sdg07" = "#FCC30B",
  "sdg08" = "#A21942",
  "sdg09" = "#FD6925",
  "sdg10" = "#DD1367",
  "sdg11" = "#FD9D24",
  "sdg12" = "#BF8B2E",
  "sdg13" = "#3F7E44",
  "sdg14" = "#0A97D9",
  "sdg15" = "#56C02B",
  "sdg16" = "#00689D",
  "sdg17" = "#19486A"
)

amr_color <- "#0F766E"
effect_palette <- c(
  "Tension (trade-off)" = "#C62828",
  "Synergy (co-benefit)" = "#2E7D32",
  "Mixed / context-dependent" = "#F9A825"
)

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

ui <- page_navbar(
  title = div(
    class = "app-brand",
    tags$img(
      src = "logo-bridge-abr.svg",
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
    fg = "#0F172A",
    primary = "#155E75",
    secondary = "#0F766E",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
            open = c("Families", "Country", "Theme", "Effect", "Evidence"),

            accordion_panel(
              title = filter_title("diagram-3", "Interaction families"),
              value = "Families",
              pickerInput(
                "family_filter",
                label = NULL,
                choices = family_choices,
                selected = family_choices,
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
                label = "Focus objectives",
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
            actionButton("apply_filters", "Apply filters", class = "btn btn-primary btn-apply"),
            downloadButton("download_filtered_sidebar", "Download filtered", class = "btn btn-outline-primary btn-download")
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
              class = "network-stage",
              shinycssloaders::withSpinner(
                visNetworkOutput("interaction_network", height = "620px"),
                type = 4,
                color = "#155E75"
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
              color = "#155E75"
            )
          ),

          div(
            class = "data-card",
            h4("Country summary in current filtered view"),
            shinycssloaders::withSpinner(
              DTOutput("country_summary_table"),
              type = 4,
              color = "#155E75"
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "Objective Dictionary",
    div(
      class = "page-wrap",
      div(
        class = "data-card",
        h3("WHO GAP and SDG objective dictionary"),
        p("This dictionary is fixed to 5 WHO GAP AMR objectives and 17 UN SDGs."),
        shinycssloaders::withSpinner(
          DTOutput("node_table"),
          type = 4,
          color = "#155E75"
        )
      ),
      div(
        class = "data-card",
        h4("Interaction counts by family, country, and effect"),
        shinycssloaders::withSpinner(
          DTOutput("summary_table"),
          type = 4,
          color = "#155E75"
        )
      )
    )
  ),

  nav_panel(
    "About",
    div(
      class = "page-wrap",
      div(
        class = "data-card",
        h3("About this tool"),
        p(
          "Policy Tension Explorer supports analysis of policy-level interactions",
          "between AMR objectives and SDGs at national action plan level."
        ),
        tags$ul(
          tags$li("Interaction families in scope: AMR-AMR, SDG-SDG, AMR-SDG"),
          tags$li("AMR nodes: WHO GAP objectives AMR-01 to AMR-05"),
          tags$li("SDG nodes: UN SDG goals SDG-01 to SDG-17"),
          tags$li("Each interaction record is tagged by country and policy theme")
        ),
        p(
          "Use this platform to surface synergies, trade-offs, and context-dependent",
          "policy dynamics for deliberation and evidence-informed policy design."
        )
      )
    )
  ),

  nav_panel(
    span(class = "download-tab-label", "Download Data"),
    div(
      class = "page-wrap",
      layout_columns(
        col_widths = c(6, 6),

        div(
          class = "data-card",
          h4("Current filtered dataset"),
          p("Download the interactions currently visible under applied filters."),
          downloadButton("download_filtered_tab", "Download filtered interactions", class = "btn btn-primary")
        ),

        div(
          class = "data-card",
          h4("Full interaction dataset"),
          p("Download all policy interaction records in the app."),
          downloadButton("download_all_interactions", "Download all interactions", class = "btn btn-primary")
        ),

        div(
          class = "data-card",
          h4("Objective dictionary"),
          p("Download the 5 AMR objectives and 17 SDG goals dictionary."),
          downloadButton("download_all_nodes", "Download objective dictionary", class = "btn btn-outline-primary")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  format_count <- function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
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
    paste0(paste(head(values, limit), collapse = ", "), " +", length(values) - limit)
  }

  default_filters <- list(
    family = family_choices,
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
    family = default_filters$family,
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

  observeEvent(input$network_node_selected, {
    selected_node(input$network_node_selected)
  })

  observeEvent(input$clear_selected_node, {
    selected_node(NULL)
  })

  observeEvent(input$apply_filters, {
    applied_filters$family <- input$family_filter
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
    updatePickerInput(session, "family_filter", selected = default_filters$family)
    updatePickerInput(session, "country_filter", selected = default_filters$country)
    updatePickerInput(session, "theme_filter", selected = default_filters$theme)
    updatePickerInput(session, "effect_filter", selected = default_filters$effect)
    updatePickerInput(session, "evidence_filter", selected = default_filters$evidence)
    updatePickerInput(session, "focus_nodes", selected = default_filters$focus_nodes)
    updatePrettySwitch(session, "context_only", value = default_filters$context_only)
    updatePrettySwitch(session, "bidirectional_only", value = default_filters$bidirectional_only)
    updateTextInput(session, "search_text", value = default_filters$search_text)

    applied_filters$family <- default_filters$family
    applied_filters$country <- default_filters$country
    applied_filters$theme <- default_filters$theme
    applied_filters$effect <- default_filters$effect
    applied_filters$evidence <- default_filters$evidence
    applied_filters$focus_nodes <- default_filters$focus_nodes
    applied_filters$context_only <- default_filters$context_only
    applied_filters$bidirectional_only <- default_filters$bidirectional_only
    applied_filters$search_text <- default_filters$search_text
    selected_node(NULL)
  })

  filtered_interactions <- reactive({
    df <- interactions_enriched

    if (length(applied_filters$family) > 0) {
      df <- df %>% filter(interaction_family %in% applied_filters$family)
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

    if (isTRUE(applied_filters$context_only)) {
      df <- df %>% filter(context_dependent)
    }

    if (isTRUE(applied_filters$bidirectional_only)) {
      df <- df %>% filter(bidirectional)
    }

    if (length(applied_filters$focus_nodes) > 0) {
      df <- df %>% filter(from %in% applied_filters$focus_nodes | to %in% applied_filters$focus_nodes)
    }

    search_term <- str_to_lower(str_trim(ifelse(is.null(applied_filters$search_text), "", applied_filters$search_text)))
    if (nchar(search_term) > 0) {
      df <- df %>% filter(str_detect(search_blob, fixed(search_term)))
    }

    df
  })

  output$dynamic_title <- renderUI({
    country_label <- compact_selection(applied_filters$country, country_choices)
    family_label <- compact_selection(applied_filters$family, family_choices)

    div(
      class = "title-block",
      h2("Policy interactions in national action plans"),
      p(
        tags$strong("Countries:"),
        paste(country_label),
        tags$span(" | "),
        tags$strong("Families:"),
        paste(family_label)
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
    format_count(sum(filtered_interactions()$effect == "Mixed / context-dependent"))
  })

  output$metric_total <- renderText({
    format_count(nrow(filtered_interactions()))
  })

  edge_color_with_alpha <- function(effect_vec, mode_vec) {
    base_cols <- unname(effect_palette[effect_vec])
    alphas <- ifelse(mode_vec == "Indirect", 0.4, 0.9)
    mapply(
      function(col, alpha_val) grDevices::adjustcolor(col, alpha.f = alpha_val),
      base_cols,
      alphas,
      USE.NAMES = FALSE
    )
  }

  output$interaction_network <- renderVisNetwork({
    df <- filtered_interactions()
    active_nodes <- unique(c(df$from, df$to, applied_filters$focus_nodes))

    plot_nodes <- nodes %>%
      mutate(
        is_sdg = node_group == "SDG",
        sdg_number = str_extract(short_label, "[0-9]{2}$"),
        label_plot = ifelse(is_sdg, sdg_number, short_label),
        title = paste0(
          "<b>", short_label, "</b><br>",
          label, "<br>",
          "Group: ", node_group, "<br>",
          "Framework: ", source_framework, "<br>",
          description
        ),
        active = id %in% active_nodes,
        base_bg = ifelse(is_sdg, sdg_palette[id], amr_color),
        color.background = ifelse(active, base_bg, "#DDE4ED"),
        color.border = ifelse(active, "#0F172A", "#A8B4C3"),
        font.color = ifelse(active, "#FFFFFF", "#6B7280"),
        shape = ifelse(node_group == "AMR", "diamond", "box"),
        size = case_when(
          node_group == "AMR" & active ~ 40,
          node_group == "AMR" ~ 34,
          active ~ 30,
          TRUE ~ 24
        )
      )

    plot_nodes$color <- I(Map(
      function(background, border) list(background = background, border = border),
      plot_nodes$color.background,
      plot_nodes$color.border
    ))
    plot_nodes$font <- I(lapply(
      plot_nodes$font.color,
      function(col) list(color = col, face = "Inter", size = 14, vadjust = 0)
    ))

    plot_nodes <- plot_nodes %>%
      transmute(
        id,
        label = label_plot,
        title,
        group = node_group,
        shape,
        color,
        font,
        size
      )

    if (nrow(df) > 0) {
      plot_edges <- df %>%
        mutate(
          edge_color = edge_color_with_alpha(effect, direct_or_indirect),
          dashes = case_when(
            effect == "Mixed / context-dependent" ~ TRUE,
            direct_or_indirect == "Indirect" ~ TRUE,
            TRUE ~ FALSE
          ),
          base_width = case_when(
            effect == "Tension (trade-off)" ~ 3.2,
            effect == "Synergy (co-benefit)" ~ 2.6,
            TRUE ~ 2.0
          ),
          width = ifelse(direct_or_indirect == "Indirect", pmax(1.2, base_width - 1.0), base_width),
          edge_title = paste0(
            "<b>", from_short, " -> ", to_short, "</b><br>",
            "Country: ", country, "<br>",
            "Theme: ", theme, "<br>",
            "Family: ", interaction_family, "<br>",
            "Effect: ", effect, "<br>",
            "Policy tension: ", policy_tension, "<br>",
            "Summary: ", interaction_summary, "<br>",
            "Evidence: ", evidence_level, "<br>",
            "Reference: ", reference
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
      visNodes(borderWidth = 1.2, shadow = list(enabled = TRUE, size = 8, x = 1, y = 2)) %>%
      visEdges(smooth = list(enabled = TRUE, type = "dynamic", roundness = 0.25)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) %>%
      visInteraction(hover = TRUE, navigationButtons = TRUE, tooltipDelay = 100) %>%
      visEvents(
        selectNode = "function(params){ if(params.nodes.length){ Shiny.setInputValue('network_node_selected', params.nodes[0], {priority: 'event'}); } }",
        deselectNode = "function(params){ Shiny.setInputValue('network_node_selected', null, {priority: 'event'}); }"
      ) %>%
      visPhysics(enabled = FALSE)
  })

  output$node_drawer <- renderUI({
    node_id <- selected_node()

    if (is.null(node_id) || !node_id %in% nodes$id) {
      return(
        div(
          class = "node-drawer",
          div(class = "drawer-placeholder", "Click any AMR or SDG node to inspect objective details and related interactions.")
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

    div(
      class = "node-drawer open",
      div(
        class = "drawer-header",
        div(
          class = "drawer-title-wrap",
          span(class = "drawer-chip", node_info$short_label),
          h4(node_info$label)
        ),
        actionLink("clear_selected_node", "Close", class = "drawer-close-link")
      ),
      p(class = "drawer-description", node_info$description),
      div(
        class = "drawer-stats",
        div(class = "drawer-stat", span("Interactions"), strong(nrow(related))),
        div(class = "drawer-stat", span("Countries"), strong(dplyr::n_distinct(related$country))),
        div(class = "drawer-stat", span("Themes"), strong(dplyr::n_distinct(related$theme)))
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
              tags$li(paste0(top_context$country[i], " - ", top_context$theme[i], " (", top_context$n[i], ")"))
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
      )
    )
  })

  output$interaction_table <- renderDT({
    tbl <- filtered_interactions() %>%
      transmute(
        `Edge ID` = edge_id,
        Country = country,
        Theme = theme,
        `Interaction Family` = interaction_family,
        `From Objective` = paste0(from_short, " - ", from_label),
        `To Objective` = paste0(to_short, " - ", to_label),
        Effect = effect,
        `Direct or Indirect` = direct_or_indirect,
        Bidirectional = ifelse(bidirectional, "Yes", "No"),
        `Context-Dependent` = ifelse(context_dependent, "Yes", "No"),
        `Policy Tension` = policy_tension,
        `Interaction Summary` = interaction_summary,
        `Evidence Level` = evidence_level,
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
      count(country, interaction_family, effect, theme, sort = TRUE) %>%
      rename(
        Country = country,
        `Interaction Family` = interaction_family,
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
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), scrollX = TRUE)
    )
  })

  output$summary_table <- renderDT({
    tbl <- interactions_enriched %>%
      count(country, interaction_family, effect, sort = TRUE) %>%
      rename(
        Country = country,
        `Interaction Family` = interaction_family,
        Effect = effect,
        `Number of Interactions` = n
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), scrollX = TRUE)
    )
  })

  download_filtered_content <- function(file) {
    out <- filtered_interactions() %>%
      transmute(
        edge_id,
        country,
        theme,
        interaction_family,
        from,
        from_short,
        from_label,
        to,
        to_short,
        to_label,
        effect,
        direct_or_indirect,
        bidirectional,
        context_dependent,
        policy_tension,
        interaction_summary,
        evidence_level,
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

shinyApp(ui = ui, server = server)
