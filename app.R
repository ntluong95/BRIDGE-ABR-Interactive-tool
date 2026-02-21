required_packages <- c("shiny", "dplyr", "readr", "stringr", "DT", "visNetwork")
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
  select(id, label, short_label, node_group, node_type)

interactions_enriched <- interactions %>%
  left_join(node_lookup, by = c("from" = "id")) %>%
  rename(
    from_label = label,
    from_short = short_label,
    from_group = node_group,
    from_type = node_type
  ) %>%
  left_join(node_lookup, by = c("to" = "id")) %>%
  rename(
    to_label = label,
    to_short = short_label,
    to_group = node_group,
    to_type = node_type
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

node_group_palette <- c(
  "AMR" = "#0f766e",
  "SDG" = "#1d4ed8"
)
effect_palette <- c(
  "Tension (trade-off)" = "#b91c1c",
  "Synergy (co-benefit)" = "#15803d",
  "Mixed / context-dependent" = "#4b5563"
)

shape_by_group <- function(group_value) {
  ifelse(group_value == "AMR", "diamond", "box")
}

ui <- navbarPage(
  title = div(
    class = "app-brand",
    tags$img(
      src = "logo-bridge-abr.svg",
      alt = "BRIDGE-ABR logo",
      class = "app-brand-logo"
    ),
    div(
      class = "app-brand-text",
      tags$span(class = "app-brand-kicker", "BRIDGE-ABR"),
      tags$span(class = "app-brand-title", "Policy Tension Explorer")
    )
  ),
  windowTitle = "BRIDGE-ABR Policy Tension Explorer",
  id = "main_nav",
  collapsible = TRUE,

  header = tags$head(
    tags$style(HTML(
      "
      body {
        background: #edf2f7;
        color: #1f2933;
        font-family: 'Merriweather', Georgia, serif;
      }
      .navbar.navbar-default {
        background: linear-gradient(120deg, #0b3c5d 0%, #155e75 52%, #1f7a8c 100%);
        border: none;
        box-shadow: 0 6px 14px rgba(9, 30, 66, 0.24);
      }
      .navbar.navbar-default .container-fluid {
        padding-left: 14px;
        padding-right: 14px;
      }
      .navbar-default .navbar-brand {
        color: #ffffff !important;
        height: 78px;
        display: flex;
        align-items: center;
        padding: 10px 6px;
      }
      .app-brand {
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .app-brand-logo {
        width: 52px;
        height: 52px;
        border-radius: 12px;
        background: #ffffff;
        padding: 5px;
        box-shadow: 0 2px 8px rgba(15, 23, 42, 0.25);
      }
      .app-brand-text {
        display: flex;
        flex-direction: column;
        line-height: 1.08;
      }
      .app-brand-kicker {
        color: #cbe8ff;
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 1.2px;
        text-transform: uppercase;
      }
      .app-brand-title {
        color: #ffffff;
        font-size: 20px;
        font-weight: 700;
      }
      .navbar-default .navbar-nav > li > a {
        color: #d9e7f3 !important;
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 15px;
        font-weight: 600;
        padding-top: 29px;
        padding-bottom: 29px;
        transition: background-color 0.15s ease, color 0.15s ease;
      }
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        color: #ffffff !important;
        background-color: rgba(255, 255, 255, 0.12) !important;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        color: #ffffff !important;
        background-color: rgba(1, 22, 39, 0.4) !important;
      }
      .navbar-default .navbar-toggle {
        border-color: rgba(255, 255, 255, 0.6);
        margin-top: 21px;
      }
      .navbar-default .navbar-toggle .icon-bar {
        background-color: #ffffff;
      }
      .page-wrap {
        max-width: 1700px;
        margin: 0 auto;
        padding: 16px 18px 26px 18px;
      }
      .card {
        background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
        border: 1px solid #d7e2ec;
        border-radius: 12px;
        padding: 14px;
        box-shadow: 0 3px 10px rgba(15, 23, 42, 0.06);
      }
      .lead-note {
        font-size: 14px;
        line-height: 1.48;
        margin-bottom: 14px;
      }
      .section-title {
        font-size: 15px;
        font-weight: 700;
        margin-bottom: 8px;
      }
      .control-buttons {
        margin-top: 8px;
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
      }
      .kpi-row {
        margin-top: 10px;
        font-size: 14px;
        font-weight: 700;
        color: #2d3748;
      }
      .legend-row {
        margin-top: 8px;
        display: flex;
        flex-wrap: wrap;
        gap: 14px;
        font-size: 12px;
      }
      .legend-chip {
        display: inline-flex;
        align-items: center;
        gap: 7px;
      }
      .legend-line {
        width: 38px;
        border-top: 2px solid #4b5563;
      }
      .legend-line.indirect {
        border-top-style: dashed;
      }
      .legend-dot {
        display: inline-block;
        width: 11px;
        height: 11px;
        border-radius: 50%;
      }
      .table-block {
        margin-top: 14px;
      }
      .dataTables_wrapper {
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 13px;
      }
      .btn {
        border-radius: 8px;
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-weight: 600;
      }
      .btn-primary {
        background-color: #0f5f8d;
        border-color: #0f5f8d;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        background-color: #0b4f74;
        border-color: #0b4f74;
      }
      .form-control,
      .selectize-input {
        border-radius: 8px;
        border-color: #cbd5e1;
      }
      .selectize-input.focus {
        border-color: #0f5f8d;
        box-shadow: 0 0 0 0.2rem rgba(15, 95, 141, 0.18);
      }
      @media (max-width: 992px) {
        .navbar-default .navbar-brand {
          height: auto;
          padding: 9px 8px;
        }
        .app-brand-logo {
          width: 42px;
          height: 42px;
        }
        .app-brand-title {
          font-size: 16px;
        }
        .navbar-default .navbar-nav > li > a {
          padding-top: 10px;
          padding-bottom: 10px;
        }
      }
      "
    ))
  ),

  tabPanel(
    title = "Policy Tension Explorer",
    div(
      class = "page-wrap",
      h2("National Action Plan policy tensions and synergies"),
      div(
        class = "lead-note",
        p(
          "This version visualizes only policy-objective interactions:",
          strong(" AMR-AMR, SDG-SDG, and AMR-SDG "),
          "at national action plan level."
        ),
        p(
          "AMR nodes are the", strong("5 WHO Global Action Plan objectives"),
          "and SDG nodes are the", strong("17 UN SDGs"),
          ". Example included in sample data: AMR-03 vs SDG-08 in Country XXX under Agriculture and Food Systems."
        )
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "card",
            div(class = "section-title", "Interaction families"),
            checkboxGroupInput(
              "family_filter",
              label = NULL,
              choices = family_choices,
              selected = family_choices
            ),
            div(class = "section-title", "Country or NAP context"),
            selectizeInput(
              "country_filter",
              label = NULL,
              choices = country_choices,
              selected = country_choices,
              multiple = TRUE,
              options = list(placeholder = "Select one or multiple countries")
            ),
            div(class = "section-title", "Themes"),
            checkboxGroupInput(
              "theme_filter",
              label = NULL,
              choices = theme_choices,
              selected = theme_choices
            ),
            div(class = "section-title", "Interaction effect"),
            checkboxGroupInput(
              "effect_filter",
              label = NULL,
              choices = effect_choices,
              selected = effect_choices
            ),
            div(class = "section-title", "Evidence level"),
            checkboxGroupInput(
              "evidence_filter",
              label = NULL,
              choices = evidence_choices,
              selected = evidence_choices,
              inline = TRUE
            ),
            div(class = "section-title", "Focus objectives (optional)"),
            selectizeInput(
              "focus_nodes",
              label = NULL,
              choices = objective_choices,
              selected = character(0),
              multiple = TRUE,
              options = list(placeholder = "Type AMR-03 or SDG-08")
            ),
            textInput(
              "search_text",
              "Search policy tension or reference",
              value = ""
            ),
            checkboxInput("context_only", "Only context-dependent interactions", FALSE),
            checkboxInput("bidirectional_only", "Only bidirectional interactions", FALSE),
            div(
              class = "control-buttons",
              actionButton("reset_filters", "Reset filters"),
              downloadButton("download_filtered", "Download filtered CSV")
            )
          )
        ),

        column(
          width = 8,
          div(
            class = "card",
            visNetworkOutput("interaction_network", height = "680px"),
            div(
              class = "legend-row",
              div(class = "legend-chip", span(class = "legend-line"), span("Direct")),
              div(class = "legend-chip", span(class = "legend-line indirect"), span("Indirect")),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#b91c1c;"),
                span("Tension (trade-off)")
              ),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#15803d;"),
                span("Synergy (co-benefit)")
              ),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#4b5563;"),
                span("Mixed / context-dependent")
              ),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#0f766e;"),
                span("AMR objective")
              ),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#1d4ed8;"),
                span("SDG goal")
              )
            ),
            div(class = "kpi-row", textOutput("kpi_text"))
          )
        )
      ),

      div(
        class = "table-block card",
        h4("Filtered policy interactions"),
        DTOutput("interaction_table")
      ),

      div(
        class = "table-block card",
        h4("Country summary in current filtered view"),
        DTOutput("country_summary_table")
      )
    )
  ),

  tabPanel(
    title = "Objective Dictionary",
    div(
      class = "page-wrap",
      h2("WHO GAP and SDG objective dictionary"),
      div(
        class = "lead-note card",
        p(
          "Nodes are fixed to WHO GAP AMR objectives (AMR-01 to AMR-05) and UN SDGs (SDG-01 to SDG-17)."
        ),
        p(
          "Interactions are expected to be coded from national action plans or policy documents with country and theme metadata."
        )
      ),
      div(
        class = "table-block card",
        h4("Objective list"),
        DTOutput("node_table")
      ),
      div(
        class = "table-block card",
        h4("Interaction counts by family, country, and effect"),
        DTOutput("summary_table")
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_interactions <- reactive({
    df <- interactions_enriched

    if (length(input$family_filter) > 0) {
      df <- df %>% filter(interaction_family %in% input$family_filter)
    } else {
      df <- df[0, ]
    }

    if (length(input$country_filter) > 0) {
      df <- df %>% filter(country %in% input$country_filter)
    } else {
      df <- df[0, ]
    }

    if (length(input$theme_filter) > 0) {
      df <- df %>% filter(theme %in% input$theme_filter)
    } else {
      df <- df[0, ]
    }

    if (length(input$effect_filter) > 0) {
      df <- df %>% filter(effect %in% input$effect_filter)
    } else {
      df <- df[0, ]
    }

    if (length(input$evidence_filter) > 0) {
      df <- df %>% filter(evidence_level %in% input$evidence_filter)
    } else {
      df <- df[0, ]
    }

    if (isTRUE(input$context_only)) {
      df <- df %>% filter(context_dependent)
    }

    if (isTRUE(input$bidirectional_only)) {
      df <- df %>% filter(bidirectional)
    }

    if (length(input$focus_nodes) > 0) {
      df <- df %>% filter(from %in% input$focus_nodes | to %in% input$focus_nodes)
    }

    search_text <- input$search_text
    if (is.null(search_text)) {
      search_text <- ""
    }

    search_term <- str_to_lower(str_trim(search_text))
    if (nchar(search_term) > 0) {
      df <- df %>% filter(str_detect(search_blob, fixed(search_term)))
    }

    df
  })

  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "family_filter", selected = family_choices)
    updateSelectizeInput(session, "country_filter", selected = country_choices, server = TRUE)
    updateCheckboxGroupInput(session, "theme_filter", selected = theme_choices)
    updateCheckboxGroupInput(session, "effect_filter", selected = effect_choices)
    updateCheckboxGroupInput(session, "evidence_filter", selected = evidence_choices)
    updateSelectizeInput(session, "focus_nodes", selected = character(0), server = TRUE)
    updateTextInput(session, "search_text", value = "")
    updateCheckboxInput(session, "context_only", value = FALSE)
    updateCheckboxInput(session, "bidirectional_only", value = FALSE)
  })

  output$kpi_text <- renderText({
    df <- filtered_interactions()
    visible_nodes <- unique(c(df$from, df$to))
    paste0(
      "Visible interactions: ", nrow(df),
      " | Objectives in view: ", length(visible_nodes),
      " | Countries: ", dplyr::n_distinct(df$country),
      " | Themes: ", dplyr::n_distinct(df$theme)
    )
  })

  output$interaction_network <- renderVisNetwork({
    df <- filtered_interactions()
    active_nodes <- unique(c(df$from, df$to, input$focus_nodes))

    plot_nodes <- nodes %>%
      mutate(
        active = id %in% active_nodes,
        label_plot = short_label,
        title = paste0(
          "<b>", short_label, "</b><br>",
          label, "<br>",
          "Type: ", node_type, "<br>",
          "Framework: ", source_framework
        ),
        shape = shape_by_group(node_group),
        color.background = ifelse(active, node_group_palette[node_group], "#d4d8df"),
        color.border = ifelse(active, "#1f2937", "#9ca3af"),
        font.color = ifelse(active, "#0f172a", "#6b7280"),
        size = ifelse(active, 30, 20)
      )

    plot_nodes$color <- I(Map(
      function(background, border) list(background = background, border = border),
      plot_nodes$color.background,
      plot_nodes$color.border
    ))
    plot_nodes$font <- I(lapply(
      plot_nodes$font.color,
      function(x) list(color = x, face = "Merriweather", size = 14)
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
          edge_color = effect_palette[effect],
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
          dashes = direct_or_indirect == "Indirect",
          arrows = ifelse(bidirectional, "to;from", "to"),
          color = edge_color,
          width = ifelse(effect == "Mixed / context-dependent", 1.6, 2.3)
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

    visNetwork(plot_nodes, plot_edges, width = "100%", height = "680px") %>%
      visNodes(borderWidth = 1.2, shadow = FALSE) %>%
      visEdges(smooth = FALSE) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) %>%
      visInteraction(hover = TRUE, navigationButtons = TRUE) %>%
      visPhysics(enabled = FALSE)
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
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), scrollX = TRUE)
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
        `Count` = n
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(10, 20, 50), scrollX = TRUE)
    )
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("bridge_abr_policy_interactions_", Sys.Date(), ".csv")
    },
    content = function(file) {
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
  )

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
}

shinyApp(ui = ui, server = server)
