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
nodes_path <- file.path(data_dir, "amr_sdg_nodes.csv")
edges_path <- file.path(data_dir, "amr_sdg_edges.csv")

if (!file.exists(nodes_path) || !file.exists(edges_path)) {
  stop("Missing data files. Expected data/amr_sdg_nodes.csv and data/amr_sdg_edges.csv")
}

nodes <- read_csv(nodes_path, show_col_types = FALSE)
edges <- read_csv(edges_path, show_col_types = FALSE)

required_nodes_cols <- c(
  "id", "label", "short_label", "node_type", "domain", "theme", "description"
)

required_edges_cols <- c(
  "edge_id", "from", "to", "interaction_family", "theme", "effect",
  "direct_or_indirect", "bidirectional", "context_dependent", "mechanism",
  "policy_tension", "evidence_level", "reference"
)

if (!all(required_nodes_cols %in% names(nodes))) {
  stop("data/amr_sdg_nodes.csv is missing required columns")
}

if (!all(required_edges_cols %in% names(edges))) {
  stop("data/amr_sdg_edges.csv is missing required columns")
}

nodes <- nodes %>%
  mutate(
    across(where(is.character), str_squish),
    id = tolower(id)
  )

edges <- edges %>%
  mutate(
    across(where(is.character), str_squish),
    from = tolower(from),
    to = tolower(to),
    bidirectional = as.logical(bidirectional),
    context_dependent = as.logical(context_dependent)
  )

unknown_from <- setdiff(unique(edges$from), nodes$id)
unknown_to <- setdiff(unique(edges$to), nodes$id)

if (length(unknown_from) > 0 || length(unknown_to) > 0) {
  stop("Edge table contains node ids not found in nodes table")
}

if (anyDuplicated(nodes$id) > 0) {
  stop("Duplicate node ids found in data/amr_sdg_nodes.csv")
}

if (anyDuplicated(edges$edge_id) > 0) {
  stop("Duplicate edge ids found in data/amr_sdg_edges.csv")
}

node_lookup <- nodes %>%
  select(id, label, short_label, node_type, domain, node_theme = theme)

edges_enriched <- edges %>%
  left_join(node_lookup, by = c("from" = "id")) %>%
  rename(
    from_label = label,
    from_short = short_label,
    from_type = node_type,
    from_domain = domain,
    from_theme = node_theme
  ) %>%
  left_join(node_lookup, by = c("to" = "id")) %>%
  rename(
    to_label = label,
    to_short = short_label,
    to_type = node_type,
    to_domain = domain,
    to_theme = node_theme
  ) %>%
  mutate(
    search_blob = str_to_lower(
      paste(
        edge_id,
        from_label,
        to_label,
        interaction_family,
        theme,
        effect,
        mechanism,
        policy_tension,
        reference,
        evidence_level
      )
    )
  )

family_levels <- c(
  "AMR-SDG",
  "AMR-AMR",
  "SDG-SDG",
  "SDG-to-AMR Outcome",
  "AMR-to-AMR Outcome"
)

family_choices <- c(
  family_levels,
  setdiff(sort(unique(edges_enriched$interaction_family)), family_levels)
)

theme_choices <- sort(unique(edges_enriched$theme))
effect_choices <- c(
  "Increase AMR risk",
  "Reduce AMR risk",
  "Mixed / context-dependent"
)
evidence_choices <- c("High", "Medium", "Emerging")

all_node_choices <- setNames(nodes$id, paste0(nodes$label, " (", nodes$short_label, ")"))
outcome_nodes <- nodes %>% filter(node_type == "AMR Outcome")
outcome_choices <- setNames(outcome_nodes$id, outcome_nodes$label)

domain_palette <- c(
  "AMR" = "#0f766e",
  "SDG" = "#1d4ed8",
  "Outcome" = "#b45309"
)

effect_palette <- c(
  "Increase AMR risk" = "#b91c1c",
  "Reduce AMR risk" = "#15803d",
  "Mixed / context-dependent" = "#4b5563"
)

shape_by_type <- function(node_type, domain) {
  case_when(
    node_type == "AMR Outcome" ~ "star",
    domain == "SDG" ~ "box",
    node_type == "AMR Response" ~ "diamond",
    TRUE ~ "ellipse"
  )
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
      tags$span(class = "app-brand-title", "AMR-SDG Policy Explorer")
    )
  ),
  windowTitle = "BRIDGE-ABR AMR-SDG Explorer",
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
        line-height: 1.45;
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
      .pathway-summary {
        font-size: 14px;
        line-height: 1.45;
        margin-top: 10px;
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
    title = "Network Explorer",
    div(
      class = "page-wrap",
      h2("AMR-SDG interaction network"),
      div(
        class = "lead-note",
        p(
          "This app maps the policy tensions and synergies described in the BRIDGE-ABR concept note.",
          "You can filter AMR-SDG links, AMR-AMR dynamics, SDG-SDG interactions, and direct pathways",
          "to AMR outcomes."
        ),
        p(
          strong("Interpretation:"),
          " red edges increase AMR risk, green edges reduce AMR risk, and gray edges are context-dependent."
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
            div(class = "section-title", "Themes"),
            checkboxGroupInput(
              "theme_filter",
              label = NULL,
              choices = theme_choices,
              selected = theme_choices
            ),
            div(class = "section-title", "Effect on AMR outcomes"),
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
            div(class = "section-title", "Focus nodes (optional)"),
            selectizeInput(
              "focus_nodes",
              label = NULL,
              choices = all_node_choices,
              selected = character(0),
              multiple = TRUE,
              options = list(placeholder = "Type to select AMR or SDG nodes")
            ),
            textInput(
              "search_text",
              "Search mechanism, tension, source",
              value = ""
            ),
            checkboxInput("context_only", "Only context-dependent links", FALSE),
            checkboxInput("bidirectional_only", "Only bidirectional links", FALSE),
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
            visNetworkOutput("interaction_network", height = "650px"),
            div(
              class = "legend-row",
              div(class = "legend-chip", span(class = "legend-line"), span("Direct")),
              div(class = "legend-chip", span(class = "legend-line indirect"), span("Indirect")),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#b91c1c;"),
                span("Increase AMR risk")
              ),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#15803d;"),
                span("Reduce AMR risk")
              ),
              div(
                class = "legend-chip",
                span(class = "legend-dot", style = "background:#4b5563;"),
                span("Mixed / context-dependent")
              )
            ),
            div(class = "kpi-row", textOutput("kpi_text"))
          )
        )
      ),

      div(
        class = "table-block card",
        h4("Filtered interactions"),
        DTOutput("interaction_table")
      ),

      div(
        class = "table-block card",
        h4("References in current filtered view"),
        DTOutput("reference_table")
      )
    )
  ),

  tabPanel(
    title = "Outcome Pathways",
    div(
      class = "page-wrap",
      h2("Pathways to AMR outcomes"),
      div(
        class = "lead-note",
        p(
          "This view traces upstream links into one AMR outcome.",
          "Use pathway depth to show immediate drivers or broader chains."
        )
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "card",
            selectInput(
              "outcome_target",
              "Select AMR outcome",
              choices = outcome_choices,
              selected = outcome_nodes$id[1]
            ),
            sliderInput(
              "path_depth",
              "Pathway depth",
              min = 1,
              max = 3,
              value = 2,
              step = 1
            ),
            checkboxGroupInput(
              "pathway_theme",
              "Themes in pathway",
              choices = theme_choices,
              selected = theme_choices
            ),
            checkboxGroupInput(
              "pathway_effect",
              "Effects in pathway",
              choices = effect_choices,
              selected = effect_choices
            ),
            downloadButton("download_pathway", "Download pathway CSV")
          )
        ),
        column(
          width = 8,
          div(
            class = "card",
            visNetworkOutput("pathway_network", height = "620px"),
            uiOutput("pathway_summary")
          )
        )
      ),

      div(
        class = "table-block card",
        h4("Pathway interactions"),
        DTOutput("pathway_table")
      )
    )
  ),

  tabPanel(
    title = "Methods & Data",
    div(
      class = "page-wrap",
      h2("Product scope and data"),
      div(
        class = "card",
        p(
          "The current product version encodes workshop-relevant interfaces from the BRIDGE-ABR concept note",
          "across three thematic domains: Agriculture and Food Systems, Environment and Climate Action,",
          "and Economy Poverty and Equity, with additional Health Systems and Governance links."
        ),
        p(
          "You can replace the CSV files in /data with your own validated interaction evidence.",
          "Keep all required columns unchanged to ensure compatibility with the app logic."
        ),
        tags$ul(
          tags$li("Nodes file: data/amr_sdg_nodes.csv"),
          tags$li("Edges file: data/amr_sdg_edges.csv")
        )
      ),
      div(
        class = "table-block card",
        h4("Node dictionary"),
        DTOutput("node_table")
      ),
      div(
        class = "table-block card",
        h4("Interaction summary by family, theme, and effect"),
        DTOutput("summary_table")
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_edges <- reactive({
    df <- edges_enriched

    if (length(input$family_filter) > 0) {
      df <- df %>% filter(interaction_family %in% input$family_filter)
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
    updateCheckboxGroupInput(session, "theme_filter", selected = theme_choices)
    updateCheckboxGroupInput(session, "effect_filter", selected = effect_choices)
    updateCheckboxGroupInput(session, "evidence_filter", selected = evidence_choices)
    updateSelectizeInput(session, "focus_nodes", selected = character(0), server = TRUE)
    updateTextInput(session, "search_text", value = "")
    updateCheckboxInput(session, "context_only", value = FALSE)
    updateCheckboxInput(session, "bidirectional_only", value = FALSE)
  })

  output$kpi_text <- renderText({
    df <- filtered_edges()
    visible_nodes <- unique(c(df$from, df$to))
    paste0(
      "Visible links: ", nrow(df),
      " | Visible nodes: ", length(visible_nodes),
      " | Interaction families: ", dplyr::n_distinct(df$interaction_family)
    )
  })

  output$interaction_network <- renderVisNetwork({
    df <- filtered_edges()

    active_nodes <- unique(c(df$from, df$to, input$focus_nodes))

    plot_nodes <- nodes %>%
      mutate(
        active = id %in% active_nodes,
        group = domain,
        label_plot = short_label,
        title = paste0(
          "<b>", label, "</b><br>",
          "Type: ", node_type, "<br>",
          "Theme: ", theme, "<br>",
          description
        ),
        shape = shape_by_type(node_type, domain),
        color.background = ifelse(active, domain_palette[domain], "#d4d8df"),
        color.border = ifelse(active, "#1f2937", "#9ca3af"),
        font.color = ifelse(active, "#0f172a", "#6b7280"),
        size = case_when(
          node_type == "AMR Outcome" ~ 34,
          node_type == "SDG Goal" ~ 26,
          active ~ 23,
          TRUE ~ 18
        )
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
        group,
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
            "<b>", from_label, " -> ", to_label, "</b><br>",
            "Family: ", interaction_family, "<br>",
            "Theme: ", theme, "<br>",
            "Effect: ", effect, "<br>",
            "Mechanism: ", mechanism, "<br>",
            "Policy tension: ", policy_tension, "<br>",
            "Evidence: ", evidence_level, "<br>",
            "Source: ", reference
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
          width = ifelse(effect == "Mixed / context-dependent", 1.4, 2.2)
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

    visNetwork(plot_nodes, plot_edges, width = "100%", height = "650px") %>%
      visNodes(borderWidth = 1.3, shadow = FALSE) %>%
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
    tbl <- filtered_edges() %>%
      transmute(
        `Edge ID` = edge_id,
        `Interaction Family` = interaction_family,
        Theme = theme,
        From = from_label,
        To = to_label,
        Effect = effect,
        `Direct or Indirect` = direct_or_indirect,
        Bidirectional = ifelse(bidirectional, "Yes", "No"),
        `Context-Dependent` = ifelse(context_dependent, "Yes", "No"),
        `Evidence Level` = evidence_level,
        Mechanism = mechanism,
        `Policy Tension` = policy_tension,
        Reference = reference
      )

    datatable(
      tbl,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), scrollX = TRUE)
    )
  })

  output$reference_table <- renderDT({
    tbl <- filtered_edges() %>%
      group_by(reference) %>%
      summarise(
        `Interactions` = n(),
        `Families` = paste(sort(unique(interaction_family)), collapse = "; "),
        `Themes` = paste(sort(unique(theme)), collapse = "; "),
        `Related Edge IDs` = paste(sort(unique(edge_id)), collapse = ", "),
        .groups = "drop"
      ) %>%
      rename(`Reference Source` = reference)

    datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = 8, lengthChange = FALSE, scrollX = TRUE)
    )
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("bridge_abr_filtered_interactions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      out <- filtered_edges() %>%
        transmute(
          edge_id,
          from,
          from_label,
          to,
          to_label,
          interaction_family,
          theme,
          effect,
          direct_or_indirect,
          bidirectional,
          context_dependent,
          evidence_level,
          mechanism,
          policy_tension,
          reference
        )
      write_csv(out, file)
    }
  )

  pathway_edges <- reactive({
    target <- input$outcome_target
    depth <- input$path_depth

    if (is.null(target) || length(target) == 0) {
      return(edges_enriched[0, ])
    }

    base_df <- edges_enriched %>%
      filter(
        theme %in% input$pathway_theme,
        effect %in% input$pathway_effect
      )

    frontier <- target
    all_edges <- base_df[0, ]

    for (step in seq_len(depth)) {
      step_edges <- base_df %>% filter(to %in% frontier)
      if (nrow(step_edges) == 0) {
        next
      }
      step_edges <- step_edges %>% mutate(path_step = step)
      all_edges <- bind_rows(all_edges, step_edges)
      frontier <- unique(step_edges$from)
    }

    all_edges %>% distinct(edge_id, .keep_all = TRUE)
  })

  output$pathway_network <- renderVisNetwork({
    p_edges <- pathway_edges()
    target <- input$outcome_target

    if (nrow(p_edges) == 0 || is.null(target)) {
      return(
        visNetwork(
          nodes = data.frame(id = character(), label = character()),
          edges = data.frame(from = character(), to = character())
        )
      )
    }

    node_levels <- data.frame(id = target, level = 0)
    frontier <- target

    for (step in seq_len(input$path_depth)) {
      candidates <- p_edges %>%
        filter(to %in% frontier) %>%
        pull(from) %>%
        unique()

      new_ids <- setdiff(candidates, node_levels$id)
      if (length(new_ids) == 0) {
        frontier <- candidates
        next
      }

      node_levels <- bind_rows(node_levels, data.frame(id = new_ids, level = step))
      frontier <- candidates
    }

    plot_nodes <- nodes %>%
      filter(id %in% unique(c(p_edges$from, p_edges$to))) %>%
      left_join(node_levels, by = "id") %>%
      mutate(
        level = ifelse(is.na(level), input$path_depth + 1, input$path_depth - level),
        label_plot = short_label,
        title = paste0(
          "<b>", label, "</b><br>",
          "Type: ", node_type, "<br>",
          "Theme: ", theme
        ),
        shape = shape_by_type(node_type, domain),
        color.background = ifelse(id == target, "#b45309", domain_palette[domain]),
        color.border = ifelse(id == target, "#78350f", "#1f2937"),
        size = ifelse(id == target, 38, 24)
      )
    plot_nodes$color <- I(Map(
      function(background, border) list(background = background, border = border),
      plot_nodes$color.background,
      plot_nodes$color.border
    ))
    plot_nodes <- plot_nodes %>%
      transmute(
        id,
        label = label_plot,
        title,
        level,
        shape,
        color,
        size
      )

    plot_edges <- p_edges %>%
      mutate(
        edge_color = effect_palette[effect],
        edge_title = paste0(
          "<b>", from_label, " -> ", to_label, "</b><br>",
          "Path step: ", path_step, "<br>",
          "Effect: ", effect, "<br>",
          "Theme: ", theme, "<br>",
          "Mechanism: ", mechanism
        )
      ) %>%
      transmute(
        from,
        to,
        title = edge_title,
        dashes = direct_or_indirect == "Indirect",
        arrows = ifelse(bidirectional, "to;from", "to"),
        color = edge_color,
        width = ifelse(effect == "Mixed / context-dependent", 1.4, 2.1)
      )

    visNetwork(plot_nodes, plot_edges, width = "100%", height = "620px") %>%
      visNodes(borderWidth = 1.2) %>%
      visEdges(smooth = FALSE) %>%
      visHierarchicalLayout(
        direction = "LR",
        sortMethod = "directed",
        levelSeparation = 130,
        nodeSpacing = 130
      ) %>%
      visInteraction(hover = TRUE, navigationButtons = TRUE) %>%
      visPhysics(enabled = FALSE)
  })

  output$pathway_summary <- renderUI({
    p_edges <- pathway_edges()
    target <- input$outcome_target

    if (nrow(p_edges) == 0 || is.null(target)) {
      return(tags$div(class = "pathway-summary", "No pathways available under current filters."))
    }

    target_label <- nodes %>% filter(id == target) %>% pull(label)

    top_drivers <- p_edges %>%
      count(from_label, sort = TRUE) %>%
      slice_head(n = 5)

    top_themes <- p_edges %>%
      count(theme, sort = TRUE) %>%
      slice_head(n = 3)

    tags$div(
      class = "pathway-summary",
      tags$p(
        strong("Selected outcome:"),
        paste(target_label)
      ),
      tags$p(
        strong("Pathway size:"),
        paste0(nrow(p_edges), " links across ", length(unique(c(p_edges$from, p_edges$to))), " nodes")
      ),
      tags$p(
        strong("Most frequent upstream nodes:"),
        paste(paste0(top_drivers$from_label, " (", top_drivers$n, ")"), collapse = "; ")
      ),
      tags$p(
        strong("Dominant themes:"),
        paste(paste0(top_themes$theme, " (", top_themes$n, ")"), collapse = "; ")
      )
    )
  })

  output$pathway_table <- renderDT({
    tbl <- pathway_edges() %>%
      arrange(path_step, from_label, to_label) %>%
      transmute(
        `Path Step` = path_step,
        `Edge ID` = edge_id,
        `Interaction Family` = interaction_family,
        Theme = theme,
        From = from_label,
        To = to_label,
        Effect = effect,
        `Direct or Indirect` = direct_or_indirect,
        `Evidence Level` = evidence_level,
        Mechanism = mechanism,
        `Policy Tension` = policy_tension,
        Reference = reference
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), scrollX = TRUE)
    )
  })

  output$download_pathway <- downloadHandler(
    filename = function() {
      target_label <- nodes %>% filter(id == input$outcome_target) %>% pull(short_label)
      target_label <- ifelse(length(target_label) == 0, "outcome", target_label)
      paste0("bridge_abr_pathway_", gsub("[^A-Za-z0-9]", "_", target_label), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      out <- pathway_edges() %>%
        transmute(
          path_step,
          edge_id,
          from,
          from_label,
          to,
          to_label,
          interaction_family,
          theme,
          effect,
          direct_or_indirect,
          bidirectional,
          context_dependent,
          evidence_level,
          mechanism,
          policy_tension,
          reference
        )
      write_csv(out, file)
    }
  )

  output$node_table <- renderDT({
    tbl <- nodes %>%
      transmute(
        `Node ID` = id,
        Label = label,
        `Short Label` = short_label,
        `Node Type` = node_type,
        Domain = domain,
        Theme = theme,
        Description = description
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), scrollX = TRUE)
    )
  })

  output$summary_table <- renderDT({
    tbl <- edges_enriched %>%
      count(interaction_family, theme, effect, sort = TRUE) %>%
      rename(
        `Interaction Family` = interaction_family,
        Theme = theme,
        Effect = effect,
        `Number of Links` = n
      )

    datatable(
      tbl,
      rownames = FALSE,
      options = list(pageLength = 12, lengthChange = FALSE, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)
