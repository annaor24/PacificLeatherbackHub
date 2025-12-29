# app.R - Pacific Leatherback Hub (No Intro Pages)

# --- Prevent rsconnect from trying to use renv on deployment ---
if (requireNamespace("rsconnect", quietly = TRUE)) {
  options(rsconnect.force.update.dependencies = TRUE)
  options(renv.consent = FALSE)
  Sys.setenv(RENV_PROJECT = "")
  Sys.unsetenv("RENV_PROJECT")
}
# -------------------------
# Libraries
# -------------------------
library(shiny)
library(shinydashboard)
library(bslib)
library(shinyjs)
library(leaflet)
# library(sf)
library(dplyr)
library(shinycssloaders)
# library(rmapshaper)
library(RColorBrewer)
library(shinyWidgets)
library(tidyr)
library(ggplot2)
library(DT)
library(plotly)
library(jsonlite)

options(timeout = 2000)

# -------------------------
# Helper Functions
# -------------------------

# Find first column that matches any pattern (case-insensitive, fixed)
find_col <- function(df, pattern_vec) {
  nm <- names(df)
  nm_low <- tolower(nm)
  for (p in pattern_vec) {
    idx <- which(grepl(p, nm_low, fixed = TRUE))
    if (length(idx) > 0) return(nm[idx[1]])
  }
  return(NULL)
}

# Decision metadata box (UI helper)
decisionMetaBox <- function(data, scale, use) {
  div(
    style = "
      background: rgba(0,0,0,0.55);
      padding: 12px;
      border-left: 4px solid #5bc0de;
      border-radius: 6px;
      margin-bottom: 15px;
    ",
    h5("📊 Evidence used", style = "margin-top:0;"),
    tags$ul(lapply(data, tags$li)),
    tags$p(
      strong("Management scale: "), scale, tags$br(),
      strong("Primary use: "), use,
      style = "font-size: 0.9em; color:#ccc;"
    )
  )
}

# Back-to-hub button (UI helper)
backToHubButton <- function(page_num) {
  absolutePanel(
    class = "absPanel",
    top   = 10,
    right = 10,
    actionButton(
      inputId = paste0("back_to_hub_", page_num),
      label   = "Back to Hub",
      class   = "navBtn"
    )
  )
}

# Hub tool-card (UI helper)
toolCard <- function(page, title, description, emoji = "") {
  div(
    class   = "tool-card",
    onclick = sprintf(
      "Shiny.setInputValue('goto_page', %d, {priority:'event'})",
      page
    ),
    h4(paste0(emoji, if (nzchar(emoji)) " " else "", title)),
    p(description)
  )
}

# Main CSS (kept identical to original styling)
app_css <- "
  html, body { margin:0; padding:0; height:100%; width:100%; overflow:hidden; }
  .absPanel { background: rgba(0,0,0,0.6); padding: 10px; border-radius: 8px; }
  .navBtn { margin-top: 10px; }
  .fade-page {
    opacity:0;
    transition: opacity 0.6s;
    height:100vh;
    width:100%;
    position:absolute;
    top:0;
    left:0;
    pointer-events:none;
    background:#0b2a3a;
    color: #fff;
    overflow-y: auto;
  }
  .fade-page.active { opacity:1; z-index:10; pointer-events:auto; }

  /* (legacy splash styles left harmlessly) */
  img.page-img { width:100%; height:100%; object-fit:contain; object-position:center; }

  /* page5 layout */
  #page5 {
    display:flex;
    flex-direction:row;
    height:100vh;
    width:100%;
    overflow:hidden;
    background: #071722;
  }
  #sidebar-container {
    width:320px;
    background: rgba(0,0,0,0.7);
    padding:15px;
    overflow-y:auto;
    color:white;
  }
  #main-container {
    flex-grow:1;
    padding:10px 20px;
    overflow-y:auto;
  }

  /* content pages */
  .content-panel {
    background: rgba(255,255,255,0.03);
    padding:20px;
    border-radius:8px;
    margin:20px;
    color:#fff;
  }
  .tool-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 15px;
    margin: 20px 0;
  }
  .tool-card {
    background: rgba(255,255,255,0.05);
    border: 1px solid rgba(255,255,255,0.1);
    border-radius: 8px;
    padding: 20px;
    cursor: pointer;
    transition: all 0.3s;
  }
  .tool-card:hover {
    background: rgba(255,255,255,0.1);
    transform: translateY(-2px);
  }
  .tool-card h4 {
    margin-top: 0;
    color: #5bc0de;
  }
"

# -------------------------
# Load Datasets (lazy + cached)
# -------------------------

.data_long_cache <- NULL
load_data_long <- function() {
  if (is.null(.data_long_cache)) {
    .data_long_cache <<- readRDS("data_long_compressed.rds")
  }
  .data_long_cache
}

.gfw_cache <- NULL
load_gfw_data <- function() {
  if (is.null(.gfw_cache)) {
    df <- readRDS("gfw_data_compressed.rds")
    # Filter out rows with NA effort once, to avoid duplication later
    df <- df[!is.na(df$Apparent.Fishing.Hours), , drop = FALSE]
    .gfw_cache <<- df
  }
  .gfw_cache
}

.gfw_eez_cache <- NULL
load_gfw_eez <- function() {
  if (is.null(.gfw_eez_cache)) {
    .gfw_eez_cache <<- readRDS("gfw_eez_compressed.rds")
  }
  .gfw_eez_cache
}

mitigation_data <- read.csv("mitigation.csv", stringsAsFactors = FALSE, check.names = FALSE)
interaction_rates <- read.csv("interactionrates.csv", stringsAsFactors = FALSE)
cap_data <- read.csv("captochange.csv", stringsAsFactors = FALSE) %>%
  mutate(
    total_mortality = as.numeric(total_mortality),
    mean_rank       = as.numeric(mean_rank)
  )

# If eez_pacific_simplified.rds is large and unused, comment it out to save memory
# eez_pacific_simple <- readRDS("eez_pacific_simplified.rds")

# -------------------------
# Clean mitigation data
# -------------------------

names(mitigation_data) <- trimws(names(mitigation_data))

col_gear     <- find_col(mitigation_data, c("gear", "geartype", "gear_type"))
col_species  <- find_col(mitigation_data, c("species", "speciesgroup", "species_group"))
col_category <- find_col(mitigation_data, c("measurecategory", "measure_category", "category"))
col_tech     <- find_col(mitigation_data, c("mitigationtechnique", "mitigation_technique", "technique", "mitigation"))
col_desc     <- find_col(mitigation_data, c("description", "desc"))
col_exp      <- find_col(mitigation_data, c("experimental", "experiment"))
col_status   <- find_col(mitigation_data, c("status"))

mit <- mitigation_data %>%
  dplyr::rename(
    GearType            = !!col_gear,
    SpeciesGroup        = !!col_species,
    MeasureCategory     = !!col_category,
    MitigationTechnique = !!col_tech,
    Description         = !!col_desc
  )

if (!is.null(col_exp))    mit <- mit %>% dplyr::rename(Experimental = !!col_exp)
if (!is.null(col_status)) mit <- mit %>% dplyr::rename(Status       = !!col_status)

mit <- mit %>%
  mutate(across(everything(), as.character))

# Gear colors (GFW mitigation table)
gear_levels <- unique(mit$GearType)
n_gear      <- length(gear_levels)
pal         <- RColorBrewer::brewer.pal(min(max(n_gear, 3), 8), "Set2")
if (length(pal) < n_gear) pal <- rep(pal, length.out = n_gear)
gear_colors <- setNames(pal[1:n_gear], gear_levels)

# GFW fishing effort color palette
# We only derive gear_types for color mapping; data itself is loaded lazily via load_gfw_data()
gear_types <- unique(load_gfw_data()$Geartype)
gear_colors_gfw <- setNames(
  RColorBrewer::brewer.pal(min(length(gear_types), 9), "Set1")[1:length(gear_types)],
  gear_types
)

# -------------------------
# UI
# -------------------------

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "darkly"),
  
  tags$head(
    tags$style(HTML(app_css))
  ),
  
  # PAGE 5: Decision-Driven Conservation Hub (start page)
  div(
    id = "page5",
    class = "fade-page active",
    div(
      style = "width:100%; padding:40px; overflow-y:auto;",
      
      h2(
        "Pacific Leatherback Conservation Decision Hub",
        style = "text-align:center; margin-bottom:10px;"
      ),
      p(
        "This hub organizes data, models, and tools around the real questions faced by fisheries managers, conservation planners, and RFMOs.",
        style = "text-align:center; color:#aaa; margin-bottom:40px;"
      ),
      
      h3("1️⃣ Where are leatherbacks most at risk?", style = "color:#5bc0de;"),
      p("Identify spatial overlap between turtles and human activity."),
      div(
        class = "tool-grid",
        toolCard(
          page        = 13,
          title       = "Dynamic Hotspots",
          description = "Near-real-time turtle density alerts.",
          emoji       = "🔴"
        ),
        toolCard(
          page        = 7,
          title       = "Fishing Effort Overlap",
          description = "Map fishing effort by gear and season.",
          emoji       = "🗺️"
        ),
        toolCard(
          page        = 12,
          title       = "Migration Corridors",
          description = "Link nesting beaches to foraging grounds.",
          emoji       = "🐢"
        )
      ),
      
      h3("2️⃣ Who is contributing most to risk?", style = "color:#5bc0de; margin-top:40px;"),
      div(
        class = "tool-grid",
        toolCard(
          page        = 10,
          title       = "Mortality Estimates",
          description = "Estimated mortality by flag and fishery.",
          emoji       = "⚠️"
        ),
        toolCard(
          page        = 6,
          title       = "EEZ Responsibility",
          description = "Fishing effort summarized by EEZ.",
          emoji       = "🌊"
        ),
        toolCard(
          page        = 11,
          title       = "Capacity to Change",
          description = "Ability to implement mitigation.",
          emoji       = "📊"
        )
      ),
      
      h3("3️⃣ What actions reduce risk?", style = "color:#5bc0de; margin-top:40px;"),
      div(
        class = "tool-grid",
        toolCard(
          page        = 9,
          title       = "Mitigation Options",
          description = "Gear-specific bycatch solutions.",
          emoji       = "🛠️"
        ),
        toolCard(
          page        = 17,
          title       = "Scenario Planning",
          description = "Test management combinations.",
          emoji       = "🎯"
        ),
        toolCard(
          page        = 18,
          title       = "Policy Effectiveness",
          description = "What has worked elsewhere?",
          emoji       = "📋"
        )
      ),
      
      h3("4️⃣ What are the consequences?", style = "color:#5bc0de; margin-top:40px;"),
      div(
        class = "tool-grid",
        toolCard(
          page        = 8,
          title       = "Population Futures",
          description = "Population projections.",
          emoji       = "📈"
        ),
        toolCard(
          page        = 16,
          title       = "Sensitivity Analysis",
          description = "Key life stages.",
          emoji       = "🔬"
        ),
        toolCard(
          page        = 15,
          title       = "Economic Trade-offs",
          description = "Cost implications.",
          emoji       = "💰"
        )
      ),
      
      h3("5️⃣ Data & participation", style = "color:#5bc0de; margin-top:40px;"),
      div(
        class = "tool-grid",
        toolCard(
          page        = 19,
          title       = "Data Portal",
          description = "Browse and contribute data.",
          emoji       = "💾"
        ),
        toolCard(
          page        = 21,
          title       = "Report Sightings",
          description = "Support monitoring.",
          emoji       = "📝"
        )
      )
    )
  ),
  
  # PAGE 6: EEZ Summary
  div(
    id = "page6",
    class = "fade-page",
    backToHubButton(6),
    fluidRow(
      column(
        3,
        div(
          class = "content-panel",
          h4("EEZ Summary Controls"),
          selectInput("sum_flag", "Select Flag:", choices = sort(unique(load_gfw_eez()$Flag))),
          selectInput(
            "sum_year",
            "Select Year:",
            choices  = sort(unique(load_gfw_eez()$Year)),
            selected = tail(sort(unique(load_gfw_eez()$Year)), 1)
          ),
          actionButton("sum_load", "Load Summary", class = "btn btn-primary w-100")
        )
      ),
      column(
        9,
        div(
          class = "content-panel",
          h3("EEZ Fishing Summary"),
          DTOutput("eez_summary_dt") %>% withSpinner(),
          plotOutput("eez_summary_plot", height = "500px") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 7: Fishing Effort Map
  div(
    id = "page7",
    class = "fade-page",
    backToHubButton(7),
    div(
      class = "content-panel",
      fluidRow(
        column(
          3,
          h4("Fishing Effort Map"),
          decisionMetaBox(
            data = c(
              "Global Fishing Watch apparent fishing effort",
              "Gear-specific interaction rates",
              "Spatial fishing activity records"
            ),
            scale = "EEZ / RFMO / High seas",
            use   = "Identify spatial risk hotspots and candidate closures"
          ),
          selectInput(
            "geartype_fs",
            "Geartypes:",
            choices  = unique(load_gfw_data()$Geartype),
            selected = unique(load_gfw_data()$Geartype)[1],
            multiple = TRUE
          ),
          selectInput(
            "flag_fs",
            "Flags:",
            choices  = unique(load_gfw_data()$Flag),
            selected = unique(load_gfw_data()$Flag)[1],
            multiple = TRUE
          ),
          dateRangeInput(
            "date_range_fs",
            "Date Range:",
            start = min(as.Date(load_gfw_data()$Time.Range), na.rm = TRUE),
            end   = max(as.Date(load_gfw_data()$Time.Range), na.rm = TRUE)
          ),
          checkboxInput("show_mortality_fs", "Show mortality estimate", FALSE),
          actionButton("plot_fs", "Generate Map", class = "btn btn-primary w-100")
        ),
        column(
          9,
          leafletOutput("map_plot", height = "70vh") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 8: Population Models
  div(
    id = "page8",
    class = "fade-page",
    backToHubButton(8),
    div(
      class = "content-panel",
      fluidRow(
        column(
          3,
          h4("Population Model Controls"),
          decisionMetaBox(
            data = c(
              "Demographic population models",
              "Scenario-based survival and recruitment parameters",
              "Bycatch reduction and headstarting assumptions"
            ),
            scale = "Population / Regional stock",
            use   = "Evaluate long-term population outcomes under management scenarios"
          ),
          selectInput("subpop_fs", "Subpopulation:", choices = unique(load_data_long()$subpop)),
          checkboxGroupInput(
            "scenario_fs",
            "Scenarios:",
            choices  = c(
              "Status quo"          = "status_quo",
              "Bycatch reduction"   = "bycatch",
              "Bycatch + headstart" = "bycatch_headstart"
            ),
            selected = "status_quo"
          ),
          uiOutput("dynamic_controls_fs"),
          radioButtons(
            "source_fs",
            "Output:",
            choices = c(
              "Population (N)"   = "N",
              "Survival Prob"    = "Psurv",
              "Nesters"          = "NF"
            ),
            selected = "Psurv"
          ),
          downloadButton("downloadSummary_fs", "Download")
        ),
        column(
          9,
          plotOutput("populationPlot_fs", height = "500px") %>% withSpinner(),
          DTOutput("summaryTable_fs") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 9: Mitigation Quiz
  div(
    id = "page9",
    class = "fade-page",
    backToHubButton(9),
    div(
      class = "content-panel",
      fluidRow(
        column(
          3,
          h4("Mitigation Strategies"),
          selectInput("gear_type_fs", "Gear Type:", choices = unique(mit$GearType)),
          selectInput("species_group_fs", "Species Group:", choices = unique(mit$SpeciesGroup))
        ),
        column(
          9,
          uiOutput("mitigation_output_fs") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 10: Mortality Estimates
  div(
    id = "page10",
    class = "fade-page",
    backToHubButton(10),
    div(
      class = "content-panel",
      h2("Estimated Annual Mortality by Flag"),
      decisionMetaBox(
        data = c(
          "Fishing effort by flag state",
          "Observer-based turtle interaction rates",
          "Fleet-level mortality estimates"
        ),
        scale = "National / RFMO",
        use   = "Identify fisheries contributing most to leatherback mortality"
      ),
      fluidRow(
        column(
          12,
          plotlyOutput("mortality_plot", height = "400px") %>% withSpinner()
        )
      ),
      fluidRow(
        column(
          12,
          h4("Detailed Mortality Data", style = "margin-top:30px;"),
          DTOutput("mortality_table") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 11: Capacity to Change
  div(
    id = "page11",
    class = "fade-page",
    backToHubButton(11),
    div(
      class = "content-panel",
      h2("Fisheries Capacity to Change"),
      decisionMetaBox(
        data = c(
          "Estimated leatherback mortality by fishery",
          "Economic and governance indicators",
          "Mitigation feasibility rankings"
        ),
        scale = "National / RFMO",
        use   = "Assess where mitigation adoption is most feasible"
      ),
      p("Compare fisheries across economic flexibility and mitigation feasibility."),
      fluidRow(
        column(
          12,
          plotlyOutput("capacity_plot", height = "500px") %>% withSpinner()
        )
      ),
      fluidRow(
        column(
          12,
          h4("Capacity Rankings", style = "margin-top:30px;"),
          DTOutput("capacity_table") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 12: Migration Corridors
  div(
    id = "page12",
    class = "fade-page",
    backToHubButton(12),
    div(
      class = "content-panel",
      h2("Migration Corridors & Beach Connectivity"),
      fluidRow(
        column(
          3,
          selectInput(
            "nesting_beach",
            "Select Nesting Beach:",
            choices = c(
              "All",
              "Playa Grande (Costa Rica)",
              "Jamursba-Medi (Indonesia)",
              "Wermon (Indonesia)"
            )
          )
        ),
        column(
          9,
          leafletOutput("migration_map", height = "600px") %>% withSpinner(),
          p(
            "Migration routes connect nesting beaches to foraging grounds across vast ocean distances.",
            style = "margin-top:15px; color:#aaa;"
          )
        )
      )
    )
  ),
  
  # PAGE 13: Hotspot Alerts
  div(
    id = "page13",
    class = "fade-page",
    backToHubButton(13),
    div(
      class = "content-panel",
      h2("Dynamic Bycatch Risk & Management Guidance"),
      div(
        style = "background: rgba(255,193,7,0.2); padding:15px; border-radius:5px; border-left:4px solid #ffc107;",
        h4("⚠️ Current High-Density Areas", style = "margin-top:0;"),
        p("Based on satellite tracking, oceanographic conditions, and historical data.")
      ),
      div(
        style = "
          background: rgba(255,255,255,0.05);
          padding: 40px;
          border-radius: 10px;
          text-align: center;
          max-width: 1000px;
          margin: 40px auto;
        ",
        h3(
          "Dynamic Bycatch Risk & Management Guidance",
          style = "color:#5bc0de;"
        ),
        p(
          style = "color:#ccc; font-size:1.1em; max-width:850px; margin: 20px auto;",
          "This links directly to the South Pacific TurtleWatch (SPTW),
           developed by Upwell. SPTW provides dynamic, near-real-time bycatch risk guidance
           used operationally by fisheries managers."
        ),
        br(),
        tags$button(
          class   = "btn btn-danger btn-lg",
          onclick = "window.open('https://www.upwell.org/sptw', '_blank');",
          "Open Upwell SPTW (New Tab)"
        ),
        br(), br(),
        p(
          style = "font-size:0.9em; color:#999;",
          "SPTW integrates satellite oceanography, species movement models,
           and fisheries activity. Content is hosted and maintained externally by Upwell."
        )
      )
    )
  ),
  
  # PAGE 14: Video Guides
  div(
    id = "page14",
    class = "fade-page",
    backToHubButton(14),
    div(
      class = "content-panel",
      h2("Mitigation Technique Video Guides"),
      fluidRow(
        column(
          4,
          div(
            style = "border:1px solid #555; padding:15px; border-radius:5px;",
            h4("Circle Hooks"),
            div(
              style = "background:#333; height:200px; display:flex; align-items:center; justify-content:center;",
              "[Video Placeholder]"
            ),
            p("Demonstration of proper circle hook use and effectiveness.", style = "margin-top:10px;")
          )
        ),
        column(
          4,
          div(
            style = "border:1px solid #555; padding:15px; border-radius:5px;",
            h4("TEDs (Turtle Excluder Devices)"),
            div(
              style = "background:#333; height:200px; display:flex; align-items:center; justify-content:center;",
              "[Video Placeholder]"
            ),
            p("Installing and maintaining TEDs in trawl nets.", style = "margin-top:10px;")
          )
        ),
        column(
          4,
          div(
            style = "border:1px solid #555; padding:15px; border-radius:5px;",
            h4("Safe Handling & Release"),
            div(
              style = "background:#333; height:200px; display:flex; align-items:center; justify-content:center;",
              "[Video Placeholder]"
            ),
            p("Best practices for releasing entangled turtles.", style = "margin-top:10px;")
          )
        )
      )
    )
  ),
  
  # PAGE 15: Economic Impact Calculator
  div(
    id = "page15",
    class = "fade-page",
    backToHubButton(15),
    div(
      class = "content-panel",
      h2("Economic Impact Calculator"),
      p("Estimate the annual costs of turtle interactions for your fishery."),
      fluidRow(
        column(
          6,
          h4("Input Your Fishery Parameters"),
          numericInput("num_vessels",       "Number of Vessels:",          value = 10, min = 1),
          numericInput("avg_interactions",  "Avg. Turtle Interactions/Year:", value = 5),
          numericInput("gear_damage_cost",  "Avg. Gear Damage Cost ($):",  value = 500),
          numericInput("lost_time_hours",   "Lost Fishing Time (hours):",  value = 4),
          actionButton("calculate_economics", "Calculate Impact", class = "btn-primary")
        ),
        column(
          6,
          h4("Estimated Annual Costs"),
          valueBoxOutput("total_cost_box", width = 12),
          plotlyOutput("cost_breakdown", height = "300px") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 16: Demographic Sensitivity
  div(
    id = "page16",
    class = "fade-page",
    backToHubButton(16),
    div(
      class = "content-panel",
      h2("Demographic Sensitivity Analysis"),
      p("Shows which life stages have the greatest impact on population growth rates."),
      fluidRow(
        column(
          12,
          plotlyOutput("sensitivity_plot", height = "500px") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 17: Scenario Planning
  div(
    id = "page17",
    class = "fade-page",
    backToHubButton(17),
    div(
      class = "content-panel",
      h2("Scenario Planning Tool"),
      decisionMetaBox(
        data = c(
          "Mortality estimates",
          "Mitigation effectiveness assumptions",
          "Economic cost proxies",
          "Population model outputs"
        ),
        scale = "EEZ / RFMO",
        use   = "Compare combined management strategies and trade-offs"
      ),
      fluidRow(
        column(
          4,
          h4("Design Your Scenario"),
          checkboxGroupInput(
            "scenario_measures",
            "Management Measures:",
            choices = c(
              "Spatial Closure (Area A)",
              "Seasonal Closure (Months 6-8)",
              "Mandatory Circle Hooks",
              "Gillnet Ban",
              "Observer Coverage 50%"
            )
          ),
          actionButton("run_scenario", "Run Scenario", class = "btn-success")
        ),
        column(
          8,
          h4("Predicted Outcomes"),
          fluidRow(
            column(6, valueBoxOutput("scenario_mortality", width = 12)),
            column(6, valueBoxOutput("scenario_economic",  width = 12))
          ),
          plotlyOutput("scenario_comparison", height = "300px") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 18: Policy Effectiveness
  div(
    id = "page18",
    class = "fade-page",
    backToHubButton(18),
    div(
      class = "content-panel",
      h2("Policy Effectiveness Comparisons"),
      p("Case studies from different regions showing regulatory outcomes."),
      fluidRow(
        column(
          12,
          DTOutput("policy_table") %>% withSpinner()
        )
      )
    )
  ),
  
  # PAGE 19: Data Portal
  div(
    id = "page19",
    class = "fade-page",
    backToHubButton(19),
    div(
      class = "content-panel",
      h2("Integrated Data Portal"),
      tabsetPanel(
        tabPanel(
          "Browse Data",
          DTOutput("data_browser") %>% withSpinner()
        ),
        tabPanel(
          "Upload Data",
          fileInput("data_upload", "Upload CSV/Excel File:"),
          p("Contribute nest counts, hatchling success, or sighting data."),
          actionButton("submit_data", "Submit", class = "btn-primary")
        ),
        tabPanel(
          "Download Data",
          selectInput(
            "download_dataset",
            "Select Dataset:",
            choices = c(
              "Fishing Effort",
              "Mortality Estimates",
              "Nesting Data",
              "Tracking Data"
            )
          ),
          downloadButton("download_data", "Download CSV")
        )
      )
    )
  ),
  
  # PAGE 20: Seafood Sustainability
  div(
    id = "page20",
    class = "fade-page",
    backToHubButton(20),
    div(
      class = "content-panel",
      h2("Seafood Sustainability Guide"),
      tabsetPanel(
        tabPanel(
          "Ratings",
          h4("Traffic-light system showing lower leatherback impact products"),
          DTOutput("ratings_table") %>% withSpinner()
        ),
        tabPanel(
          "Impact Calculator",
          fluidRow(
            column(
              6,
              h4("Your Seafood Consumption"),
              numericInput("tuna_servings",   "Tuna servings/week:",   value = 2, min = 0),
              numericInput("shrimp_servings", "Shrimp servings/week:", value = 1, min = 0),
              selectInput(
                "purchase_preference",
                "Purchase Preference:",
                choices = c("Price", "Sustainability", "Mixed")
              ),
              actionButton("calc_consumer_impact", "Calculate Impact", class = "btn-primary")
            ),
            column(
              6,
              h4("Your Estimated Annual Impact"),
              valueBoxOutput("consumer_impact_box", width = 12),
              plotOutput("consumer_alternatives", height = "300px")
            )
          )
        )
      )
    )
  ),
  
  # PAGE 21: Report Sightings
  div(
    id = "page21",
    class = "fade-page",
    backToHubButton(21),
    div(
      class = "content-panel",
      h2("Report Turtle Sightings or Interactions"),
      fluidRow(
        column(
          6,
          textInput("reporter_name", "Your Name (optional):"),
          dateInput("sighting_date", "Date of Sighting:"),
          textInput("sighting_location", "Location (lat/long or description):"),
          selectInput(
            "interaction_type",
            "Type of Interaction:",
            choices = c(
              "Live Sighting",
              "Dead/Injured",
              "Entangled in Gear",
              "Nesting Activity",
              "Other"
            )
          ),
          textAreaInput("sighting_details", "Additional Details:", rows = 4),
          fileInput("sighting_photo", "Upload Photo (optional):"),
          actionButton("submit_sighting", "Submit Report", class = "btn-primary")
        ),
        column(
          6,
          div(
            style = "background:rgba(92,184,92,0.2); padding:20px; border-radius:5px; border-left:4px solid #5cb85c;",
            h4("Why Report?", style = "margin-top:0;"),
            p("Your observations help researchers:"),
            tags$ul(
              tags$li("Track population trends"),
              tags$li("Identify critical habitats"),
              tags$li("Document fishing interactions"),
              tags$li("Inform conservation strategies")
            ),
            p("All reports are confidential and used for research purposes only.")
          )
        )
      )
    )
  )
) # end UI

# -------------------------
# Server
# -------------------------

server <- function(input, output, session) {
  
  # -------------------------
  # SIMPLE LOGIN PROTECTION
  # -------------------------
  USERNAME <- "betatest"
  PASSWORD <- "ISTS2026"
  authed   <- reactiveVal(FALSE)
  
  observe({
    if (!authed()) {
      showModal(
        modalDialog(
          title = "Protected Access",
          textInput("login_user", "Username:"),
          passwordInput("login_pass", "Password:"),
          footer = actionButton("login_btn", "Log in", class = "btn-primary"),
          easyClose = FALSE,
          fade = TRUE
        )
      )
    }
  })
  
  observeEvent(input$login_btn, {
    if (identical(input$login_user, USERNAME) &&
        identical(input$login_pass, PASSWORD)) {
      
      authed(TRUE)
      removeModal()
      showNotification("Welcome to the Hub", type = "message", duration = 3)
      
    } else {
      showNotification("Invalid username or password", type = "error", duration = 5)
    }
  })
  
  # Helper to toggle pages
  toggle_page <- function(from, to) {
    runjs(sprintf(
      "$('#%s').removeClass('active'); $('#%s').addClass('active');",
      from, to
    ))
  }
  
  # Navigation from hub to tools
  observeEvent(input$goto_page, {
    toggle_page("page5", paste0("page", input$goto_page))
  })
  
  # Back to hub buttons for pages 6–21
  for (i in 6:21) {
    local({
      page_num <- i
      btn_id   <- paste0("back_to_hub_", page_num)
      observeEvent(input[[btn_id]], {
        toggle_page(paste0("page", page_num), "page5")
      })
    })
  }
  
  # -------------------------
  # PAGE 6: EEZ Summary
  # -------------------------
  
  observeEvent(input$sum_load, {
    req(input$sum_flag, input$sum_year)
    
    df <- load_gfw_eez() %>%
      filter(Flag == input$sum_flag, Year == input$sum_year)
    
    output$eez_summary_dt <- renderDT({
      datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$eez_summary_plot <- renderPlot({
      df2 <- df %>%
        group_by(Geartype) %>%
        summarise(TotalHours = sum(Apparent.Fishing.Hours, na.rm = TRUE))
      
      ggplot(df2, aes(Geartype, TotalHours, fill = Geartype)) +
        geom_col() +
        theme_minimal() +
        labs(
          title = paste("Fishing Hours by Gear for", input$sum_flag, input$sum_year),
          x     = "Gear Type",
          y     = "Total Hours"
        )
    })
  })
  
  # -------------------------
  # PAGE 7: Fishing Effort Map
  # -------------------------
  
  filtered_data_map_fs <- eventReactive(input$plot_fs, {
    req(load_gfw_data(), input$geartype_fs, input$flag_fs, input$date_range_fs)
    
    df <- load_gfw_data()
    if ("Time.Range" %in% names(df) && !inherits(df$Time.Range, "Date")) {
      try({
        df$Time.Range <- as.Date(df$Time.Range)
      }, silent = TRUE)
    }
    
    df %>%
      filter(
        Geartype %in% input$geartype_fs,
        Flag     %in% input$flag_fs,
        Time.Range >= input$date_range_fs[1],
        Time.Range <= input$date_range_fs[2]
      )
  })
  
  # Aggregate/bucket points to reduce marker count and memory
  output$map_plot <- renderLeaflet({
    req(filtered_data_map_fs())
    
    df <- filtered_data_map_fs() %>%
      mutate(
        Lon_bin = round(Lon, 2),
        Lat_bin = round(Lat, 2)
      ) %>%
      group_by(Lon_bin, Lat_bin, Geartype, Flag) %>%
      summarise(
        Hours = sum(Apparent.Fishing.Hours, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Geartype_Flag = paste(Geartype, Flag, sep = "_"))
    
    color_map <- colorFactor("Set1", domain = unique(df$Geartype_Flag))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon_bin,
        lat = ~Lat_bin,
        radius = ~pmin(6, sqrt(Hours) / 2),
        stroke = FALSE,
        fillOpacity = 0.8,
        color = ~color_map(Geartype_Flag),
        popup = ~paste(
          "Effort Hours:", round(Hours, 1),
          "<br>Gear:", Geartype,
          "<br>Flag:", Flag
        )
      ) %>%
      addLegend(
        position = "bottomright",
        title    = "Fishing Effort",
        pal      = color_map,
        values   = df$Geartype_Flag
      )
  })
  
  mortality_estimate_fs <- reactive({
    if (!isTRUE(input$show_mortality_fs)) return(NULL)
    req(filtered_data_map_fs())
    
    effort_df <- filtered_data_map_fs()
    if (nrow(effort_df) == 0) return(0)
    
    trim_all <- function(x) {
      x <- gsub("[[:space:]]+", " ", x)
      x <- gsub("\u00A0", " ", x)
      trimws(tolower(x))
    }
    
    ir <- interaction_rates %>%
      rename(
        geartype = Geartype,
        flag     = Flag,
        rate     = avg.tmpfh
      ) %>%
      mutate(
        geartype = trim_all(as.character(geartype)),
        flag     = trim_all(as.character(flag))
      )
    
    eff <- effort_df %>%
      mutate(
        geartype = trim_all(as.character(Geartype)),
        flag     = trim_all(as.character(Flag))
      )
    
    gear_only_rates <- ir %>%
      filter(flag == "" | is.na(flag)) %>%
      select(geartype, fallback_rate = rate)
    
    joined <- eff %>%
      left_join(ir, by = c("geartype", "flag")) %>%
      left_join(gear_only_rates, by = "geartype") %>%
      mutate(
        final_rate = ifelse(is.na(rate) | rate == 0, fallback_rate, rate),
        final_rate = as.numeric(final_rate),
        estimated_mortality = as.numeric(Apparent.Fishing.Hours) * final_rate
      )
    
    sum(joined$estimated_mortality, na.rm = TRUE)
  })
  
  observe({
    req(input$plot_fs)
    
    est   <- mortality_estimate_fs()
    proxy <- leafletProxy("map_plot")
    
    proxy %>% clearControls()
    
    if (!is.null(est) && !is.na(est)) {
      label_html <- paste0(
        "<div style='padding:8px; background: rgba(255,255,255,0.95); ",
        "border-radius:4px; border:2px solid #d9534f;'>",
        "<b style='color:#d9534f;'>Estimated Mortality:</b><br>",
        "<span style='font-size:18px; font-weight:bold;'>",
        format(round(est, 1), big.mark = ","),
        "</span> turtles",
        "</div>"
      )
      proxy %>% addControl(html = label_html, position = "topleft")
    }
  })
  
  # -------------------------
  # PAGE 8: Population Models
  # -------------------------
  
  output$dynamic_controls_fs <- renderUI({
    req(input$scenario_fs)
    
    out <- list()
    
    if (any(input$scenario_fs %in% c("bycatch", "bycatch_headstart"))) {
      out <- append(
        out,
        list(
          pickerInput(
            "reduction_fs",
            "Bycatch Reduction (%):",
            choices  = c(0, 10, 20, 30, 40, 50),
            selected = 0,
            multiple = TRUE
          )
        )
      )
    }
    
    if ("bycatch_headstart" %in% input$scenario_fs) {
      out <- append(
        out,
        list(
          pickerInput(
            "eggs_fs",
            "Headstarted Eggs:",
            choices  = c(2000, 4000, 6000),
            selected = 2000,
            multiple = TRUE
          ),
          pickerInput(
            "duration_fs",
            "Duration (years):",
            choices  = c(20, 40),
            selected = 20,
            multiple = TRUE
          )
        )
      )
    }
    
    tagList(out)
  })
  
  filtered_data_pop_fs <- reactive({
    req(input$subpop_fs, input$source_fs)
    
    df <- load_data_long() %>%
      filter(subpop == input$subpop_fs) %>%
      filter(source == input$source_fs)
    
    if (!is.null(input$scenario_fs)) {
      keep_scen <- unique(c("status_quo", input$scenario_fs))
      df <- df %>%
        filter(scenario %in% keep_scen)
    }
    
    if (!is.null(input$reduction_fs)) {
      df <- df %>%
        filter((scenario != "bycatch") | (reduction %in% input$reduction_fs))
    }
    
    if ("bycatch_headstart" %in% input$scenario_fs) {
      df <- df %>%
        filter(
          (scenario != "bycatch_headstart") |
            (reduction %in% input$reduction_fs &
               eggs %in% input$eggs_fs &
               duration %in% input$duration_fs)
        )
    }
    
    df %>%
      mutate(
        scenario_label = case_when(
          scenario == "status_quo"        ~ "Status quo",
          scenario == "bycatch"           ~ paste("Bycatch", reduction, "%"),
          scenario == "bycatch_headstart" ~ paste("Bycatch", reduction, "% HS", eggs, "eggs", duration, "yrs"),
          TRUE                            ~ scenario
        ),
        year  = as.numeric(as.character(year)),
        value = as.numeric(as.character(value))
      )
  })
  
  output$populationPlot_fs <- renderPlot({
    df <- filtered_data_pop_fs()
    req(nrow(df) > 0, input$source_fs)
    
    df <- df %>% mutate(year = as.numeric(year))
    
    ggplot(df, aes(x = year, y = value, group = scenario_label, color = scenario_label)) +
      geom_line(linewidth = 1.2) +
      labs(
        title = paste(
          "Projected",
          switch(
            input$source_fs,
            "N"     = "Population Size",
            "Psurv" = "Probability of Survival",
            "NF"    = "Number of Nesters"
          ),
          "for", input$subpop_fs
        ),
        x     = "Year",
        y     = switch(
          input$source_fs,
          "N"     = "Population Size",
          "Psurv" = "Probability of Survival",
          "NF"    = "Number of Nesters"
        ),
        color = "Scenario"
      ) +
      theme_minimal()
  })
  
  output$summaryTable_fs <- renderDT({
    df <- filtered_data_pop_fs()
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      filter(year %in% c(2030, 2050)) %>%
      select(scenario, year, value) %>%
      distinct() %>%
      pivot_wider(
        names_from  = year,
        values_from = value,
        values_fn   = list
      ) %>%
      datatable(options = list(pageLength = 5))
  })
  
  output$downloadSummary_fs <- downloadHandler(
    filename = function() {
      paste0("population_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data_pop_fs(), file, row.names = FALSE)
    }
  )
  
  # -------------------------
  # PAGE 9: Mitigation Quiz
  # -------------------------
  
  expanded_boxes_fs <- reactiveVal(character(0))
  
  output$mitigation_output_fs <- renderUI({
    req(input$gear_type_fs, input$species_group_fs)
    
    gear_mitigation <- mit %>%
      filter(
        GearType     == input$gear_type_fs,
        SpeciesGroup == input$species_group_fs
      )
    
    if (nrow(gear_mitigation) == 0) {
      return(tags$p("No mitigation data available for this selection."))
    }
    
    rows <- lapply(seq_len(nrow(gear_mitigation)), function(i) {
      tech <- gear_mitigation$MitigationTechnique[i]
      desc <- gear_mitigation$Description[i]
      cat  <- gear_mitigation$MeasureCategory[i]
      
      is_expanded <- tech %in% expanded_boxes_fs()
      onclick_js  <- sprintf(
        "Shiny.setInputValue('clicked_box_fs', %s, {priority:'event'})",
        jsonlite::toJSON(tech, auto_unbox = TRUE)
      )
      
      div(
        style = paste0(
          "border:1px solid #337ab7; padding:10px; margin:5px; ",
          "border-radius:5px; cursor:pointer; background-color:",
          ifelse(is_expanded, "#f0f8ff;", "#1a1a1a;"),
          " color:#fff;"
        ),
        onclick = onclick_js,
        strong(tech), tags$br(),
        tags$small(tags$em(cat)),
        if (is_expanded) tags$p(desc) else NULL
      )
    })
    
    tagList(rows)
  })
  
  observeEvent(input$clicked_box_fs, {
    req(input$clicked_box_fs)
    current <- expanded_boxes_fs()
    tech    <- input$clicked_box_fs
    
    if (tech %in% current) {
      expanded_boxes_fs(setdiff(current, tech))
    } else {
      expanded_boxes_fs(c(current, tech))
    }
  })
  
  # -------------------------
  # PAGE 10: Mortality Estimates
  # -------------------------
  
  output$mortality_plot <- renderPlotly({
    df <- cap_data %>%
      filter(!is.na(total_mortality) & !is.na(flag_gfw)) %>%
      arrange(desc(total_mortality)) %>%
      head(15)
    
    plot_ly(
      df,
      x    = ~reorder(flag_gfw, total_mortality),
      y    = ~total_mortality,
      type = "bar",
      marker = list(color = "coral"),
      text = ~paste0(flag_gfw, "<br>", round(total_mortality, 1), " turtles"),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = "Estimated Annual Mortality by Flag (Top 15)",
        xaxis = list(title = "Flag", tickangle = -45),
        yaxis = list(title = "Estimated Leatherback Mortality"),
        margin = list(b = 100)
      )
  })
  
  output$mortality_table <- renderDT({
    cap_data %>%
      select(flag_gfw, country, total_mortality, mean_rank, RFMO, rank_group) %>%
      arrange(desc(total_mortality)) %>%
      datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        colnames = c(
          "Flag", "Country", "Total Mortality",
          "Mean Rank", "RFMO", "Rank Group"
        )
      ) %>%
      formatRound(columns = "total_mortality", digits = 1) %>%
      formatRound(columns = "mean_rank",       digits = 2)
  })
  
  # -------------------------
  # PAGE 11: Capacity to Change
  # -------------------------
  
  output$capacity_plot <- renderPlotly({
    p <- ggplot(
      cap_data,
      aes(
        x    = total_mortality,
        y    = mean_rank,
        text = paste("Flag:", flag_gfw, "<br>Country:", country)
      )
    ) +
      geom_point(size = 3, alpha = 0.7, color = "steelblue") +
      labs(
        title = "Capacity to Change",
        x     = "Total Mortality",
        y     = "Mean Rank (Lower = Better Capacity)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$capacity_table <- renderDT({
    cap_data %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # -------------------------
  # PAGE 12: Migration Corridors
  # -------------------------
  
  output$migration_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -100, lat = 10, zoom = 3) %>%
      addMarkers(
        lng   = -85.8,
        lat   = 10.4,
        popup = "Playa Grande, Costa Rica - Major nesting site"
      )
  })
  
  # -------------------------
  # PAGE 15: Economic Impact Calculator
  # -------------------------
  
  output$total_cost_box <- renderValueBox({
    valueBox(
      value    = "Calculate",
      subtitle = "Click Calculate",
      icon     = icon("calculator"),
      color    = "yellow"
    )
  })
  
  observeEvent(input$calculate_economics, {
    total <- input$num_vessels * input$avg_interactions *
      (input$gear_damage_cost + input$lost_time_hours * 50)
    
    output$total_cost_box <- renderValueBox({
      valueBox(
        value    = paste0("$", format(total, big.mark = ",")),
        subtitle = "Total Annual Cost",
        icon     = icon("dollar-sign"),
        color    = "red"
      )
    })
    
    output$cost_breakdown <- renderPlotly({
      df <- data.frame(
        Category = c("Gear Damage", "Lost Time", "Compliance"),
        Cost     = c(total * 0.4, total * 0.5, total * 0.1)
      )
      
      plot_ly(df, labels = ~Category, values = ~Cost, type = "pie")
    })
  })
  
  # -------------------------
  # PAGE 16: Demographic Sensitivity
  # -------------------------
  
  output$sensitivity_plot <- renderPlotly({
    df <- data.frame(
      Stage       = c("Eggs/Hatchlings", "Juveniles", "Sub-adults", "Adults"),
      Sensitivity = c(0.2, 0.5, 0.7, 0.9)
    )
    
    plot_ly(
      df,
      x    = ~Stage,
      y    = ~Sensitivity,
      type = "bar",
      marker = list(color = "steelblue")
    ) %>%
      layout(
        title = "Life Stage Sensitivity to Mortality",
        yaxis = list(title = "Elasticity Value")
      )
  })
  
  # -------------------------
  # PAGE 17: Scenario Planning
  # -------------------------
  
  output$scenario_mortality <- renderValueBox({
    valueBox(
      value    = "Run Scenario",
      subtitle = "Mortality Change",
      icon     = icon("chart-line"),
      color    = "green"
    )
  })
  
  output$scenario_economic <- renderValueBox({
    valueBox(
      value    = "Run Scenario",
      subtitle = "Economic Impact",
      icon     = icon("dollar-sign"),
      color    = "yellow"
    )
  })
  
  output$scenario_comparison <- renderPlotly({
    plot_ly(type = "bar") %>%
      add_trace(
        x    = c("Current", "Scenario"),
        y    = c(100, 60),
        name = "Mortality"
      ) %>%
      layout(
        title = "Scenario Outcomes",
        yaxis = list(title = "% of Baseline")
      )
  })
  
  # -------------------------
  # PAGE 18: Policy Effectiveness
  # -------------------------
  
  output$policy_table <- renderDT({
    data.frame(
      Region           = c("Hawaii", "Costa Rica", "Indonesia"),
      Policy           = c("Longline closure", "TEDs mandatory", "Gillnet restrictions"),
      Year             = c(2004, 2008, 2015),
      Mortality_Change = c("-40%", "-60%", "-25%"),
      Status           = c("Active", "Active", "Partial compliance")
    )
  })
  
  # -------------------------
  # PAGE 19: Data Portal
  # -------------------------
  
  output$data_browser <- renderDT({
    data.frame(
      Dataset      = c("Fishing Effort 2024", "Nest Counts 2023", "Tracking Data"),
      Records      = c(15234, 892, 47),
      Last_Updated = c("2024-11-15", "2024-08-20", "2024-10-30"),
      Quality      = c("High", "Medium", "High")
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$download_dataset, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data.frame(placeholder = "data"), file)
    }
  )
  
  # -------------------------
  # PAGE 20: Consumer Tools
  # -------------------------
  
  output$ratings_table <- renderDT({
    data.frame(
      Product = c(
        "Albacore Tuna (US West Coast)",
        "Yellowfin Tuna (Longline)",
        "Farmed Shrimp",
        "Wild-caught Shrimp (US)"
      ),
      Rating = c(
        "🟢 Best Choice",
        "🟡 Good Alternative",
        "🔴 Avoid",
        "🟡 Good Alternative"
      ),
      Leatherback_Impact = c("Low", "Medium", "Low", "Medium")
    )
  })
  
  output$consumer_impact_box <- renderValueBox({
    valueBox(
      value    = "Calculate",
      subtitle = "Click Calculate",
      icon     = icon("calculator"),
      color    = "blue"
    )
  })
  
  output$consumer_alternatives <- renderPlot({
    barplot(
      c(5, 2, 1),
      names.arg = c("Current", "Eco-certified", "Best Choice"),
      col       = c("coral", "gold", "lightgreen"),
      main      = "Your Impact by Choice",
      ylab      = "Turtle Impact Score"
    )
  })
  
  # -------------------------
  # PAGE 21: Report Sightings
  # -------------------------
  
  observeEvent(input$submit_sighting, {
    showNotification(
      "Thank you for your report! Data submitted successfully.",
      type     = "message",
      duration = 5
    )
  })
  
  # Resize leaflet maps on page transitions
  observe({
    runjs("
      $('.fade-page').on('transitionend webkitTransitionEnd oTransitionEnd', function(){
        setTimeout(function(){
          if(window.HTMLWidgets){
            var maps = document.getElementsByClassName('leaflet-html-widget');
            for(var i = 0; i < maps.length; i++){
              maps[i].dispatchEvent(new Event('resize'));
            }
          }
        }, 350);
      });
    ")
  })
}

# -------------------------
# Run the app
# -------------------------

shinyApp(ui = ui, server = server)
