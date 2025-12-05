#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# ---- Libraries ----
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(bslib)
library(shinyjs)
library(plotly)    # used for radar + scatter
library(ggplot2)   # fallback visuals
library(scales)    # for percent formatting
library(rsconnect)
# (install any missing packages before running) 

# ---- Data loading ----
csv_path <- "midfielders_data.csv"
xlsx_path <- "midfielders_data.xlsx"

if (file.exists(csv_path)) {
  DummyData <- readr::read_csv(csv_path, show_col_types = FALSE)
} else if (file.exists(xlsx_path)) {
  DummyData <- readxl::read_excel(xlsx_path)
} else {
  stop("Please place 'midfielders_data.csv' (or 'midfielders_data.xlsx') in the app directory.")
}

# Clean column names: remove leading/trailing spaces
names(DummyData) <- trimws(names(DummyData))

# Optional: standardize key names
if ("Player Name" %in% names(DummyData)) names(DummyData)[names(DummyData) == "Player Name"] <- "Player"
if ("MidfielderType" %in% names(DummyData)) names(DummyData)[names(DummyData) == "MidfielderType"] <- "Midfielder Type"

# Verify essential columns exist
required_cols <- c("Player","Team","Age","90s","OverallComposite")
missing_cols <- setdiff(required_cols, names(DummyData))
if (length(missing_cols) > 0) stop("Missing essential columns: ", paste(missing_cols, collapse=", "))


# Convert empty strings to NA for safety
DummyData <- DummyData %>% mutate(across(everything(), ~ ifelse(. == "", NA, .)))

# ---- Team colors (Premier League) ----
team_colors <- c(
  "Arsenal"="#EF0107","Aston Villa"="#95BFE5","Bournemouth"="#DA291C","Brentford"="#E30613",
  "Brighton"="#0057B8","Burnley"="#6C1D45","Chelsea"="#034694","Crystal Palace"="#1B458F",
  "Everton"="#003399","Fulham"="#CC0000","Liverpool"="#C8102E","Luton Town"="#FF5A36",
  "Manchester City"="#6CABDD","Manchester United"="#DA291C","Newcastle United"="#241F20",
  "Nottingham Forest"="#E0282F","Sheffield United"="#EE2737","Tottenham Hotspur"="#FFFFFF",
  "West Ham"="#7A263A","Wolverhampton Wanderers"="#FDB913","Leicester City"="#95BFE5","Southampton"="#DA291C"
)
league_teams <- list("Premier League" = names(team_colors))

# ---- Stat maps & weights (use your provided mapping) ----
stat_map <- list(
  Shooting = c("npxG","Share of npxG","Non Penalty Goals","Non Penalty Goals/TeamXG","Shots"),
  Passing  = c("Passes Completed","Share of Passes Completed","Pass Completion %","Average Pass Length","Assists","xAG","Progressive Passes/Passes Completed"),
  Dribbling = c("Successful Take-Ons","Share of Sucessful Take-Ons","Successful Take-On Percentage","Progressive Carries"),
  Defending = c("Tackles Won","Share of Tackles Won","Tackle Completion %","Blocks","Aerial Duels Won","Aerial Duel Win %"),
  Positioning = c("Interceptions","Progressive Passes Received","Touches","Share of Touches")
)

stat_weights <- list(
  Shooting = c("npxG"=25,"Share of npxG"=25,"Non Penalty Goals"=10,"Non Penalty Goals/TeamXG"=10,"Shots"=30),
  Passing  = c("Passes Completed"=10,"Share of Passes Completed"=10,"Pass Completion %"=20,"Average Pass Length"=20,"Assists"=5,"xAG"=15,"Progressive Passes/Passes Completed"=20),
  Dribbling = c("Successful Take-Ons"=15,"Share of Sucessful Take-Ons"=15,"Successful Take-On Percentage"=30,"Progressive Carries"=40),
  Defending = c("Tackles Won"=15,"Share of Tackles Won"=10,"Tackle Completion %"=35,"Blocks"=15,"Aerial Duels Won"=15,"Aerial Duel Win %"=25),
  Positioning = c("Interceptions"=30,"Blocks"=20,"Progressive Passes Received"=20,"Touches"=15,"Share of Touches"=15),
  `90s` = c("90s"=100)
)

# ---- Ensure composites exist, warn if not ----
required_composites <- c("ShootingComposite","PassingComposite","DribblingComposite","DefendingComposite","PositioningComposite","OverallComposite")
missing_comps <- setdiff(required_composites, names(DummyData))
if (length(missing_comps) > 0) {
  warning("Missing composite columns: ", paste(missing_comps, collapse = ", "), ". Some displays may show NA.")
}

# ---- Derive Midfielder Type if missing ----
if (!("Midfielder Type" %in% names(DummyData))) {
  DummyData <- DummyData %>%
    mutate(across(all_of(required_composites), ~ coalesce(.x, 0))) %>%
    mutate(
      OffensiveMetric = 0.10*ShootingComposite + 0.20*PassingComposite + 0.15*DribblingComposite,
      `Midfielder Type` = case_when(
        OffensiveMetric > DefendingComposite + 25 ~ "Offensive",
        DefendingComposite > OffensiveMetric + 25 ~ "Defensive",
        TRUE ~ "Balanced"
      )
    ) %>% select(-OffensiveMetric)
}

# ---- Estimate Transfer Value: final agreed formula + elite ability bonus ----
if (!"EstValue" %in% names(DummyData)) {
  DummyData <- DummyData %>%
    mutate(
      OverallComposite = coalesce(OverallComposite, 0),
      `90s` = coalesce(`90s`, 0),
      Age = coalesce(Age, 25)
    )
  
  mean_age <- mean(DummyData$Age, na.rm = TRUE)
  mean_90s <- mean(DummyData$`90s`, na.rm = TRUE)
  
  # Composites to check (excluding OverallComposite)
  comps_to_check <- c("ShootingComposite", "PassingComposite", "DribblingComposite", 
                      "DefendingComposite", "PositioningComposite")
  
  # Compute 90th percentiles
  perc90_list <- sapply(comps_to_check, function(col) {
    if (col %in% names(DummyData)) quantile(DummyData[[col]], 0.9, na.rm = TRUE) else NA
  })
  
  DummyData <- DummyData %>%
    rowwise() %>%
    mutate(
      # Base value formula
      EstValue_raw = ((OverallComposite/100)^1.4) *
        (ifelse(mean_90s > 0, (`90s` / mean_90s), 0)) *
        ((mean_age / Age)^2.5) * 65,
      
      # Age adjustment: bonus for youth (<23), slight discount for older (>30)
      EstValue_raw = EstValue_raw * ifelse(is.na(Age), 1,
                                           ifelse(Age < 23, 1.10,
                                                  ifelse(Age > 30, 0.90, 1))),
      
      # Identify elite composites (≥90th percentile)
      elite_flags = list(mapply(function(value, p90) {
        if (is.na(value) || is.na(p90)) return(FALSE)
        value >= p90
      }, c_across(all_of(comps_to_check)), perc90_list)),
      
      # Count and list elite abilities
      n_elite = sum(unlist(elite_flags)),
      EliteAbilities = paste(comps_to_check[unlist(elite_flags)], collapse = ", "),
      
      # Apply +5% per elite composite
      EstValue_raw = EstValue_raw * (1 + 0.05 * n_elite),
      
      # Final clamped value
      EstValue = pmin(pmax(EstValue_raw, 2), 150)
    ) %>%
    ungroup() %>%
    select(-EstValue_raw, -elite_flags, -n_elite)
}


# ---- Helper: safe numeric coercion ----
safe_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

# ---- Prepare team names present in data (restrict league teams to those present) ----
teams_present <- unique(na.omit(DummyData$Team))
# if a team doesn't have a color, leave it grey
missing_teams_color <- setdiff(teams_present, names(team_colors))
if (length(missing_teams_color) > 0) {
  # assign neutral grey to missing teams
  newcols <- rep("#9AA0A6", length(missing_teams_color))
  names(newcols) <- missing_teams_color
  team_colors <- c(team_colors, newcols)
}

# ---- UI: Long, commented, split into sections ----
ui <- fluidPage(
  useShinyjs(),
  # Add CSS for exit buttons
  tags$head(
    tags$style(HTML("
      .exit-button { 
        position: absolute; 
        top: 12px; 
        right: 12px; 
        font-size: 24px; 
        cursor: pointer; 
        color: #000; /* Change to white if your background is dark */
        z-index: 1000; 
      }
    "))
  ),
  
  theme = bs_theme(version = 4, bootswatch = "darkly"),
  tags$head(tags$style(HTML("
    /* hide tab headers */
    .nav.nav-tabs { display: none; }
    /* basic layout */
    .left-panel { padding: 12px; }
    .right-panel { padding: 12px; background: rgba(255,255,255,0.03); border-radius:8px; }
    .player-image { width:100%; border-radius: 8px; }
    .composite-summary { font-weight:700; cursor:pointer; margin:8px 0; color:#e6eef6; }
    .stat-table th, .stat-table td { padding:6px 8px; border:1px solid #ddd; background:white; color:black; font-size:13px; }
    .info-box { position: fixed; top: 12px; right: 12px; width:360px; z-index:9999; background: rgba(255,255,255,0.96); color:#000; padding:12px; border-radius:8px; box-shadow: 0 8px 24px rgba(0,0,0,0.45); }
    .close-info { float:right; cursor:pointer; background:none; border:none; font-size:18px; }
    .team-link { text-decoration:underline; cursor:pointer; }
    /* ensure DT small padding */
    table.dataTable td, table.dataTable th { padding: 6px !important; }
  "))),
  # Dismissable info box
  
  div(id="info_box", class="info-box",
      actionButton("close_info_box", "×", class = "close-info"),
      HTML("<strong>How to use:</strong><br/>
         - This project uses 2024/2025 Premier League stats from midfielders<br/>
         - Use the menus on the right to filter players; press <b>Filter</b> to run.<br/>
         - Leaderboards (left) are global and not affected by the filter.<br/>
         - Click player rows or names to open the player profile.<br/>
         - A scatterplot below the leaderboards has an x and y axis that can be adjusted to different stats and can be used to open player profiles")
  ),
  
  tabsetPanel(id = "main_tabs",
              # ---------------- Page 1: Leaders + Filter ----------------
              tabPanel("Page 1",
                       fluidRow(
                         column(12, align="center",
                                div(style="max-width:900px; margin:0 auto;",
                                    selectInput("search", label = NULL, choices = c("", sort(unique(DummyData$Player))), width = "100%")
                                )
                         )
                       ),
                       br(),
                       fluidRow(
                         column(6, class = "left-panel",
                                h3("Leaderboards"),
                                div(style = "display:flex; gap:6px; margin-bottom:6px;",
                                    actionButton("go_overall","Overall"),
                                    actionButton("go_shoot","Shooting"),
                                    actionButton("go_drib","Dribbling"),
                                    actionButton("go_pass","Passing"),
                                    actionButton("go_def","Defending"),
                                    actionButton("go_pos","Positioning"),
                                    actionButton("go_transfer_value", "Transfer Value")
                                    
                                ),
                                # Only the overall leaders visible here; other pages show full tables
                                DTOutput("leaders_overall"),
                                tags$hr(),
                                h4("Scatterplot (below)"),
                                fluidRow(
                                  column(5, 
                                         selectInput("scatter_x", "X axis", 
                                                     choices = names(DummyData)[sapply(DummyData, is.numeric)], 
                                                     selected = "OverallComposite")
                                  ),
                                  column(5, 
                                         selectInput("scatter_y", "Y axis", 
                                                     choices = names(DummyData)[sapply(DummyData, is.numeric)], 
                                                     selected = "EstValue")
                                  ),
                                  column(2, 
                                         # Stack both checkboxes in the same column
                                         checkboxInput("scatter_names", "Names", value = FALSE),
                                         br(),
                                         checkboxInput("scatter_lm", "Show line of best fit")
                                  )
                                ),
                                
                                br(),
                                plotlyOutput("scatter_plot", height = "420px")
                         ),
                         
                         # filter panel on right
                         column(6, class = "right-panel",
                                h3("Player Filter"),
                                p("Sliders set composite value ranges (0–100). Press Filter to apply. Leaderboards are unaffected."),
                                selectInput("slider_target", "Composite Slider", choices = c("Overall","Shooting","Dribbling","Passing","Defending","Positioning")),
                                uiOutput("stat_slider_ui"),
                                tags$hr(),
                                h4("League"),
                                checkboxGroupInput("selected_leagues", label = NULL, choices = names(league_teams), selected = names(league_teams)),
                                tags$hr(),
                                h4("Teams"),
                                
                                # Select All checkbox – appears directly under "Teams"
                                checkboxInput(
                                  "select_all_teams", 
                                  "Select All", 
                                  value = TRUE, 
                                  width = "100%"
                                ),
                                
                                # Add spacing before the team checkboxes
                                tags$div(style = "margin-top: 10px;"),
                                
                                # All team checkboxes stacked vertically below Select All
                                div(
                                  style = "display: block; margin-left: 10px;",
                                  uiOutput("team_checkboxes")
                                ),
                              
                                tags$hr(),
                                h4("Age range"),
                                selectInput("age_filter", NULL, choices = c("All","Under 21","21-25","26-30","31+"), selected = "All"),
                                h4("Midfielder Type"),
                                selectInput("type_filter", NULL, choices = c("All","Offensive","Balanced","Defensive"), selected = "All"),
                                # Reset button at top of filter section
                                actionButton("reset_filter", "Reset Filters", class = "btn btn-warning", style = "margin-bottom:10px;"),
                                tags$hr(),
                                br(),
                                actionButton("run_filter", "Filter", class = "btn btn-primary", style = "width:100%;")
                              
                         )
                       )
              ),
              
              # ---------------- Leaderboard pages ----------------
              tabPanel("Page 2", 
                       actionLink("close_2", "×", style="float:right; font-size:40px; color:#fff; margin:6px;"),
                       h2("Overall Leaders"), 
                       DTOutput("leaders_overall_full")
              ),
              
              tabPanel("Page 3", 
                       div(class="exit-button", 
                           actionLink("close_3","×", 
                                      style="float:right; font-size:40px; color:#fff; margin:6px;")
                       ),
                       h2("Shooting Leaders"), 
                       DTOutput("leaders_shooting")
              ),
              
              tabPanel("Page 4", 
                       div(class="exit-button", 
                           actionLink("close_4","×", 
                                      style="float:right; font-size:40px; color:#fff; margin:6px;")
                       ),
                       h2("Dribbling Leaders"), 
                       DTOutput("leaders_dribbling")
              ),
              
              tabPanel("Page 5", 
                       div(class="exit-button", 
                           actionLink("close_5","×", 
                                      style="float:right; font-size:40px; color:#fff; margin:6px;")
                       ),
                       h2("Passing Leaders"), 
                       DTOutput("leaders_passing")
              ),
              
              tabPanel("Page 6", 
                       div(class="exit-button", 
                           actionLink("close_6","×", 
                                      style="float:right; font-size:40px; color:#fff; margin:6px;")
                       ),
                       h2("Defending Leaders"), 
                       DTOutput("leaders_defending")
              ),
              
              tabPanel("Page 7", 
                       div(class="exit-button", 
                           actionLink("close_7","×", 
                                      style="float:right; font-size:40px; color:#fff; margin:6px;")
                       ),
                       h2("Positioning Leaders"), 
                       DTOutput("leaders_positioning")
              ),
              tabPanel("Transfer Value",
                       actionLink("close_transfer", "×", 
                                  style="float:right; font-size:40px; color:#fff; margin:6px;"),
                       h2("Estimated Transfer Values"),
                       DTOutput("leaders_transfer_value")
              ),
              
              
              
              # ---------------- Player profile ----------------
              tabPanel("Page 8",
                       actionLink("close_profile", "×", style="float:right; font-size:40px; color:#fff; margin:6px;"),
                       br(),
                       fluidRow(
                         column(3, uiOutput("player_image_ui")),
                         column(6, uiOutput("player_header_ui")),
                         column(3, uiOutput("player_similar_ui"))
                       ),
                       tags$hr(),
                       fluidRow(column(12, div(
                         style = "width:50%; float:left;",
                         plotlyOutput("player_radar", height = "400px")
                       )
                       )),
                       tags$hr(),
                       fluidRow(column(12, uiOutput("player_composites_ui")))
              ),
              
              
              # ---------------- Filter results ----------------
              tabPanel("Page 9",
                       actionLink("close_profile", "×", style="float:right; font-size:40px; color:#fff; margin:6px;"),
                       h2("Filtered Players"),
                       DTOutput("filter_results"),
                       fluidRow(
                         column(5,
                                selectInput("filter_scatter_x", "X-Axis:", choices = names(DummyData), selected = "PassingComposite")
                         ),
                         column(5,
                                selectInput("filter_scatter_y", "Y-Axis:", choices = names(DummyData), selected = "DefendingComposite")
                         ),
                         column(2,
                                checkboxInput("filter_scatter_names", "Names", value = FALSE),
                                br(),
                                checkboxInput("filter_scatter_lm", "Show line of best fit")
                         )
                       ),
                       plotlyOutput("filter_scatter", height = "450px")
              ),
              
              
              # ---------------- Team page ----------------
              tabPanel("Page 10",
                       uiOutput("team_header"),
                       tags$hr(),
                       DTOutput("team_stats"),
                       tags$hr(),
                       DTOutput("team_players"),
                       plotOutput("team_players_bar", height = "330px")
                       
              )
  ) # tabsetPanel
) # fluidPage

# ---- SERVER ----
server <- function(input, output, session) {
  useShinyjs()
  
  # Filter Page "X" button
  observeEvent(input$close_9, {
    updateTabsetPanel(session, "main_tabs", "Page 1") # Go back to main page
  })
  
  # ---- Info dismiss ----
  observeEvent(input$close_info_box, { hide("info_box") })
  
  # ---- Slider UI + reactive store ----
  slider_values <- reactiveValues()
  default_ranges <- list(Overall = c(0,100), Shooting = c(0,100), Dribbling = c(0,100),
                         Passing = c(0,100), Defending = c(0,100), Positioning = c(0,100))
  
  output$stat_slider_ui <- renderUI({
    tgt <- input$slider_target %||% "Overall"
    sliderInput("composite_range", paste0(tgt, " composite range"), min = 0, max = 100,
                value = slider_values[[tgt]] %||% default_ranges[[tgt]], step = 1)
  })
  
  observeEvent(input$composite_range, {
    tgt <- input$slider_target %||% "Overall"
    slider_values[[tgt]] <- input$composite_range
  }, ignoreNULL = FALSE)
  
  # ---- Team checkboxes UI and Select All logic ----
output$team_checkboxes <- renderUI({
  req(input$selected_leagues)
  
  # get all teams in selected leagues
  teams <- unique(unlist(league_teams[input$selected_leagues]))
  
  # show all teams if select_all_teams is TRUE
  checkboxGroupInput(
    "selected_teams",
    label = NULL,
    choices = teams,
    selected = if (isTRUE(input$select_all_teams)) teams else character(0)
  )
})

# ---- Handle select all toggle ----
observeEvent(input$select_all_teams, {
  req(input$selected_leagues)
  teams <- unique(unlist(league_teams[input$selected_leagues]))
  updateCheckboxGroupInput(session, "selected_teams",
                           selected = if (isTRUE(input$select_all_teams)) teams else character(0))
})

  observeEvent(input$reset_filter, {
    
    # ---- Composite slider ----
    updateSelectInput(session, "slider_target", selected = "Overall")
    slider_values[["Overall"]]   <- c(0, 100)
    slider_values[["Passing"]]   <- c(0, 100)
    slider_values[["Defending"]] <- c(0, 100)
    
    output$stat_slider_ui <- renderUI({
      sliderInput("composite_range", "Overall composite range",
                  min = 0, max = 100, value = c(0, 100))
    })
    
    # ---- League checkboxes ----
    updateCheckboxGroupInput(session, "selected_leagues",
                             selected = names(league_teams))
    
    # ---- Teams ----
    teams_all <- unname(unlist(league_teams))   # <-- FIXED HERE
    
    updateCheckboxInput(session, "select_all_teams", value = TRUE)
    
    output$team_checkboxes <- renderUI({
      checkboxGroupInput(
        "selected_teams",
        label = NULL,
        choices = teams_all,
        selected = teams_all
      )
    })
    
    # ---- Age ----
    updateSelectInput(session, "age_filter", selected = "All")
    
    # ---- Midfielder type ----
    updateSelectInput(session, "type_filter", selected = "All")
    
    # ---- Tab navigation (replace tabset ID below) ----
    updateTabsetPanel(session, "REPLACE_THIS_ID", selected = "Filter")
    
  })
  
  observeEvent(input$run_filter, {
    updateTabsetPanel(session, "main_tabs", selected = "Page 9")
  })

  
  # ---- DataTable coloring helper ----
  # ---- safe color helper: checks for Team existence before styling ----
  color_dt_by_team <- function(dt, df, cols = NULL, team_col = "Team") {
    # dt : a DT::datatable object (already created)
    # df : the original data.frame used to create dt (so we can read Team values)
    # cols: character vector of column names in dt to style (NULL => style all visible columns)
    if (is.null(cols)) cols <- names(df)
    
    # If the Team column exists in the data, apply background coloring by team values.
    # If not, just return dt unchanged (avoids "You specified the columns: Team" errors).
    if (team_col %in% names(df)) {
      unique_teams <- unique(na.omit(df[[team_col]]))
      # map keys -> colors, falling back to neutral grey if team not in team_colors
      keys <- unique_teams
      values <- sapply(keys, function(k) if (!is.null(team_colors[[k]])) team_colors[[k]] else "#9AA0A6", USE.NAMES = FALSE)
      # Apply background color mapping across requested columns (valueColumns defines which column to read values from)
      dt <- dt %>%
        formatStyle(columns = cols,
                    backgroundColor = styleEqual(keys, values),
                    valueColumns = team_col)
      # Force Tottenham Hotspur text to be black in all visible columns (if present)
      dt <- dt %>% formatStyle(columns = cols, color = styleEqual("Tottenham Hotspur", "black"), valueColumns = team_col)
      return(dt)
    } else {
      # Team not present in this table: don't attempt any team-based styling
      return(dt)
    }
  }
  
  
  # ---- Leaderboard prep & outputs ----
  leader_prepare <- function(df, comp_col, n = 30) {
    if (!comp_col %in% names(df)) {
      return(data.frame(Note = paste("Missing", comp_col)))
    }
    out <- df %>% arrange(desc(.data[[comp_col]])) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Player, Team, all_of(comp_col)) %>% head(n)
    out
  }
  
  # small wrapper to build DT with team coloring
  build_leader_dt <- function(df) {
    dt <- datatable(df, selection = "single", rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE))
    dt <- color_dt_by_team(dt, df, cols = names(df))
    dt
  }
  
  # outputs for leaderboards (left)
  output$leaders_overall <- renderDT({
    out <- leader_prepare(DummyData, "OverallComposite", n = 10)
    build_leader_dt(out)
  })
  output$leaders_overall_full <- renderDT({
    out <- leader_prepare(DummyData, "OverallComposite", n = 30)
    dt <- build_leader_dt(out)
    # ensure Tottenham text black in full table
    dt
  })
  output$leaders_shooting <- renderDT({
    out <- leader_prepare(DummyData, "ShootingComposite", n = 30)
    build_leader_dt(out)
  })
  output$leaders_dribbling <- renderDT({
    out <- leader_prepare(DummyData, "DribblingComposite", n = 30)
    build_leader_dt(out)
  })
  output$leaders_passing <- renderDT({
    out <- leader_prepare(DummyData, "PassingComposite", n = 30)
    build_leader_dt(out)
  })
  output$leaders_defending <- renderDT({
    out <- leader_prepare(DummyData, "DefendingComposite", n = 30)
    build_leader_dt(out)
  })
  output$leaders_positioning <- renderDT({
    out <- leader_prepare(DummyData, "PositioningComposite", n = 30)
    build_leader_dt(out)
  })
  output$leaders_transfer_value <- renderDT({
    
    # Get data sorted by EstValue using your helper
    out <- leader_prepare(DummyData, "EstValue", n = 30)
    
    # If EstValue missing, show the message
    if ("Note" %in% names(out)) {
      return(datatable(out, options = list(dom = 't')))
    }
    
    # Format EstValue into "£xx.xM" or "N/A"
    out$EstValue <- sapply(out$EstValue, function(v) {
      if (is.na(v)) {
        "N/A"
      } else {
        paste0("£", round(v, 2), "M")
      }
    })
    
    # Rename column for display
    names(out)[names(out) == "EstValue"] <- "Estimated Transfer Value"
    
    # Build the styled table using your existing helper
    build_leader_dt(out)
  })
  
  
  # When clicking rows on the each leaderboard, open player profile
  observeEvent(input$leaders_overall_rows_selected, {
    sel <- input$leaders_overall_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      player <- leader_prepare(DummyData, "OverallComposite") %>% slice(sel) %>% pull(Player)
      updateSelectInput(session, "search", selected = player)
      updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  
  observeEvent(input$leaders_shooting_rows_selected, {
    sel <- input$leaders_shooting_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      player <- leader_prepare(DummyData, "ShootingComposite") %>% slice(sel) %>% pull(Player)
      updateSelectInput(session, "search", selected = player); updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  
  observeEvent(input$leaders_overall_full_rows_selected, {
    sel <- input$leaders_overall_full_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      player <- leader_prepare(DummyData, "OverallComposite") %>% 
        slice(sel) %>% 
        pull(Player)
      
      updateSelectInput(session, "search", selected = player)
      updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  
  observeEvent(input$leaders_dribbling_rows_selected, {
    sel <- input$leaders_dribbling_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      player <- leader_prepare(DummyData, "DribblingComposite") %>% slice(sel) %>% pull(Player)
      updateSelectInput(session, "search", selected = player); updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  observeEvent(input$leaders_passing_rows_selected, {
    sel <- input$leaders_passing_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      player <- leader_prepare(DummyData, "PassingComposite") %>% slice(sel) %>% pull(Player)
      updateSelectInput(session, "search", selected = player); updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  observeEvent(input$leaders_defending_rows_selected, {
    sel <- input$leaders_defending_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      player <- leader_prepare(DummyData, "DefendingComposite") %>% slice(sel) %>% pull(Player)
      updateSelectInput(session, "search", selected = player); updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  observeEvent(input$leaders_positioning_rows_selected, {
    sel <- input$leaders_positioning_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      player <- leader_prepare(DummyData, "PositioningComposite") %>% slice(sel) %>% pull(Player)
      updateSelectInput(session, "search", selected = player); updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  observeEvent(input$leaders_transfer_value_rows_selected, {
    sel <- input$leaders_transfer_value_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      
      player <- leader_prepare(DummyData, "EstValue") %>%
        slice(sel) %>%
        pull(Player)
      
      updateSelectInput(session, "search", selected = player)
      updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  
  
  
  # navigation button actions
  observeEvent(input$go_overall, { updateTabsetPanel(session, "main_tabs", "Page 2") })
  observeEvent(input$go_shoot, { updateTabsetPanel(session, "main_tabs", "Page 3") })
  observeEvent(input$go_drib, { updateTabsetPanel(session, "main_tabs", "Page 4") })
  observeEvent(input$go_pass, { updateTabsetPanel(session, "main_tabs", "Page 5") })
  observeEvent(input$go_def, { updateTabsetPanel(session, "main_tabs", "Page 6") })
  observeEvent(input$go_pos, { updateTabsetPanel(session, "main_tabs", "Page 7") })
  observeEvent(input$go_transfer_value, {updateTabsetPanel(session, "main_tabs", "Transfer Value")})
  
  
  # Close links for each page to go back
  observeEvent(input$close_2, { updateTabsetPanel(session, "main_tabs", "Page 1") })
  observeEvent(input$close_3, { updateTabsetPanel(session, "main_tabs", "Page 1") })
  observeEvent(input$close_4, { updateTabsetPanel(session, "main_tabs", "Page 1") })
  observeEvent(input$close_5, { updateTabsetPanel(session, "main_tabs", "Page 1") })
  observeEvent(input$close_6, { updateTabsetPanel(session, "main_tabs", "Page 1") })
  observeEvent(input$close_7, { updateTabsetPanel(session, "main_tabs", "Page 1") })
  observeEvent(input$close_transfer, { updateTabsetPanel(session, "main_tabs", "Page 1") })
  
  observeEvent(input$close_profile, {
    updateTabsetPanel(session, "main_tabs", "Page 1")
  })
  observeEvent(input$close_team, {
    updateTabsetPanel(session, "main_tabs", "Page 1")
  })
  
  
  # ---- Scatter plot (plotly) ----
  output$scatter_plot <- renderPlotly({
    req(input$scatter_x, input$scatter_y)
    
    xcol <- input$scatter_x
    ycol <- input$scatter_y
    
    if (!(xcol %in% names(DummyData)) || !(ycol %in% names(DummyData))) 
      return(plotly_empty())
    
    plot_df <- DummyData %>%
      mutate(
        xval = safe_numeric(.data[[xcol]]),
        yval = safe_numeric(.data[[ycol]])
      )
    
    midfield_types <- unique(plot_df$`Midfielder Type`)
    default_colors <- c("#E07A5F","#2A9D8F","#5E6472","#9AA0A6","#F4D03F","#8E44AD")
    type_colors <- setNames(default_colors[seq_along(midfield_types)], midfield_types)
    
    p <- plot_ly(
      plot_df,
      x = ~xval, y = ~yval,
      type = 'scatter',
      mode = 'markers',
      color = ~`Midfielder Type`,
      colors = type_colors,
      text = ~paste0("<b>", Player, "</b><br>", Team, "<br>Age: ", Age),
      hoverinfo = "text",
      marker = list(size = 10),
      source = "scatter"
    ) %>%
      layout(
        xaxis = list(title = xcol),
        yaxis = list(title = ycol),
        paper_bgcolor = "#0b1b2a",
        plot_bgcolor = "#071427",
        showlegend = TRUE
      )
    
    # Player names
    if (isTRUE(input$scatter_names)) {
      p <- add_text(p, text = ~Player, textposition = "top center", showlegend = FALSE)
    }
    
    # Line of best fit
    if (isTRUE(input$scatter_lm)) {
      fit <- lm(yval ~ xval, data = plot_df)
      xseq <- seq(min(plot_df$xval, na.rm = TRUE),
                  max(plot_df$xval, na.rm = TRUE),
                  length.out = 200)
      ypred <- predict(fit, newdata = data.frame(xval = xseq))
      
      p <- add_lines(
        p,
        x = xseq,
        y = ypred,
        line = list(width = 2),
        inherit = FALSE,
        showlegend = FALSE
      )
    }
    
    p
  })
  
  
  # ---- Observe clicks on scatter plot ----
  observeEvent(event_data("plotly_click", source = "scatter"), {
    click_data <- event_data("plotly_click", source = "scatter")
    if (!is.null(click_data)) {
      xcol <- input$scatter_x
      ycol <- input$scatter_y
      
      clicked_player <- DummyData %>%
        mutate(
          xval = safe_numeric(.data[[xcol]]),
          yval = safe_numeric(.data[[ycol]])
        ) %>%
        slice(which.min((xval - click_data$x)^2 + (yval - click_data$y)^2)) %>%
        pull(Player)
      
      updateTextInput(session, "search", value = clicked_player)
    }
  })
  
  # ======================================================
  # FILTERED DATASET (runs automatically when filters change)
  # ======================================================
  filtered_dataset <- reactive({
    
    df <- DummyData
    
    # ---- League filter ----
    if (!is.null(input$selected_leagues) && length(input$selected_leagues) > 0) {
      allowed_teams <- unique(unlist(league_teams[input$selected_leagues]))
      df <- df[df$Team %in% allowed_teams, ]
    }
    
    # ---- Team filter ----
    if (!is.null(input$selected_teams) && length(input$selected_teams) > 0) {
      df <- df[df$Team %in% input$selected_teams, ]
    }
    
    # ---- Age filter ----
    if (!is.null(input$age_filter) && input$age_filter != "All") {
      df <- df[
        (input$age_filter == "Under 21" & df$Age < 21) |
          (input$age_filter == "21-25" & df$Age >= 21 & df$Age <= 25) |
          (input$age_filter == "26-30" & df$Age >= 26 & df$Age <= 30) |
          (input$age_filter == "31+" & df$Age >= 31),
      ]
    }
    
    # ---- Midfielder type ----
    if (!is.null(input$type_filter) && input$type_filter != "All") {
      df <- df[grepl(input$type_filter, df$`Midfielder Type`, ignore.case = TRUE), ]
    }
    
    # ---- Composite ranges ----
    for (nm in names(default_ranges)) {
      rng <- slider_values[[nm]] %||% default_ranges[[nm]]
      col <- paste0(nm, "Composite")
      if (col %in% names(df)) {
        df <- df[df[[col]] >= rng[1] & df[[col]] <= rng[2], ]
      }
    }
    
    # ---- Sort ----
    if ("OverallComposite" %in% names(df)) {
      df <- df[order(-df$OverallComposite), ]
    }
    
    df
  })
  
  
  # ---- Filter results DT ----
  output$filter_results <- renderDT({
    df <- filtered_dataset()
    if (nrow(df) == 0) {
      datatable(data.frame(Message = "No players match the filter"), options = list(dom = 't'))
    } else {
      out <- df %>% mutate(EstValueFormatted = paste0("£", round(as.numeric(EstValue), 1), "M")) %>%
        select(Player, Team, Age, `90s`, OverallComposite, EstValueFormatted)
      names(out)[names(out) == "EstValueFormatted"] <- "EstValue"
      dt <- datatable(out, rownames = FALSE, selection = "single", options = list(pageLength = 30, lengthChange = FALSE))
      dt <- color_dt_by_team(dt, df = df, cols = names(out))
    }
  })
  
  
  output$filter_scatter <- renderPlotly({
    df <- filtered_dataset()
    req(nrow(df) > 0)
    
    xcol <- input$filter_scatter_x
    ycol <- input$filter_scatter_y
    
    # safety check
    if (!(xcol %in% names(df)) || !(ycol %in% names(df))) 
      return(plotly_empty())
    
    # numeric conversion
    df <- df %>%
      mutate(
        xval = safe_numeric(.data[[xcol]]),
        yval = safe_numeric(.data[[ycol]])
      )
    
    # consistent midfielder-type colors
    types <- unique(df$`Midfielder Type`)
    default_cols <- c("#E07A5F","#2A9D8F","#5E6472","#9AA0A6","#F4D03F","#8E44AD")
    type_colors <- setNames(default_cols[seq_along(types)], types)
    
    # scatter plot
    p <- plot_ly(
      df,
      x = ~xval, y = ~yval,
      type = 'scatter', mode = 'markers',
      color = ~`Midfielder Type`,
      colors = type_colors,
      text = ~paste0("<b>", Player, "</b><br>", Team, "<br>Age: ", Age),
      hoverinfo = "text",
      marker = list(size = 10),
      source = "filter_scatter"
    ) %>%
      layout(
        xaxis = list(title = xcol),
        yaxis = list(title = ycol),
        paper_bgcolor = "#0b1b2a",
        plot_bgcolor = "#071427",
        showlegend = TRUE
      )
    
    # Add player names if checked
    if (isTRUE(input$filter_scatter_names)) {
      p <- add_text(p, text = ~Player, textposition = "top center", showlegend = FALSE)
    }
    
    # Add line of best fit if checked
    if (isTRUE(input$filter_scatter_lm)) {
      p <- add_trace(
        p,
        x = df$xval,
        y = fitted(lm(yval ~ xval, data = df)),
        type = 'scatter',
        mode = 'lines',
        inherit = FALSE,
        showlegend = FALSE
      )
    }
    
    p
  })
  
  
  observeEvent(event_data("plotly_click", source = "filter_scatter"), {
    click <- event_data("plotly_click", source = "filter_scatter")
    req(click)
    
    df <- filtered_dataset()
    
    xcol <- input$filter_scatter_x
    ycol <- input$filter_scatter_y
    
    df <- df %>%
      mutate(
        xval = safe_numeric(.data[[xcol]]),
        yval = safe_numeric(.data[[ycol]])
      )
    
    # nearest point selection
    clicked_player <- df %>%
      slice(which.min((xval - click$x)^2 + (yval - click$y)^2)) %>%
      pull(Player)
    
    updateTextInput(session, "search", value = clicked_player)
    updateTabsetPanel(session, "main_tabs", "Page 8")
  })
  
  
  observeEvent(input$filter_results_rows_selected, {
    sel <- input$filter_results_rows_selected
    if (!is.null(sel) && length(sel) > 0) {
      df <- filtered_dataset()
      player <- df %>% slice(sel) %>% pull(Player)
      updateSelectInput(session, "search", selected = player)
      updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  
  # ---- Player selection -> profile page ----
  observeEvent(input$search, {
    if (!is.null(input$search) && input$search != "") {
      updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  }, ignoreNULL = TRUE)
  
  # ---- Player image UI (top-left) ----
  output$player_image_ui <- renderUI({
    req(input$search)
    p <- DummyData %>% filter(Player == input$search) %>% slice(1)
    if (nrow(p) == 0) return(NULL)
    url <- p[["Player Image Address"]][1]
    # if local file path relative to www/ is supplied, show it; else if full URL show it
    if (!is.na(url) && nzchar(url)) {
      tags$img(src = url, class = "player-image")
    } else {
      tags$div(style = "background:#222; color:#ddd; padding:12px; border-radius:8px; height:180px; display:flex; align-items:center; justify-content:center;", "No image")
    }
  })
  
  
  
  # ---- Player header UI ----
  output$player_header_ui <- renderUI({
    req(input$search)
    p <- DummyData %>% filter(Player == input$search) %>% slice(1)
    if (nrow(p) == 0) return(NULL)
    
    team <- p$Team[1]
    team_col <- team_colors[team] %||% "#9AA0A6"
    
    est <- p$EstValue[1]
    est_display <- ifelse(is.na(est), "N/A", paste0("£", round(est, 1), "M"))
    
    # color code estimated value
    est_col <- ifelse(
      is.na(est) || est < 20, "#E74C3C",
      ifelse(est < 45, "#F4D03F", "#2ECC71")
    )
    
    tagList(
      tags$h2(p$Player[1]),
      tags$h4(tags$span("Age: "), p$Age[1]),
      tags$h4(
        tags$span("Team: "),
        tags$span(
          team,
          class = "team-link",
          style = paste0("color:", team_col, ";"),
          onclick = sprintf(
            "Shiny.setInputValue('team_click','%s', {priority: 'event'});",
            gsub("'", "\\'", team)
          )
        )
      ),
      tags$h4(tags$span("Midfielder Type: "), p$`Midfielder Type`[1]),
      
      # --- Elite Abilities section ---
      if (!is.na(p$EliteAbilities[1]) && nzchar(p$EliteAbilities[1])) {
        tags$h4(
          tags$span("Elite Abilities: "),
          gsub("Composite", "", p$EliteAbilities[1])
        )
      } else {
        tags$h4(tags$span("Elite Abilities: None"))
      },
      
      # --- Estimated Transfer Value ---
      tags$div(
        style = "margin-top:6px;",
        tags$span(style = "font-weight:700;", "Estimated Transfer Value: "),
        tags$span(
          style = paste0("font-weight:700; color:", est_col, "; margin-left:6px;"),
          est_display
        ),
        actionButton("est_info_btn", label = "ⓘ", style = "margin-left:8px; padding:3px;")
      )
    )
  })
  
  # show modal for est value inputs
  observeEvent(input$est_info_btn, {
    showModal(modalDialog(
      title = "Estimated transfer value - inputs used",
      HTML(
        "<ul>
        <li><b>OverallComposite</b> - main performance driver.</li>
        <li><b>90s</b> - Minutes availability (per 90)</li>
        <li><b>Age</b> - Younger players get a premium via (mean(Age)/Age)^1.4 and an extra youth premium.</li>
        <li><b>Elite Attributes</b> - Players get a 5% value increase for each attribute that is in the +90 percentile.</li>
        <li>Heuristic multiplier applied; values clamped to a reasonable range.</li>
        <li>The color you see the estimate value as reflects the value itself</li>
        <li><b>Color</b> - Red is less than £20m, Yellow is between £20m and £45m, and Green is more than £45m</li>
      </ul>
      <h4>Mathematical Formula</h4>
      <pre style='padding:10px; font-size:14px;'>
ETV_i = min(
    150,
    max(
        2,
        [ (Overall_i / 100)^1.4 
          * (90s_i / mean(90s)) 
          * (mean(Age) / Age_i)^2.5 
          * 65 
          * Adj_Age_i 
          * (1 + 0.05 * n_elite_i) ]
    )
)
Where:
Overall_i      = player’s overall composite (0–100)
90s_i          = number of 90-minute equivalents played
mean(90s)      = mean number of 90s across dataset
Age_i          = player’s age
mean(Age)      = mean age across dataset
Adj_Age_i      = 1.10 if <23, 1.00 if 23–30, 0.90 if >30
n_elite_i      = count of composites >=90th percentile
ETV values are bounded between £2M and £150M
      </pre>"
      ),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  
  # when user clicks team name in header (JS sets team_click input)
  observeEvent(input$team_click, {
    req(input$team_click)
    team <- input$team_click
    updateTabsetPanel(session, "main_tabs", "Page 10")
    # populate team-specific outputs in separate reactive below
    session$userData$current_team <- team
  })
  
  # ---- Similar players (top 3 Euclidean on composites) ----
  output$player_similar_ui <- renderUI({
    req(input$search)
    
    comps <- c("ShootingComposite","PassingComposite","DribblingComposite","DefendingComposite","PositioningComposite")
    if (!all(comps %in% names(DummyData))) return(NULL)
    
    base <- DummyData %>% filter(Player == input$search) %>% slice(1)
    if (nrow(base) == 0) return(NULL)
    
    vec <- as.numeric(base[comps])
    others <- DummyData %>% 
      filter(Player != base$Player[1]) %>% 
      select(Player, Team, all_of(comps))
    
    if (nrow(others) == 0) return(NULL)
    
    # Compute Euclidean distances
    others$dist <- apply(others[comps], 1, function(x) sqrt(sum((as.numeric(x) - vec)^2, na.rm = TRUE)))
    
    # Compute mathematically scaled similarity %
    max_dist <- max(others$dist, na.rm = TRUE)
    others <- others %>%
      mutate(Similarity = pmax(0, (1 - (dist / max_dist)) * 100))
    
    sim <- others %>% arrange(dist) %>% slice(1:3)
    
    tagList(
      tags$div(
        style = "display:flex; align-items:center; justify-content:space-between;",
        tags$h4("Most similar players"),
        actionButton("similar_info_btn", label = "ⓘ", style = "padding:3px; margin-left:8px; font-size:14px;")
      ),
      
      lapply(seq_len(nrow(sim)), function(i) {
        pname <- sim$Player[i]
        pteam <- sim$Team[i]
        rating <- round(sim$Similarity[i], 1)
        
        tags$p(
          HTML(paste0(
            "<b>", i, ".</b> ",
            "<a href='#' style='color:#1E90FF; text-decoration:underline;' ",
            "onclick=\"Shiny.setInputValue('search','", gsub("'", "\\'", pname), "',{priority:'event'});\">",
            pname, "</a> — ", pteam,
            " <span style='color:#AAAAAA;'>(Similarity: ", rating, "%)</span>"
          ))
        )
      })
    )
  })
  
  # ---- Info modal for similar players ----
  observeEvent(input$similar_info_btn, {
    showModal(modalDialog(
      title = "Similarity Calculation Explanation",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      
      # Top half
      tags$div(
        style = "margin-bottom:15px;",
        tags$h4("How Similar Players Are Determined"),
        tags$p("Players are compared using five composite performance metrics: Shooting, Passing, Dribbling, Defending, and Positioning."),
        tags$p("The Euclidean distance measures how far apart two players are in this five-dimensional performance space. 
              Smaller distances indicate stronger similarity.")
      ),
      
      # Bottom half (LaTeX)
      tags$div(
        tags$h4("Mathematical Formula"),
        withMathJax(
          helpText("The Euclidean distance between player ",
                   "\\( i \\) and player \\( j \\) is:"),
          helpText("$$
        d(i, j) = \\sqrt{(S_i - S_j)^2 + (P_i - P_j)^2 + (D_i - D_j)^2 + (Df_i - Df_j)^2 + (Pos_i - Pos_j)^2}
        $$"),
          helpText("The similarity percentage is computed as:"),
          helpText("$$
        Similarity(i, j) = \\left(1 - \\frac{d(i, j)}{d_{max}}\\right) \\times 100
        $$"),
          helpText("This ensures similarity is proportional to actual statistical distance — not artificially scaled.")
        )
      )
    ))
  })
  
  
  # ---- Player radar (plotly) ----
  output$player_radar <- renderPlotly({
    req(input$search)
    comps <- c("ShootingComposite","PassingComposite","DribblingComposite","DefendingComposite","PositioningComposite")
    p <- DummyData %>% filter(Player == input$search) %>% slice(1)
    if (nrow(p) == 0) return(plotly_empty())
    
    values <- sapply(comps, function(cn) as.numeric(p[[cn]][1]) %||% 0)
    categories <- c("Shooting","Passing","Dribbling","Defending","Positioning")
    
    # close loop
    r_vals <- c(values, values[1])
    theta <- c(categories, categories[1])
    
    plot_ly(
      type = 'scatterpolar', 
      r = r_vals, 
      theta = theta, 
      fill = 'toself',
      line = list(color = 'rgba(30,144,255,0.9)', width = 2), 
      marker = list(size = 6),
      hoverinfo = 'text',
      text = paste0(theta, ": ", round(c(r_vals),1))
    ) %>%
      layout(
        polar = list(
          domain = list(x = c(0, 0.5)),      # move radar chart to left half of div
          radialaxis = list(
            range = c(0,100), 
            tickvals = seq(0,100,20),
            tickfont = list(color = "black"),
            gridcolor = "black"
          ),
          angularaxis = list(tickfont = list(color = "white")) # attribute labels white
        ),
        margin = list(l = 20, r = 20, t = 50, b = 50), # enough room so top/bottom labels render
        showlegend = FALSE,
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
  })
  
  
  # ---- Collapsible composite tables with weights and percentiles ----
  # Build HTML for each composite with details rows
  build_stat_rows_html <- function(player_row, stats, weights) {
    # returns a tagList of <tr>
    rows <- lapply(stats, function(stat) {
      if (!(stat %in% names(player_row))) return(NULL)
      v <- player_row[[stat]][1]
      vnum <- ifelse(is.na(v), NA, round(as.numeric(v), 2))
      perc_name <- paste0(stat, "_Percentile")
      perc <- ifelse(perc_name %in% names(player_row), as.numeric(player_row[[perc_name]][1]), NA)
      wt <- ifelse(!is.null(weights) && stat %in% names(weights), weights[stat], NA)
      # color logic for bar
      bar_color <- ifelse(is.na(perc), "#9AA0A6", ifelse(perc >= 70, "#2ECC71", ifelse(perc >= 30, "#9AA0A6", "#E74C3C")))
      bar_width <- ifelse(is.na(perc), 0, perc)
      tags$tr(
        tags$td(stat),
        tags$td(style = "text-align:right;", ifelse(is.na(vnum), "", format(vnum, nsmall = 2))),
        tags$td(style = "text-align:right;", ifelse(is.na(wt), "", paste0(wt, "%"))),
        tags$td(style = "text-align:right;", ifelse(is.na(perc), "", paste0(round(perc,1), "%"))),
        tags$td(tags$div(style = "width:140px; background:#eee; height:12px; border-radius:6px; overflow:hidden;",
                         tags$div(style = paste0("width:", bar_width, "%; height:12px; background:", bar_color, ";"))))
      )
    })
    do.call(tagList, rows)
  }
  
  output$player_composites_ui <- renderUI({
    req(input$search)
    player_row <- DummyData %>% filter(Player == input$search) %>% slice(1)
    if (nrow(player_row) == 0) return(NULL)
    
    # Overall composite: show the composites that make it up and their weights
    overall_weights <- c(Shooting = 15, Passing = 25, Dribbling = 20, Defending = 25, Positioning = 15)
    comps_list <- list(
      Overall = list(col = "OverallComposite", stats = c("ShootingComposite","PassingComposite","DribblingComposite","DefendingComposite","PositioningComposite"), weights = overall_weights),
      Shooting = list(col = "ShootingComposite", stats = stat_map$Shooting, weights = stat_weights$Shooting),
      Passing = list(col = "PassingComposite", stats = stat_map$Passing, weights = stat_weights$Passing),
      Dribbling = list(col = "DribblingComposite", stats = stat_map$Dribbling, weights = stat_weights$Dribbling),
      Defending = list(col = "DefendingComposite", stats = stat_map$Defending, weights = stat_weights$Defending),
      Positioning = list(col = "PositioningComposite", stats = stat_map$Positioning, weights = stat_weights$Positioning)
    )
    
    ui_list <- lapply(names(comps_list), function(name) {
      comp <- comps_list[[name]]
      comp_val <- if (comp$col %in% names(player_row)) round(as.numeric(player_row[[comp$col]][1]), 2) else NA
      comp_perc <- if (paste0(comp$col, "_Percentile") %in% names(player_row)) round(as.numeric(player_row[[paste0(comp$col, "_Percentile")]][1]),1) else NA
      
      if (name == "Overall") {
        # show sub-composites table
        sub_rows <- lapply(names(comp$weights), function(k) {
          v <- if (paste0(k, "Composite") %in% names(player_row)) round(as.numeric(player_row[[paste0(k, "Composite")]][1]),2) else NA
          tags$tr(tags$td(k), tags$td(style="text-align:right;", ifelse(is.na(v), "", v)),
                  tags$td(style="text-align:right;", paste0(comp$weights[[k]], "%")))
        })
        sub_table <- tags$table(class = "stat-table", tags$thead(tags$tr(tags$th("Composite"), tags$th("Value"), tags$th("Weight"))), tags$tbody(sub_rows))
        tags$details(
          tags$summary(class = "composite-summary", paste0("OverallComposite: ", comp_val, if (!is.na(comp_perc)) paste0(" (", comp_perc, "th percentile)"))),
          tags$div(style = "margin-top:8px;", sub_table)
        )
      } else {
        # build stat rows table
        stat_rows <- build_stat_rows_html(player_row, comp$stats, comp$weights)
        stat_table <- tags$table(class = "stat-table",
                                 tags$thead(tags$tr(tags$th("Stat"), tags$th("Value"), tags$th("Weight"), tags$th("Percentile"), tags$th("Bar"))),
                                 tags$tbody(stat_rows))
        tags$details(
          tags$summary(class = "composite-summary", paste0(name, " Composite: ", comp_val, if (!is.na(comp_perc)) paste0(" (", comp_perc, "th percentile)"))),
          tags$div(style = "margin-top:8px;", stat_table)
        )
      }
    })
    
    do.call(tagList, ui_list)
  })
  
  # ---- Team page: header, stats, players ----
  # reactive team (set via team_click JS or other navigation)
  reactive_team <- reactiveVal(NULL)
  observeEvent(input$team_click, { reactive_team(input$team_click) })
  # also allow query string to set team
  observe({
    qs <- getQueryString()
    if (!is.null(qs$team)) reactive_team(qs$team)
  })
  
  output$team_header <- renderUI({
    team <- reactive_team()
    if (is.null(team)) team <- input$selected_team
    req(team)
    
    # Filter for the team (case-insensitive)
    matches <- DummyData %>%
      filter(grepl(tolower(trimws(team)), tolower(trimws(Team)), fixed = TRUE))
    
    if (nrow(matches) == 0) return(NULL)
    
    t <- matches %>% slice(1) %>% as.data.frame()
    team_name <- as.character(t$Team[1])
    url <- as.character(unlist(t[["Team Image Address"]]))[1]
    if (is.na(url) || !nzchar(url)) url <- "https://via.placeholder.com/60?text=?"
    
    # Build team header with image, name, and close button
    tagList(
      div(class = "team-header",
          img(src = url, height = "60px",
              style = "margin-right:15px; border-radius:8px; vertical-align:middle;"),
          h2(team_name, style = "display:inline-block; vertical-align:middle;"),
          actionLink("close_team", "×", class = "close-btn",
                     style = "float:right; font-size:40px; color:#fff; line-height:34px; margin-left:12px; margin-top:-8px;")
      )
    )
  })
  
  
  
  
  # ---- Team stats (averages + ranking for all composites including EstValue) ----
  output$team_stats <- renderDT({
    team <- reactive_team()
    if (is.null(team)) return(datatable(data.frame(Message="No team selected"), options = list(dom = 't')))
    
    # ---- Composite columns
    comps <- c("OverallComposite","ShootingComposite","PassingComposite","DribblingComposite","DefendingComposite","PositioningComposite")
    
    # ---- Filter for selected team
    team_df <- DummyData %>% filter(Team == team)
    if (nrow(team_df) == 0) return(datatable(data.frame(Message="No players found"), options = list(dom='t')))
    
    # ---- Compute team averages
    team_means <- team_df %>% summarise(across(all_of(comps), ~ round(mean(.x, na.rm=TRUE),1)))
    
    # ---- Compute team average Estimated Value
    avg_est <- round(mean(team_df$EstValue, na.rm=TRUE),1)
    team_means$AverageEstValue <- avg_est
    
    # ---- Compute rankings relative to all teams
    all_means <- DummyData %>% group_by(Team) %>% summarise(across(all_of(comps), ~ mean(.x, na.rm=TRUE)), AverageEstValue = mean(EstValue, na.rm=TRUE)) %>% ungroup()
    rank_row <- sapply(c(comps, "AverageEstValue"), function(cn) rank(-all_means[[cn]])[all_means$Team == team])
    
    # ---- Convert ranks to strings
    rank_row_str <- paste0(rank_row, "th / ", nrow(all_means))
    rank_row_df <- as.data.frame(t(rank_row_str))
    names(rank_row_df) <- c(comps, "AverageEstValue")
    
    # ---- Convert averages to character for bind_rows
    team_means_chr <- team_means %>% mutate(across(everything(), as.character))
    team_means_chr$AverageEstValue <- paste0("£", team_means_chr$AverageEstValue, "M")
    
    # ---- Format rank row Estimated Value too
    rank_row_df$AverageEstValue <- paste0("", rank_row_df$AverageEstValue)
    
    # ---- Bind averages + ranks
    out_df <- bind_rows(
      data.frame(Row = "Average (team mean)", team_means_chr, check.names = FALSE),
      data.frame(Row = "Rank (out of teams)", rank_row_df, check.names = FALSE)
    )
    
    # ---- Render DT
    datatable(out_df, rownames = FALSE, options = list(dom='t', pageLength = 5))
  })
  
  
  
  output$team_players <- renderDT({
    team <- reactive_team()
    if (is.null(team)) {
      return(datatable(data.frame(Message = "No team selected"), options = list(dom = 't')))
    }
    
    # Keep Team in the data so color_dt_by_team can read it
    df <- DummyData %>%
      filter(Team == team) %>%
      select(Player, Team, Age, `Midfielder Type`, `90s`, OverallComposite, EstValue) %>%
      mutate(EstValue = paste0("£", round(as.numeric(EstValue), 1), "M"))
    
    # Build DT (Team column present so color_dt_by_team can use it)
    dt <- datatable(
      df,
      options = list(pageLength = 50, lengthChange = FALSE),
      selection = "single",
      rownames = FALSE,
      escape = FALSE
    )
    
    # Apply the same team-based coloring used for leaderboards
    dt <- color_dt_by_team(dt, df = df, cols = names(df))
    
    dt
  })
  
  
  output$team_players_bar <- renderPlot({
    team <- reactive_team()
    if (is.null(team)) return(NULL)
    
    # Grab team + colors
    df <- DummyData %>%
      filter(Team == team)
    
    # Get team color from your colors list
    team_color <- team_colors[[team]]
    if (is.null(team_color)) team_color <- "#4e79a7"  # fallback
    
    # Reorder players by 90s
    df <- df %>% arrange(desc(`90s`))
    
    # Plot
    par(mar = c(10, 4, 3, 1))   # bottom margin = 10 → gives room for labels
    
    barplot(
      height = pmin(df$`90s`, 38),      # cap max at 38
      names.arg = df$Player,
      las = 2,                          # horizontal labels
      col = team_color,                 # inside bar color
      border = "black",                 # black border around bars
      ylim = c(0, 40),                  # leave space above bars
      main = "Player 90s Played",
      ylab = "90s Played (capped at 38)"
    )
  })
  
  
  
  
  observeEvent(input$team_players_rows_selected, {
    sel <- input$team_players_rows_selected
    team <- reactive_team()
    
    # Ensure both exist
    if (!is.null(sel) && length(sel) > 0 && !is.null(team)) {
      # Use the *exact same data frame* used in renderDT
      df <- DummyData %>%
        filter(Team == team) %>%
        select(Player, Team, Age, `Midfielder Type`, `90s`, OverallComposite, EstValue)
      
      # Get the correct player by row number
      player <- df$Player[sel]
      
      # Update the profile
      updateSelectInput(session, "search", selected = player)
      updateTabsetPanel(session, "main_tabs", "Page 8")
    }
  })
  
  
  # END server
}

# ---- run app ----
shinyApp(ui = ui, server = server)







