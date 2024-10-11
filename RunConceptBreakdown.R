library(shiny)
library(dplyr)
library(nflfastR)
library(nflseedR)
library(nflreadr)
library(nflplotR)
library(nflverse)
library(ggplot2)
library(gt)
library(stringr)

# Load the play-by-play data from nflfastr for the 2024 season
pbp_data <- load_pbp(seasons = 2024)

# Load roster data to get player GSIS IDs
roster_data <- load_rosters(seasons = 2024) %>%
  select(gsis_id, full_name)

# Create a function to process player performance data for a team
process_player_data <- function(team_abbr, ranking_metric) {
  team_pbp <- pbp_data %>% 
    filter(posteam == team_abbr, play_type %in% c("pass", "run")) %>%
    mutate(player_gsis = if_else(play_type == "pass", receiver_id, rusher_id)) %>%
    left_join(roster_data, by = c("player_gsis" = "gsis_id")) %>%
    mutate(player_name = full_name) %>%
    filter(!is.na(player_gsis)) %>%
    left_join(roster_data, by = c("player_gsis" = "gsis_id"))
  
  player_data <- team_pbp %>%
    group_by(player_name, player_gsis) %>%
    summarise(.groups = 'drop',
              targets = sum(!is.na(receiver_player_name) & play_type == "pass", na.rm = TRUE),
              receptions = sum(complete_pass == 1, na.rm = TRUE),
              receiving_yards = sum(yards_gained * (play_type == "pass"), na.rm = TRUE),
              carries = sum(!is.na(rusher_player_name) & play_type == "run", na.rm = TRUE),
              rushing_yards = sum(yards_gained * (play_type == "run"), na.rm = TRUE),
              total_yards = sum(yards_gained, na.rm = TRUE),
              touchdowns = sum(touchdown, na.rm = TRUE),
              epa_per_play = mean(epa, na.rm = TRUE),
              fantasy_ppr = sum(yards_gained * 0.1 + touchdowns * 6 + receptions, na.rm = TRUE),
              fantasy_half_ppr = sum(yards_gained * 0.1 + touchdowns * 6 + receptions * 0.5, na.rm = TRUE)
    ) %>%
    mutate(
      receptions_carries = receptions + carries,
      productivity_rank = rank(-!!sym(ranking_metric))
    ) %>%
    arrange(desc(!!sym(ranking_metric)))
  
  return(player_data)
}

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("NFL Team Player Performance Tracker - 2024 Season"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select a Team:", choices = c("", unique(pbp_data$posteam)), width = '100%'),
      selectInput("ranking_metric", "Select Ranking Metric:", 
                  choices = c("Total Yards" = "total_yards", 
                              "Fantasy PPR Points" = "fantasy_ppr",
                              "Receptions/Runs" = "receptions_carries"),
                  selected = "total_yards", width = '100%'),
      helpText("Select an NFL team to see its top offensive players' performance for the 2024 season."),
      width = 2
    ),
    mainPanel(
      width = 10,
      gt_output("player_table")
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  player_data <- reactive({
    process_player_data(input$team, input$ranking_metric)
  })
  
  output$player_table <- render_gt({
    req(input$team != "" && input$team != "NA")
    player_data() %>%
      gt() %>%
      gt_nfl_headshots(columns = "player_gsis", height = 60) %>%
      cols_move(columns = c(player_name), after = player_gsis) %>%
      cols_label(
        player_gsis = "",
        player_name = "Player Name",
        targets = "Targets",
        receptions = "Receptions",
        receiving_yards = "Receiving Yards",
        carries = "Carries",
        rushing_yards = "Rushing Yards",
        total_yards = "Total Yards",
        touchdowns = "Touchdowns",
        epa_per_play = "EPA/Play",
        fantasy_ppr = "PPR Fantasy Points",
        fantasy_half_ppr = "Half PPR Fantasy Points",
        productivity_rank = "Productivity Rank"
      ) %>%
      fmt_number(
        columns = c("epa_per_play", "total_yards", "touchdowns", "fantasy_ppr", "fantasy_half_ppr", "receiving_yards", "rushing_yards"),
        decimals = 2
      ) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_source_note(
        source_note = md("Data: nflfastR | Twitter: @ETOMK")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)