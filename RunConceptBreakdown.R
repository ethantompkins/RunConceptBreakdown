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

# Create a function to process player performance data for a team
process_player_data <- function(team_abbr) {
  team_pbp <- pbp_data %>% 
    filter(posteam == team_abbr, play_type %in% c("pass", "run"))
  
  player_data <- team_pbp %>%
    group_by(player_name = if_else(play_type == "pass", receiver_player_name, rusher_player_name)) %>%
    filter(!is.na(player_name)) %>%
    summarise(
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
      productivity_rank = rank(-total_yards)
    ) %>%
    arrange(desc(total_yards))
  
  return(player_data)
}

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("NFL Team Player Performance Tracker - 2024 Season"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select a Team:", choices = unique(pbp_data$posteam), width = '100%'),
      helpText("Select an NFL team to see its top offensive players' performance for the 2024 season."),
      width = 2
    ),
    mainPanel(
      width = 10,
      gt_output("player_table"),
      plotOutput("player_plot")
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  player_data <- reactive({
    process_player_data(input$team)
  })
  
  output$player_table <- render_gt({
    player_data() %>%
      gt() %>%
      tab_header(
        title = html(if (is.null(input$team) || input$team == "") {
          "Player Performance Breakdown"
        } else {
          paste0("Player Performance Breakdown: <img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/nfl/500/", tolower(input$team), ".png' width='30' style='vertical-align:middle;'>")
        }),
        subtitle = "Data through the 2024 NFL Season"
      ) %>%
      cols_label(
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
        source_note = md("Data: nflfastR & nflverse | Table: User-Generated")
      )
  })
  
  output$player_plot <- renderPlot({
    player_data() %>%
      ggplot(aes(x = reorder(player_name, -total_yards), y = total_yards, fill = player_name)) +
      geom_bar(stat = "identity") +
      geom_nfl_headshots(aes(player_gsis = player_name), height = 0.08) +
      geom_nfl_logos(aes(team_abbr = input$team), height = 0.1) +
      labs(
        title = paste("Top Offensive Players for", input$team, "- 2024 Season"),
        x = "Player Name",
        y = "Total Yards",
        fill = "Player Name"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)