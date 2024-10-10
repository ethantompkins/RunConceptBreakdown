library(shiny)
library(dplyr)
library(nflfastR)
library(nflseedR)
library(nflreadr)
library(nflplotR)
library(nflverse)
library(ggplot2)
library(DT)

# Load the play-by-play data from nflfastr
pbp_data <- load_pbp(seasons = 2023)

# Define run concepts
run_concepts <- list(
  "Gap Scheme" = c("Man/Duo", "Power", "Draw", "Counter", "Trap"),
  "Zone Scheme" = c("Inside Zone", "Outside Zone"),
  "Other" = c("Scramble", "Trick/WR Run", "QB Sneak")
)

# Create a function to process data
process_run_data <- function(team_abbr) {
  team_pbp <- pbp_data %>% 
    filter(posteam == team_abbr, play_type == "run")
  
  run_data <- team_pbp %>% 
    group_by(run_concept = case_when(
      str_detect(desc, "Duo|Man") ~ "Man/Duo",
      str_detect(desc, "Power") ~ "Power",
      str_detect(desc, "Draw") ~ "Draw",
      str_detect(desc, "Counter") ~ "Counter",
      str_detect(desc, "Trap") ~ "Trap",
      str_detect(desc, "Inside Zone") ~ "Inside Zone",
      str_detect(desc, "Outside Zone") ~ "Outside Zone",
      str_detect(desc, "Scramble") ~ "Scramble",
      str_detect(desc, "Trick|End Around|Jet Sweep|WR Run") ~ "Trick/WR Run",
      str_detect(desc, "QB Sneak") ~ "QB Sneak",
      TRUE ~ "Other"
    )) %>%
    summarise(
      attempts = n(),
      total_yards = sum(yards_gained, na.rm = TRUE),
      epa_per_play = mean(epa, na.rm = TRUE)
    ) %>%
    mutate(
      run_pct = attempts / sum(attempts),
      run_pct_rank = rank(-run_pct)
    ) %>%
    arrange(desc(run_pct))
  
  return(run_data)
}

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("NFL Team Run Concept Breakdown"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select a Team:", choices = unique(pbp_data$posteam)),
      helpText("Select an NFL team to see its run concept breakdown.")
    ),
    mainPanel(
      DTOutput("run_table"),
      plotOutput("run_plot")
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  run_data <- reactive({
    process_run_data(input$team)
  })
  
  output$run_table <- renderDT({
    run_data() %>% 
      mutate(
        concept_category = case_when(
          run_concept %in% run_concepts[["Gap Scheme"]] ~ "Gap Scheme",
          run_concept %in% run_concepts[["Zone Scheme"]] ~ "Zone Scheme",
          run_concept %in% run_concepts[["Other"]] ~ "Other",
          TRUE ~ "Other"
        )
      ) %>%
      arrange(concept_category, desc(run_pct)) %>%
      datatable(
        options = list(pageLength = 15),
        rownames = FALSE,
        colnames = c("Concept Category", "Run Concept", "Attempts", "Total Yards", "EPA/Play", "Run %", "Run % Rank"),
        extensions = 'Buttons',
        class = 'stripe hover row-border',
        callback = JS('table.on("order.dt", function() { table.column(0).nodes().to$().css({"background-color": "#f0f0f0"}); });')
      ) %>%
      formatPercentage("run_pct", 1) %>%
      formatRound(columns = c("epa_per_play", "total_yards"), digits = 2)
  })
  
  output$run_plot <- renderPlot({
    run_data() %>% 
      ggplot(aes(x = reorder(run_concept, -run_pct), y = run_pct, fill = run_concept)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0("Rank: ", run_pct_rank)), vjust = -0.5) +
      labs(
        title = paste("Run Concept Breakdown for", input$team),
        x = "Run Concept",
        y = "Run %",
        fill = "Run Concept"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)