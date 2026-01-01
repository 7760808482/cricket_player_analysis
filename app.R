library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

cricket_data <- read.csv("cricket_data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  
  titlePanel("Cricket Player Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "player",
        "Select Player",
        choices = sort(unique(cricket_data$Player_Name)),
        selected = "Virat Kohli"
      )
    ),
    
    mainPanel(
      fluidRow(
        column(
          12,
          align = "center",
          uiOutput("player_photo_ui")
        )
      ),
      br(),
      fluidRow(
        column(3, wellPanel(
          h4("Matches"),
          textOutput("total_matches")
        )),
        column(3, wellPanel(
          h4("Runs"),
          textOutput("total_runs")
        )),
        column(3, wellPanel(
          h4("Average"),
          textOutput("avg")
        )),
        column(3, wellPanel(
          h4("Strike Rate"),
          textOutput("sr")
        ))
      ),
      
      tabsetPanel(
        
        tabPanel("TEST",
                 tableOutput("test")
        ),
        
        tabPanel("ODI",
                 tableOutput("odi")
        ),
        
        tabPanel("T20I",
                 tableOutput("t20")
        ),
        
        tabPanel("IPL",
                 tableOutput("ipl")
        )
      ),
      
      hr(),
      
      h3("Runs Distribution - International"),
      plotOutput("intl_pie"),
      
      h3("Runs Distribution - IPL"),
      plotOutput("ipl_pie"),
      
      h3("Runs Distribution (International + IPL)"),
      plotOutput("combinedPie", height = "400px"),
      
      h3("Win Probability Based on Performance"),
      plotOutput("win_pie", height = "350px"),
      
      hr(),
      h3("Bowling Performance Summary"),
      
      fluidRow(
        column(4, wellPanel(h4("Total Wickets"), textOutput("total_wickets"))),
        column(4, wellPanel(h4("Bowling Average"), textOutput("bowl_avg"))),
        column(4, wellPanel(h4("Economy Rate"), textOutput("eco_rate")))
      ),
      
      fluidRow(
        column(4, wellPanel(h4("Best Bowling"), textOutput("best_bowling"))),
        column(4, wellPanel(h4("5 Wicket Hauls"), textOutput("five_wkts"))),
        column(4, wellPanel(h4("10 Wicket Hauls"), textOutput("ten_wkts")))
      ),
      
      hr(),
      h3("Bowling Performance Charts"),
      plotOutput("wickets_bar"),
      plotOutput("economy_bar"),
    )
  )
)

server <- function(input, output) {
  
  player_data <- reactive({
    cricket_data %>%
      filter(tolower(Player_Name) == tolower(input$player))
  })
  
  format_table <- function(format_name) {
    player_data() %>%
      filter(Format == format_name) %>%
      select(
        Player_Name, Country, Role, Total_Matches, Total_Runs, Highest_Score, Average,
        Strike_Rate, Total_4s, Total_6s,
        Centuries, Half_Centuries, ICC_Ranking
      )
    
  }
  output$player_photo_ui <- renderUI({
    
    req(input$player)
    
    img_name <- paste0(input$player, ".jpg")
    img_path <- file.path("www", img_name)
    
    if (!file.exists(img_path)) {
      img_name <- "default.jpg"
    }
    
    tags$img(
      src = img_name,
      width = "180px",
      height = "220px",
      style = "border:1px solid #ccc; padding:5px;"
    )
  })
  output$test <- renderTable({ format_table("TEST") })
  output$odi  <- renderTable({ format_table("ODI") })
  output$t20  <- renderTable({ format_table("T20I") })
  output$ipl  <- renderTable({ format_table("IPL") })
  
  output$intl_pie <- renderPlot({
    
    intl_data <- player_data() %>%
      filter(Format %in% c("TEST", "ODI", "T20I")) %>%
      group_by(Format) %>%
      summarise(Runs = sum(Total_Runs), .groups = "drop")
    
    pie(
      intl_data$Runs,
      labels = paste(intl_data$Format, intl_data$Runs),
      main = "International Runs"
    )
  })
  
  output$ipl_pie <- renderPlot({
    
    ipl_data <- player_data() %>%
      filter(Format == "IPL")
    
    pie(
      ipl_data$Total_Runs,
      labels = paste("IPL", ipl_data$Total_Runs),
      main = "IPL Runs"
    )
  })
  
  output$combinedPie <- renderPlot({
    
    pie_data <- player_data() %>%
      group_by(Format) %>%
      summarise(Runs = sum(Total_Runs), .groups = "drop") %>%
      filter(Format %in% c("TEST", "ODI", "T20I", "IPL")) %>%
      mutate(
        Percent = round(Runs / sum(Runs) * 100, 1),
        label = paste0(Format, " (", Percent, "%)")
      )
    
    ggplot(pie_data, aes(x = "", y = Runs, fill = Format)) +
      geom_col(width = 1) +
      coord_polar("y") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                size = 4) +
      theme_void() +
      labs(title = paste("Runs Distribution (%) -", input$player))
  })
  
  output$win_pie <- renderPlot({
    
    win_data <- player_data() %>%
      group_by(Format) %>%
      summarise(Runs = sum(Total_Runs), .groups = "drop") %>%
      filter(Format %in% c("TEST", "ODI", "T20I", "IPL")) %>%
      mutate(
        Win_Prob = round(Runs / max(Runs) * 100, 1),
        label = paste0(Format, " (", Win_Prob, "%)")
      )
    
    ggplot(win_data, aes(x = "", y = Win_Prob, fill = Format)) +
      geom_col(width = 1) +
      coord_polar("y") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                size = 4) +
      theme_void() +
      labs(title = "Estimated Win Probability")
  })
  
  output$total_matches <- renderText({
    sum(player_data()$Total_Matches)
  })
  
  output$total_runs <- renderText({
    sum(player_data()$Total_Runs)
  })
  
  output$avg <- renderText({
    round(mean(player_data()$Average, na.rm = TRUE), 2)
  })
  
  output$sr <- renderText({
    round(mean(player_data()$Strike_Rate, na.rm = TRUE), 2)
  })
  
  output$total_wickets <- renderText({
    sum(player_data()$Total_Wickets, na.rm = TRUE)
  })
  
  output$bowl_avg <- renderText({
    avg <- mean(player_data()$Bowling_Average[player_data()$Bowling_Average > 0], na.rm = TRUE)
    ifelse(is.nan(avg), "N/A", round(avg, 2))
  })
  
  output$eco_rate <- renderText({
    eco <- mean(player_data()$Economy_Rate[player_data()$Economy_Rate > 0], na.rm = TRUE)
    ifelse(is.nan(eco), "N/A", round(eco, 2))
  })
  
  output$best_bowling <- renderText({
    bb <- player_data()$Best_Bowling[player_data()$Best_Bowling != "0"]
    ifelse(length(bb) == 0, "N/A", bb[1])
  })
  
  output$five_wkts <- renderText({
    sum(player_data()$Five_Wickets, na.rm = TRUE)
  })
  
  output$ten_wkts <- renderText({
    sum(player_data()$Ten_Wickets, na.rm = TRUE)
  })
  output$wickets_bar <- renderPlot({
    
    wickets_data <- player_data() %>%
      group_by(Format) %>%
      summarise(Wickets = sum(Total_Wickets), .groups = "drop")
    
    ggplot(wickets_data, aes(x = Format, y = Wickets, fill = Format)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Wickets by Format", y = "Total Wickets", x = "Format") +
      guides(fill = "none")
  })
  output$economy_bar <- renderPlot({
    
    eco_data <- player_data() %>%
      filter(Economy_Rate > 0) %>%
      group_by(Format) %>%
      summarise(Economy = mean(Economy_Rate), .groups = "drop")
    
    ggplot(eco_data, aes(x = Format, y = Economy, fill = Format)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Economy Rate by Format", y = "Economy Rate", x = "Format") +
      guides(fill = "none")
  })
  
  
}

shinyApp(ui, server)