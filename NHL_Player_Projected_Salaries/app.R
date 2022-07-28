#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Put needed data in ------------------------------------------------------


library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(ggthemes)
library(plotly)
library(shinyWidgets)

`Past Seasons Player Salaries` <- read_csv("All Seasons Player Salary Projections Short Version.csv") %>%
  mutate(team = gsub('L.A', 'LAK', team),
         team = gsub('N.J', 'NJD', team),
         team = gsub('S.J', 'SJS', team),
         team = gsub('T.B', 'TBL', team)) %>%
  select(player,
         team,
         position,
         season,
         games_played,
         age,
         projected_cap_hit,
         everything())
`Past Seasons Player Salaries` <- `Past Seasons Player Salaries` %>%
  rename(Player = player,
         Position = position,
         Team = team,
         Season = season,
         `Games Played` = games_played,
         Age = age,
         `Projected Cap Hit Percentage` = projected_percent_cap_hit,
         `Actual Cap Hit` = cap_hit,
         `Projected Cap Hit` = projected_cap_hit) %>%
  mutate(`Team Savings` = `Projected Cap Hit` - `Actual Cap Hit`)


# The app code ------------------------------------------------------------
choicesAll <- `Past Seasons Player Salaries` %>%
  select(Player,
         Team,
         Position,
         Season,
         `Games Played`,
         Age)

team <- `Past Seasons Player Salaries`$Team
season <- `Past Seasons Player Salaries`$Season
position_names <- c("D" = "Defense", "F" = "Forward")
`Past Seasons Player Salaries`$Position <-
  as.character(position_names[`Past Seasons Player Salaries`$Position])
position <- `Past Seasons Player Salaries`$Position
actual_cap_hit <- `Past Seasons Player Salaries`$`Actual Cap Hit`
  


# Start UI part of app ----------------------------------------------------


ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  titlePanel(
    div(span("Paying NHL Players What They Are Worth", style = "color:black", align = "center"),
        align = "center",
        br(),
        span(em(h6("Seasons 2010-11 to 2015-16 Have Less Data Due to Insufficient Salary Tracking Data")))), 
    windowTitle =  "Paying NHL Players What They Are Worth"
  ),
  shinytitle::use_shiny_title(),
  sidebarLayout(
    sidebarPanel(width = 2,
      checkboxGroupInput("show_vars_all", "Player Attributes to Show:",
                         names(choicesAll),
                         selected = names(choicesAll)),
      br(),
      pickerInput(
        inputId = "team", 
        label = "Select Team(s)",
        choices = str_sort(unique(team)),
        selected = unique(team),
        options = list(`actions-box` = TRUE,
                       create = FALSE,
                       placeholder = "Please Select a Team",
                       onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       onType = I("function (str) {if (str === \"\") {this.close();}}"),
                       onItemAdd = I("function() {this.close();}")),
        multiple = T
      ),
      pickerInput(
        inputId = "position", 
        label = "Select Position(s)",
        choices = str_sort(unique(position), decreasing = T),
        selected = unique(position),
        options = list(`actions-box` = TRUE,
                       create = FALSE,
                       placeholder = "Please Select a Position",
                       onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       onType = I("function (str) {if (str === \"\") {this.close();}}"),
                       onItemAdd = I("function() {this.close();}")),
        multiple = T
      ),
      br(),
      pickerInput(
        inputId = "season", 
        label = "Select Season(s)",
        choices = str_sort(unique(season), decreasing = T),
        selected = "2021-22",
        options = list(`actions-box` = TRUE,
                       create = FALSE,
                       placeholder = "Please Select a Season",
                       onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       onType = I("function (str) {if (str === \"\") {this.close();}}"),
                       onItemAdd = I("function() {this.close();}")),
        multiple = T
      ),
      sliderInput(
        "cap_hit",
        label = "Player's Actual Cap Hit",
        min = min(unique(actual_cap_hit)),
        max = max(unique(actual_cap_hit)),
        value = c(min(unique(actual_cap_hit)), max(unique(actual_cap_hit)))
      )
  ),
    mainPanel(
      DT::dataTableOutput("mytable1"), 
      plotlyOutput("plot1"),
      uiOutput("link")
    )
  )
)


# Server Part of app ------------------------------------------------------


server <- function(input, output) {
  
  # Make the season selection reactive to the user
  displayData <- reactive({
    `Past Seasons Player Salaries` %>%
      filter(Season %in% input$season,
             Team %in% input$team,
             Position %in% input$position,
             `Actual Cap Hit` %in% c(input$cap_hit[1]:input$cap_hit[2]))
  })
  
  # Make the datatable
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(displayData()[, c(input$show_vars_all, "Projected Cap Hit Percentage", "Projected Cap Hit", "Actual Cap Hit", "Team Savings"), drop = FALSE],
                  caption = "Players Must Have Played At Least 20 Games in a Season To Qualify") %>%
      formatCurrency(c("Projected Cap Hit", "Actual Cap Hit", "Team Savings"),
                     currency = "$", interval = 3, mark = ",") %>%
      formatPercentage("Projected Cap Hit Percentage", digits = 2)
  })
  
  # Make the plot that goes with it
  output$plot1 <- renderPlotly({
    plot <-
      ggplotly(
      ggplot(data = displayData(),
             aes(label = Player,
                 label2 = Team,
                 label3 = Season,
                 x = `Projected Cap Hit`,
                 y = `Actual Cap Hit`,
                 color = `Team Savings`)) +
        geom_abline(slope = 1, 
                    intercept = 0,
                    color = "black") +
        geom_point(alpha = 0.2) +
        theme_gdocs() +
        scale_color_gradient(low = "darkblue",
                            high = "darkorange",
                            labels = scales::dollar) +
        scale_x_continuous(labels = scales::dollar) +
        scale_y_continuous(labels = scales::dollar) +
        ggtitle("How Players Meet Contract Expectations") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5,
                                        face = "bold"))
      )
      plot %>%
        style(text = paste0("Player: ", displayData()$Player,
                            "</br></br>",
                            "Team: ", displayData()$Team,
                            "</br>",
                            "Season: ", displayData()$Season,
                            "</br>",
                            "Projected Cap Hit: ", scales::dollar(displayData()$`Projected Cap Hit`),
                            "</br>",
                            "Actual Cap Hit: ", scales::dollar(displayData()$`Actual Cap Hit`),
                            "</br>",
                            "Team Savings: ", scales::dollar(displayData()$`Team Savings`)
                            )
              )
  })
  
  shinytitle::change_window_title(title = "Paying NHL Players What They Are Worth")
  
  linkToHTML <- a("Projected Salary Methodology", href = "https://rpubs.com/hsjens/927909")
  output$link <- renderUI({
    tagList(tags$h5("", linkToHTML))
  })
  
}

shinyApp(ui, server)