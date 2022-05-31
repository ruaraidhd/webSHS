library(tidyverse)
library(EnvStats)
library(lubridate)
library(shiny)
library(shinycssloaders)
library(ggrepel)
library(readxl)
library(shinyWidgets)

library(RSQLite)

source("peak deposition analysis.R")

ui <- fluidPage(
  titlePanel("WebSHS"),
  sidebarLayout(sidebarPanel(p("What does smoking indoors mean for particle air pollution in your home? Choose some options below to see a simulation."),
                              sliderInput(inputId = "cigarettes",
                                         label="Cigarettes smoked indoors yesterday",
                                         min=1,
                                         max=40,
                                         value=11
                                         ),
                              radioGroupButtons(inputId = "windows",
                                                   label="Were your windows mostly open or closed?",
                                                   choices=list("Open"=1, "Closed"=2),
                                                   selected = 2,
                                                   justified = TRUE,
                                                   status = "primary",
                                                   checkIcon = list(yes=icon("ok", lib = "glyphicon"))
                                                   ),
                             checkboxGroupButtons(inputId = "smoking_times",
                                                  label = "When did you smoke?",
                                                  choiceNames = c("Morning (06:00 - 11:59)", 
                                                              "Afternoon (12:00 - 17:59)", 
                                                              "Evening (18:00 - 23:59)", 
                                                              "Night (00:00 - 05:59)"),
                                                  choiceValues = c("morning", "afternoon", "evening", "night"),
                                                  status = "primary",
                                                  selected = c("morning", "evening"),
                                                  direction = "vertical",
                                                  justified = TRUE,
                                                  checkIcon = list(
                                                    yes = icon("ok", 
                                                               lib = "glyphicon"),
                                                    no = icon("remove",
                                                              lib = "glyphicon"))
                                                  )
                             ),
                mainPanel(
                          h3(textOutput("pm2.5_value", inline=TRUE)),
                          plotOutput(outputId = "pm2.5_plot") %>% withSpinner(color="#327da8"),
                          em("The ", span("red dashed line", style="color: #b04633;"), "represents the WHO guideline limit for average PM2.5 exposure over 24 hours"),
                          plotOutput(outputId = "visualisation")
                          )
                )
  )


server <- function(input, output) {
  peaks_data <- reactive({
      mydb <- dbConnect(SQLite(), "webshs-db.sqlite")
      if(input$windows == 1) {
        aer <- 2.0
      } else {
        aer <- 0.6
      }
      if ("morning" %in% input$smoking_times) {
        morning_smoking <- TRUE
      } else { 
        morning_smoking <- FALSE
      }
    
      if ("afternoon" %in% input$smoking_times) {
        afternoon_smoking <- TRUE
      } else { 
        afternoon_smoking <- FALSE
      }

      if ("evening" %in% input$smoking_times) {
        evening_smoking <- TRUE
      } else { 
        evening_smoking <- FALSE
      }
    
      if ("night" %in% input$smoking_times) {
        night_smoking <- TRUE
      } else { 
        night_smoking <- FALSE
      }
      
      dbDisconnect(mydb)

      generate_peaks(input$cigarettes, 
                     aer, 
                     morning_smoking,
                     afternoon_smoking,
                     evening_smoking,
                     night_smoking
                     ) 
    })
  output$pm2.5_plot <- renderPlot({
    ggplot(peaks_data(), aes(x=datetime, y=pm2.5)) +
      geom_line(color="#327da8", size=1.5) +
      geom_hline(yintercept = 25, color="#b04633", linetype="dashed", size=1) +
      xlab("Time") +
      ylab("PM2.5 (µg/m³)") +
      ggtitle("Second-hand smoke in your home") +
      labs(caption = "Estimated PM2.5 deposition rates after He et al, 2005") +
      theme_minimal() +
      theme(text=element_text(size=18, family="Calibri Light"))
  })
  output$mean_pm2.5 <- renderText({round(mean(peaks_data()$pm2.5, na.rm=TRUE), 0)})
  output$pm2.5_value <- renderText({paste("Second-hand smoke in your home led to an estimated average PM2.5 level of", round(mean(peaks_data()$pm2.5, na.rm=TRUE), 0), "µg/m³ over this day")})
  output$visualisation <- renderPlot({
    generate_viz(round(mean(peaks_data()$pm2.5, na.rm=TRUE), 0))
  })
}

shinyApp(ui = ui, server = server)

