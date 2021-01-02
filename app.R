library(tidyverse)
library(EnvStats)
library(lubridate)
library(shiny)
library(shinycssloaders)
library(ggrepel)
library(readxl)

generate_peaks <- function(cigarettes) {
  # This is a test implementation of some analysis based on
  # He, C., Morawska, L., & Gilbert, D. (2005). Particle deposition rates in 
  # residential houses. Atmospheric Environment, 39(21), 3891–3899. 
  # https://doi.org/10.1016/j.atmosenv.2005.03.016
  
  # Looking for deposition rates
  # The function is:
  # log(Cin / Cin0) = -(air exchange rate + deposition rate) * time
  # a = AER
  # k = deposition rate
  # t = time
  
  # AERs can vary a lot, use values between 0.6 and 3.0 to start with
  a <- 0.65 # this is per hour by convention
  a_minutes <- a/60 # AER expressed in changes per minute
  
  # Deposition rates are highly particle-size specific. Let's start with
  # particles under 0.2 um
  k <- 2 # this is per hour
  k_minutes<- k/60
  
  # To calculate the current concentration, we must have
  # Cin/Cin0 = exp(-(a + k) * t)
  # Cin = Cin0 * exp(-(a + k) * t)
  
  Cin0 <- 300
  Cin0 * exp(-(a_minutes + k_minutes) * 30)
  
  # Get an SHS-like peak value - actually skip this for now
  # last_peaks <- read_excel("last peaks.xlsx")
  yesterday = today() - 1
  start_dttm <- as.POSIXct(paste(yesterday, "00:00"))
  end_dttm <- as.POSIXct(paste(yesterday, "23:59"))
  
  dt <- difftime(end_dttm, start_dttm, units="mins")
  dt <- as.numeric(dt)
  
  #baseline data
  outdoor_pm2.5 <- 8
  infiltration_factor <- 0.66
  baseline_pm2.5 <- outdoor_pm2.5 * infiltration_factor
  
  peaks_to_sim <- cigarettes
  peak_heights <- sample(50:500, size=peaks_to_sim)
  
  baseline_over_time<- tibble(datetime=seq(start_dttm, end_dttm, by="mins"))
  baseline_over_time$pm2.5 <- baseline_pm2.5
  
  for (each in peak_heights) {
    vals <- tibble()
    peak_dttm <- start_dttm + minutes(sample(1:dt, 1))
    peak_to_end <- as.numeric(difftime(end_dttm, peak_dttm, units="mins"))
    for (minute in 0:peak_to_end) {
      dttm <- peak_dttm + minutes(minute)
      pm <- each * exp(-(a_minutes + k_minutes) * minute)
      vals <- rbind(vals, tibble(datetime=dttm, minute=minute, pm=pm))
      # print(vals)
    }
    baseline_over_time <- full_join(baseline_over_time, vals, by="datetime")
    
    baseline_over_time$pm2.5 <- rowSums(baseline_over_time[, c("pm2.5", "pm")], na.rm=TRUE)
    baseline_over_time <- baseline_over_time %>% select(datetime, pm2.5)
  }
  return(baseline_over_time)
}

generate_viz <- function(datum) {
  datum <- data.frame(x=c(datum))
  min_width <- max(c(datum$x +20, 100))
  plt <- ggplot(datum, aes()) +
    xlim(0, min_width) +
    ylim(0,110) +
    # coord_cartesian(clip = "off") +
    geom_rect(xmin=0, ymin=0, xmax=12.0, ymax=100, fill="green4") +
    geom_rect(xmin=12.0, ymin=0, xmax=35.4, ymax=100, fill="yellow2") +
    geom_rect(xmin=35.4, ymin=0, xmax=55.4, ymax=100, fill="orange2") +
    geom_rect(xmin=55.4, ymin=0, xmax=150.4, ymax=100, fill="red") +
    geom_rect(xmin=150.4, ymin=0, xmax=250.4, ymax=100, fill="darkorchid4") +
    geom_rect(xmin=250.4, ymin=0, xmax=500, ymax=100, fill="brown") +
    geom_label_repel(x=6,y=5, label="Good", family="Calibri", lineheight=1, direction="y") +
    geom_label_repel(x=((35.4-12)/2)+12,y=5, label="Moderate", family="Calibri", lineheight=1, direction="y") +
    geom_label_repel(x=((55.4-35.4)/2)+35.4,y=5, label="Unhealthy for\nsensitive groups", family="Calibri", lineheight=1, direction="y") +
    geom_label_repel(x=((150.4-55.4)/2)+55.4,y=5, label="Unhealthy", family="Calibri", lineheight=1, direction="y") +
    geom_label_repel(x=((250.4-150.4)/2)+150.4,y=5, label="Very unhealthy", family="Calibri", lineheight=1, direction="y", xlim=c(150,250)) +
    geom_label_repel(x=((500-250.4)/2)+250.4,y=5, label="Hazardous", family="Calibri", lineheight=1, direction="y", xlim=c(250,Inf)) +
    # geom_rect(xmin=datum$x - 2, xmax=datum$x + 2, ymin=35, ymax=40, fill="white", color="grey22") +
    # geom_rect(xmin=datum$x - 20, xmax=datum$x + 30, ymin=40, ymax=70, color="grey22", fill="white") +
    geom_label(aes(x=x, y=50, label=paste("Your home:\n", x, "µg/m³")),  family="Calibri", size=10, lineheight=1, label.padding=unit(0.5, "lines")) +
    theme_minimal() +
    theme(axis.title=element_blank(), panel.grid = element_blank(), axis.text.y = element_blank())
  plt
  return(plt)
}

# source("peak deposition analysis.R")

ui <- fluidPage(
  titlePanel("WebSHS"),
  sidebarLayout(sidebarPanel(sliderInput(inputId = "cigarettes",
                                         label="Cigarettes smoked indoors yesterday",
                                         min=1,
                                         max=30,
                                         value=11
                                         )
                             ),
                mainPanel(plotOutput(outputId = "pm2.5_plot") %>% withSpinner(color="#327da8"),
                          # h3("PM2.5 in your home was ", textOutput("mean_pm2.5", inline=TRUE), "(µg/m³) over yesterday") %>% withSpinner(color="#327da8"),
                          plotOutput(outputId = "visualisation")
                          )))


server <- function(input, output) {
  peaks_data <- reactive({ generate_peaks(input$cigarettes) })
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
  output$visualisation <- renderPlot({
    generate_viz(round(mean(peaks_data()$pm2.5, na.rm=TRUE), 0))
  })
}

shinyApp(ui = ui, server = server)

