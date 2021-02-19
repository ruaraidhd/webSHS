library(tidyverse)
library(EnvStats)
library(lubridate)
library(openair)
library(readxl)
library(profvis)
library(purrr)

# profvis({generate_peaks_model(10,10,as_date("2021-01-20"), 8)})

generate_peaks <- function(cigarettes, 
                           aer=0.65, 
                           morning_smoking = TRUE, 
                           afternoon_smoking = TRUE, 
                           evening_smoking = TRUE, 
                           night_smoking = TRUE) {
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
  # a <- 0.65 # this is per hour by convention - IGNORE now this is an argument
  a_minutes <- aer/60 # AER expressed in changes per minute
  
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
  
  morning_mins <- (6*60):(12*60 - 1) # 06.00 - 11.59
  afternoon_mins <- (12*60):(18*60-1) # 12.00 - 17.59
  evening_mins <- (18*60):(24*60-1) # 18.00 - 23:59
  night_mins <- 0:(6*60-1) # 00.00 - 05.59
  
  smoking_times <- c()
  
  if (morning_smoking) {
    smoking_times <- append(smoking_times, morning_mins)
  }
  if (afternoon_smoking) {
    smoking_times <- append(smoking_times, afternoon_mins)
  }
  if (evening_smoking) {
    smoking_times <- append(smoking_times, evening_mins)
  }
  if (night_smoking) {
    smoking_times <- append(smoking_times, night_mins)
  }
  
  #But what if I haven't selected anything? Assume everything instead.
  if (!any(morning_smoking, afternoon_smoking, evening_smoking, night_smoking)) {
    smoking_times <- 1:1439
  }
  
  for (each in peak_heights) {
    vals <- tibble()
    # peak_dttm <- start_dttm + minutes(sample(1:dt, 1)) # the old way - distribute through the whole day
    peak_dttm <- start_dttm + minutes(sample(smoking_times, 1)) # gets from a distribution of times
    peak_to_end <- as.numeric(difftime(end_dttm, peak_dttm, units="mins"))
    for (minute in 0:peak_to_end) {
      dttm <- peak_dttm + minutes(minute)
      pm <- each * exp(-(a_minutes + k_minutes) * minute)
      vals <- rbind(vals, tibble(datetime=dttm, minute=minute, pm=pm))
      if (pm < 2) {
        break
      }
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

# peak_gen <- function(each, start_dttm, end_dttm, dt, cutoff_pm=1) {
#   vals <- tibble()
#   peak_dttm <- start_dttm + minutes(sample(1:dt, 1))
#   peak_to_end <- as.numeric(difftime(end_dttm, peak_dttm, units="mins"))
#   for (minute in 0:peak_to_end) {
#     dttm <- peak_dttm + minutes(minute)
#     pm <- each * exp(-(a_minutes + k_minutes) * minute)
#     vals <- rbind(vals, tibble(datetime=as.POSIXct(dttm),  pm=pm))
#     if (pm < cutoff_pm) {
#       break
#     }
#   }
#   return(vals)
# }

