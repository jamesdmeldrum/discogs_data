#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library("rnaturalearth")
library("rnaturalearthdata")

print("Reading data.")
if (!(exists("df") & is.data.frame(df))) {
  df <- read.csv("../../dat_with_years_reduced.csv")
  print("Data read.")
} else {
  print("Data already loaded, how good")
}


reduce_by_year <- function(dat, start_year, end_year) {
  return(filter(dat, year >= start_year, year <= end_year))
}

breaks_maker <- function(nums) {
  nums <- nums[order(nums, decreasing = FALSE)]
  breaks <- c()
  breaks[1] <- nums[1]
  breaks[2] <- nums[round(length(nums)/2)]
  breaks[3] <- nums[length(nums)]
  return(breaks)
}


plot_countries <- function(dat, start_year, end_year) {
  print("Begun plotting.")
  dat <- reduce_by_year(dat, start_year, end_year)
  dat[dat == "United Kingdom"] <- "UK"
  dat[dat == "United States of America"] <- "USA"
  dat[dat == "US"] <- "USA"
  country_counts <- dat %>%
    group_by(country) %>%
    summarise(counts = n())
  country_counts <- country_counts[order(country_counts$counts, decreasing = TRUE),]
  
  country_formats <- dat %>%
    group_by(country) %>%
    count(country, format_) %>%
    slice(which.max(n))
  # head(country_counts)
  
  country_dat <- data.frame(region = as.character(),
                            count = as.numeric(),
                            Format = as.character())
  
  mapdata <- map_data("world")
  
  for (cntry in unique(mapdata$region)) {
    count_row <- filter(country_counts, country == cntry)
    format_row <- filter(country_formats, country == cntry)
    if (nrow(count_row) == 0) {
      num <- 0
      form <- NA
    } else {
      num <- count_row$counts
      form <- format_row$format_
    }
    country_dat[nrow(country_dat)+1,] <- c(cntry, num, form)
  }
  
  country_dat$count <- as.numeric(country_dat$count)
  country_dat$Format <- as.factor(country_dat$Format)
  
  country_dat <- country_dat[order(country_dat$count, decreasing = T),]
  
  texts <- c()
  for (i in 1:nrow(country_dat)) {
    country_name <- country_dat[i,]$region
    count <- country_dat[i,]$count
    Format <- country_dat[i,]$Format
    
    text = paste(paste(paste(paste(country_name, "Count:", sep = "\n"), count, sep = " "), '\nMajor Format:'), Format, sep = ' ')
    texts[length(texts)+1] <- text
  }
  
  country_dat$texts <- texts
  
  mapdata <- left_join(mapdata, country_dat, by = "region")
  mapdata <- filter(mapdata, count != 0)
  
  subtitle <- paste(paste(start_year, " - ", sep = ""), end_year, sep = "")
  
  mapplot <- ggplot(mapdata, aes(x = long, y = lat, group = group, text = texts)) +
    geom_polygon(aes(fill = count, colour = Format)) +
    scale_fill_viridis_c(option ="plasma", name = "Number of Releases", trans = "log", breaks = breaks_maker(mapdata$count)) +
    labs(title = subtitle,
         subtitle = subtitle) +
    # xlab("Longitude") +
    # ylab("Latitude") +
    theme_economist() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  interactive <- plotly::ggplotly(mapplot)
  print("Finished plot.")
  return(interactive)
}

mapit <- function(map) {
  print("Begun rendering.")
  out <- tryCatch(
    {
      for (i in 1:length(map$x$data)) {
        text_pre <- map$x$data[[i]]$text
        text_post <- strsplit(text_pre, "<")[[1]][1]
        map$x$data[[i]]$text <- text_post
      }
    }, error=function(cond) {
      return(map)
    }
  )
  print("Rendered")
  return(map)
}

load_data <- function(start_year, end_year) {
  
  print("Reached loading section.")
  mapit(plot_countries(df, start_year, end_year))
}

print("Reached shinyserver function.")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  hide("loading_page")
  show("main_content")

    output$distPlot <- renderPlotly({
      
      print("reached here")

        start_year <- input$start_year
        end_year <- input$end_year
        
        print("also reached here")

        # draw the histogram with the specified number of bins
        load_data(start_year, end_year)

    })

})
