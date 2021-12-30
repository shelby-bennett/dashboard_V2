library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(wesanderson)
library(lubridate)
library(sqldf)



source("load_dat.R")

##Function to grap cumulative sequences##
cumulativeSequences <- function(sc2Data){
  # load SC2 data
  sc2byDate <- data.frame(table(sc2Data$collection_date))
  names(sc2byDate) <- c("date","num")
  sc2byDate <- sc2byDate[!(sc2byDate$date=="2020"|sc2byDate$date=="2021"),]
  
  fig <- plot_ly()
  # total number
  fig <- fig %>% add_trace(
    type = "scatter",
    x = as.Date(sc2byDate$date, format= "%Y-%m-%d"),
    y = cumsum(sc2byDate$num),
    name = 'Total',
    mode = "lines"
  )
  fig <- fig %>%
    layout(
      xaxis = list(
        type = "date",
        range=c('2020-01-01', format(Sys.Date(),"%Y-%m-%d"))
      ),
      hovermode = 'compare',
      autosize=TRUE
    )
  return(fig)
}




##Calling Cumulative function for ALL data##
g_seqs = cumulativeSequences(gisaid_dat)

##Calling Cumulative Function for ONLY DCLS data##

d_seqs = cumulativeSequences(dcls_data)



##List of current (as of 07/28/21) VOCs##

voc_list = c(
  "B.1.1.7",
  "B.1.351",
  "P.1",
  "B.1.617.2"
)

#Function for figure prep of VOCs
plotVOC <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$collection_date,sc2Data$lineage_PANGO_lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  
  fig <- plot_ly()
  for(voc in voc_list){
    data <- data.frame(date="2020-01-01",lineage=voc,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$lineage == voc,])
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$date, format= "%Y-%m-%d"),
      y = cumsum(data$num),
      name = voc,
      mode = "lines"
    )
  }
  
  fig <- fig %>%
    layout(
      xaxis = list(
        type = "date",
        range=c('2020-01-01', format(Sys.Date(),"%Y-%m-%d"))
      ),
      hovermode = 'compare'
    )
  return(fig)
}

##List of current (as of 07/28/21) VOIs##

voi_list = c(
  "B.1.525",
  "B.1.526",
  "B.1.617.1",
  "B.1.617.3"
)



##Function for figure preperation of VOIs##
plotVOI <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$collection_date,sc2Data$lineage_PANGO_lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  
  fig <- plot_ly()
  for(voi in voi_list){
    data <- data.frame(date="2020-01-01",lineage=voi,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$lineage == voi,])
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$date, format= "%Y-%m-%d"),
      y = cumsum(data$num),
      name = voi,
      mode = "lines"
    )
  }
  
  fig <- fig %>%
    layout(
      xaxis = list(
        type = "date",
        range=c('2020-01-01', format(Sys.Date(),"%Y-%m-%d"))
      ),
      hovermode = 'compare'
    )
  return(fig)
}




##Figure creation for VOCs and VOIs##


allvoc = plotVOC(gisaid_dat)
allvoi = plotVOI(gisaid_dat)

dvoc = plotVOC(dcls_data)
dvoi = plotVOI(dcls_data)

