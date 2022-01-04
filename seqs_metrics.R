library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(wesanderson)
library(lubridate)
library(sqldf)



source("working_loadat.R")

##Function to grap cumulative sequences##
cumulativeSequences <- function(sc2Data){
  # load SC2 data
  sc2byDate <- data.frame(table(sc2Data$collection_date))
  names(sc2byDate) <- c("date","num")
  sc2byDate <- sc2byDate[!(sc2byDate$date=="2020"|sc2byDate$date=="2021"|sc2byDate$date=="2022"),]
  
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
g_seqs = cumulativeSequences(final_g_data_var2)       #all sequences in gisaid

##Calling Cumulative Function for ONLY DCLS data##

d_seqs = cumulativeSequences(final_dg_datavar2)       #DCLS sequences in gisaid



##List of current (as of 01/04/22) VOCs##

voc_list = VOC_list
who_voc = WHO_VOC

#Function for figure prep of VOCs (lineage)
plotVOC <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$collection_date,sc2Data$lineage_PANGO_lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"|sc2bylineage$date=="2022"),]
  
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

plotVOC_who <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$collection_date,sc2Data$variant))
  names(sc2bylineage) <- c("date","variant","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"|sc2bylineage$date=="2022"),]
  
  fig <- plot_ly()
  for(voc in who_voc){
    data <- data.frame(date="2020-01-01",variant=voc,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$variant == voc,])
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

##List of current (as of 01/04/22) VBMs##

vbm_list = VBM_list
vbm_who = WHO_VBM



##Function for figure preperation of VBMs (lineage)##
plotVBM <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$collection_date,sc2Data$lineage_PANGO_lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"|sc2bylineage$date=="2022"),]
  
  fig <- plot_ly()
  for(vbm in vbm_list){
    data <- data.frame(date="2020-01-01",lineage=vbm,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$lineage == vbm,])
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$date, format= "%Y-%m-%d"),
      y = cumsum(data$num),
      name = vbm,
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

plotVBM_who <- function(sc2Data){
  sc2bylineage <- data.frame(table(sc2Data$collection_date,sc2Data$variant))
  names(sc2bylineage) <- c("date","variant","num")
  sc2bylineage <- sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"|sc2bylineage$date=="2022"),]
  
  fig <- plot_ly()
  for(vbm in vbm_who){
    data <- data.frame(date="2020-01-01",variant=vbm,num=0)
    data <- rbind(data,sc2bylineage[sc2bylineage$variant == vbm,])
    fig <- fig %>% add_trace(
      type = "scatter",
      x = as.Date(data$date, format= "%Y-%m-%d"),
      y = cumsum(data$num),
      name = vbm,
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


##Figure creation for VOCs and VBMs##


gisvoc = plotVOC(final_g_data_var2)
gisvbm = plotVBM(final_g_data_var2)
gisvoc_who = plotVOC_who(final_g_data_var2)
gisvbm_who = plotVBM_who(final_g_data_var2)

dvoc = plotVOC(final_d_data_var2)
dvbm = plotVBM(final_d_data_var2)
dvoc_who = plotVOC_who(final_d_data_var2)
dvbm_who = plotVBM_who(final_d_data_var2)
