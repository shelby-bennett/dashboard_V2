
#Variant report metrics

library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(wesanderson)
library(lubridate)
library(sqldf)
library(aweek)
library(tidyr)

source("load_dat.R")


#function to take in and prepare proportional data. 
prepareLineagePropData <- function(com_data) {
  sc2bylineage <- data.frame(table(com_data$collection_date, com_data$lineage_PANGO_lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage = sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"),]
  sc2bylineage$date <- as.Date(sc2bylineage$date, format= "%Y-%m-%d")
  sc2bylineage <- within(sc2bylineage, {
    weeks <- format(floor_date(as.Date(sc2bylineage$date, "%m/%d/%Y"),unit = "week"))
    weeks <- factor(weeks, levels = unique(weeks))
    
    months <- format(date, "%B-%Y")
    months <- factor(months, levels = unique(months))
    
    quarters <- paste(quarters(date), format(date, "%Y"), sep = "-")
    quarters <- factor(quarters, levels = unique(quarters))
  })
  sc2bylineage <- sc2bylineage[!is.na(sc2bylineage$date),]
  sc2bylineage <- droplevels(sc2bylineage)
  sc2bylineage <- sc2bylineage %>% mutate_at(c("date","quarters","months","weeks","lineage"), as.character())
  
  varWeekly <- group_by_at(sc2bylineage,vars(weeks,lineage)) %>% 
    summarise(.groups="keep",num = sum(num))
  names(varWeekly) <- c("date","lineage","num")
  varMonthly <- group_by_at(sc2bylineage,vars(months,lineage)) %>% 
    summarise(.groups="keep",num = sum(num))
  names(varMonthly) <- c("date","lineage","num")
  varQuarterly <- group_by_at(sc2bylineage,vars(quarters,lineage)) %>% 
    summarise(.groups="keep",num = sum(num))
  names(varQuarterly) <- c("date","lineage","num")
  
  return(list("weekly"=varWeekly,"monthly"=varMonthly,"quarterly"=varQuarterly))
}

##Data prepertation for ALL data##
gis_prep = prepareLineagePropData(gisaid_dat)  #calling function to prepare all data with proportion of lineages

gis_week = as_tibble(gis_prep$weekly)         # prep a weekly prop as a tibble
gis_monthly = as_tibble(gis_prep$monthly)     # prep a monthly prop as a tibble
gis_quarterly = as_tibble(gis_prep$quarterly) # prep a quarterly prop as a tibble

##Figure preperation for ALL current GISAID data ##

#Weekly
g_weekfig = ggplot(data = gis_week, aes(x = date, y = num,fill = lineage)) +              
  geom_bar(position = "fill", stat = 'identity' )  +
  coord_cartesian(clip = "off") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = wes_palette(n=100, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0.95,vjust = 0.2)) + 
  labs(x = "Week", y = "Proportion", title = "Proportion of Variants in Virginia by week", 
       caption = "This chart includes all currently submitted sequences in Virginia as of 08/11/21",colour = "Lineage")

#Monthly
g_monthfig = ggplot(data = gis_monthly, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=100, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "Month", y = "Proportion", title = "Proportion of Variants in Virginia by month", colour = "Lineage")

#Quarterly 
g_quartfig = ggplot(data = gis_quarterly, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=100, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "quarter", y = "Proportion", title = "Proportion of Variants in Virginia by quarter", colour = "Lineage")


##Data preperation for ONLY DCLS data##

dcls_prep = prepareLineagePropData(dcls_data)

dcls_week = as_tibble(dcls_prep$weekly)
dcls_month = as_tibble(dcls_prep$monthly)
dcls_quarterly = as_tibble(dcls_prep$quarterly)


  

##Figure Preperation for only DCLS data##
#dcls_week$date = mdy("Mar 6 2020")
  
#Weekly 
d_weekfig =ggplot(data = dcls_week, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity' )  +
  coord_cartesian(clip = "off") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = wes_palette(n=100, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0.95,vjust = 0.2)) + 
  labs(x = "Week", y = "Proportion", title = "Proportion of Variants in Virginia by week", 
       caption = "This chart includes all sequences submitted by DCLS in Virginia as of 08/11/21",colour = "Lineage")

#Monthly 
d_monthfig = ggplot(data = dcls_month, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=100, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "Month", y = "Proportion", title = "Proportion of Variants in Virginia by month", colour = "Lineage")

#Quarterly 
d_quartfig = ggplot(data = gis_quarterly, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=100, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "quarter", y = "Proportion", title = "Proportion of Variants in Virginia by quarter", colour = "Lineage")