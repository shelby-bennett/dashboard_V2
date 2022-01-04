
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

source("working_loadat.R")


#function to take in and prepare proportional data. 
prepareLineagePropData <- function(com_data) {
  sc2bylineage <- data.frame(table(com_data$collection_date, com_data$lineage_PANGO_lineage))
  names(sc2bylineage) <- c("date","lineage","num")
  sc2bylineage = sc2bylineage[!(sc2bylineage$date=="2020"|sc2bylineage$date=="2021"|sc2bylineage$date=="2022"),]
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

#function to take in and prepare variant prop data
prepareVariantPropData <- function(com_data) {
  sc2byvariant <- data.frame(table(com_data$collection_date, com_data$variant))
  names(sc2byvariant) <- c("date","variant","num")
  sc2byvariant = sc2byvariant[!(sc2byvariant$date=="2020"|sc2byvariant$date=="2021"),]
  sc2byvariant$date <- as.Date(sc2byvariant$date, format= "%Y-%m-%d")
  sc2byvariant <- within(sc2byvariant, {
    weeks <- format(floor_date(as.Date(sc2byvariant$date, "%m/%d/%Y"),unit = "week"))
    weeks <- factor(weeks, levels = unique(weeks))
    
    months <- format(date, "%B-%Y")
    months <- factor(months, levels = unique(months))
    
    quarters <- paste(quarters(date), format(date, "%Y"), sep = "-")
    quarters <- factor(quarters, levels = unique(quarters))
  })
  sc2byvariant <- sc2byvariant[!is.na(sc2byvariant$date),]
  sc2byvariant <- droplevels(sc2byvariant)
  sc2byvariant <- sc2byvariant %>% mutate_at(c("date","quarters","months","weeks","variant"), as.character())
  
  varWeekly <- group_by_at(sc2byvariant,vars(weeks,variant)) %>% 
    summarise(.groups="keep",num = sum(num))
  names(varWeekly) <- c("date","variant","num")
  varMonthly <- group_by_at(sc2byvariant,vars(months,variant)) %>% 
    summarise(.groups="keep",num = sum(num))
  names(varMonthly) <- c("date","variant","num")
  varQuarterly <- group_by_at(sc2byvariant,vars(quarters,variant)) %>% 
    summarise(.groups="keep",num = sum(num))
  names(varQuarterly) <- c("date","variant","num")
  
  return(list("weekly"=varWeekly,"monthly"=varMonthly,"quarterly"=varQuarterly))
}


##Data prepertation for ALL data##
gis_prep_lin = prepareLineagePropData(final_g_data_var2)  #calling function to prepare all data with proportion of lineages

gis_week_lin = as_tibble(gis_prep_lin$weekly)         # prep a weekly prop for lins as a tibble
gis_monthly_lin = as_tibble(gis_prep_lin$monthly)     # prep a monthly prop for lins as a tibble
gis_quarterly_lin = as_tibble(gis_prep_lin$quarterly) # prep a quarterly prop for lins as a tibble

gis_prep_var = prepareVariantPropData(final_g_data_var2)

gis_week_var = as_tibble(gis_prep_var$weekly)
gis_monthly_var = as_tibble(gis_prep_var$monthly)
gis_quarterly_var = as_tibble(gis_prep_var$quarterly)
###Figure preperation for ALL current GISAID data###
##PANGO LINEAGE DATA
#Weekly(pango lineages)
g_weekfig_lin = ggplot(data = gis_week_lin, aes(x = date, y = num,fill = lineage)) +              
  geom_bar(position = "fill", stat = 'identity' )  +
  coord_cartesian(clip = "off") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = wes_palette(n=139, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0.95,vjust = 0.2)) + 
  labs(x = "Week", y = "Proportion", title = "Proportion of Variants in Virginia by week", 
       caption = "This chart includes all currently submitted sequences in Virginia as of 01/04/22",colour = "Lineage")

#Monthly(pango lineages)
g_monthfig_lin = ggplot(data = gis_monthly_lin, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=139, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "Month", y = "Proportion", title = "Proportion of Variants in Virginia by month", colour = "Lineage")

#Quarterly (pango lineages)
g_quartfig_lin = ggplot(data = gis_quarterly_lin, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=13, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "quarter", y = "Proportion", title = "Proportion of Variants in Virginia by quarter", colour = "Lineage")
##WHO VARIANT
#weekly (who vars)
g_weekfig_var = ggplot(data = gis_week_var, aes(x = date, y = num,fill = variant)) +              
  geom_bar(position = "fill", stat = 'identity' )  +
  coord_cartesian(clip = "off") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = wes_palette(n=13, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0.95,vjust = 0.2)) + 
  labs(x = "Week", y = "Proportion", title = "Proportion of Variants in Virginia by week", 
       caption = "This chart includes all currently submitted sequences in Virginia as of 01/04/22",colour = "Variant")

#Monthly(who vars)
g_monthfig_var = ggplot(data = gis_monthly_var, aes(x = date, y = num,fill = variant)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=13, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "Month", y = "Proportion", title = "Proportion of Variants in Virginia by month", colour = "Variant")

#Quarterly (who vars)
g_quartfig_var = ggplot(data = gis_quarterly_var, aes(x = date, y = num,fill = variant)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=13, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "quarter", y = "Proportion", title = "Proportion of Variants in Virginia by quarter", colour = "Variant")

##Data preparation for ONLY DCLS data##

dcls_prep_lin = prepareLineagePropData(final_d_data_var2)

dcls_week_lin = as_tibble(dcls_prep_lin$weekly)
dcls_month_lin = as_tibble(dcls_prep_lin$monthly)
dcls_quarterly_lin = as_tibble(dcls_prep_lin$quarterly)

dcls_prep_var = prepareVariantPropData(final_d_data_var2)

dcls_week_var = as_tibble(dcls_prep_var$weekly)
dcls_month_var = as_tibble(dcls_prep_var$monthly)
dcls_quarterly_var = as_tibble(dcls_prep_var$quarterly)



###Figure Preparation for only DCLS data##

#Weekly 
d_weekfig_lin =ggplot(data = dcls_week_lin, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity' )  +
  coord_cartesian(clip = "off") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = wes_palette(n=139, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0.95,vjust = 0.2)) + 
  labs(x = "Week", y = "Proportion", title = "Proportion of Variants in Virginia by week", 
       caption = "This chart includes all sequences submitted by DCLS in Virginia as of 01/04/22",colour = "Lineage")

#Monthly 
d_monthfig_lin = ggplot(data = dcls_month_lin, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=139, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "Month", y = "Proportion", title = "Proportion of Variants in Virginia by month", colour = "Lineage")

#Quarterly 
d_quartfig_lin = ggplot(data = dcls_quarterly_lin, aes(x = date, y = num,fill = lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=139, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "quarter", y = "Proportion", title = "Proportion of Variants in Virginia by quarter", colour = "Lineage")
##WHO VARIANTS
#Weekly 
d_weekfig_var =ggplot(data = dcls_week_var, aes(x = date, y = num,fill = variant)) + 
  geom_bar(position = "fill", stat = 'identity' )  +
  coord_cartesian(clip = "off") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = wes_palette(n=13, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0.95,vjust = 0.2)) + 
  labs(x = "Week", y = "Proportion", title = "Proportion of Variants in Virginia by week", 
       caption = "This chart includes all sequences submitted by DCLS in Virginia as of 01/04/22",colour = "Variant")

#Monthly 
d_monthfig_var = ggplot(data = dcls_month_var, aes(x = date, y = num,fill = variant)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=13, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "Month", y = "Proportion", title = "Proportion of Variants in Virginia by month", colour = "Variant")

#Quarterly 
d_quartfig_var = ggplot(data = dcls_quarterly_var, aes(x = date, y = num,fill = variant)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=13, name = "Darjeeling1", type = "continuous")) + 
  theme(axis.text.x = element_text(angle = -90)) + 
  labs(x = "quarter", y = "Proportion", title = "Proportion of Variants in Virginia by quarter", colour = "Variant")


