library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(wesanderson)
library(lubridate)
library(sqldf)
library(readxl)
library(leaflet)
# hltrg = list(
#   "Northern" = c("loudoun", "fairfax", "arlington", "alexandria", "prince william"),
#   "Northwest" = c("lord fairfax", "rappahannock rapidan","rappahannock", "central shenandoah", "thomas jefferson"),
#   "Central" = c("chickahominy", "henrico", "richmond", "chesterfield", "crater", "piedmont"),
#   "Eastern" = c("three rivers", "eastern shore", "peninsula", "hampton", "western tidewater", "norfolk", 
#                 "portsmouth", "chesapeake", "virginia beach" ),
#   "Southwest" = c("lenowisco", "cumberland plateau", "new river", "mount rogers", "west piedmont", "roanoke", "alleghany",
#                   "central virginia", "pittsylvania-danville", "southside")
# )


joined_locdata = cleaned_subs %>% 
  select(seq_id, collection_date, city_county = `City/County`, health_district = `Health District`, health_region = `Health Region`, 
         lineage_PANGO_lineage) %>% 
  drop_na(health_district)


past_60 = joined_locdata %>%
  filter(collection_date >= Sys.Date()-60) #grap data from the pas 60 days of current date

dis_data = past_60 %>% 
  group_by(health_district) %>% 
  count(lineage_PANGO_lineage)
dis_data %>% 
  group_by(health_district,lineage_PANGO_lineage) %>% 
  summarize(num = sum(n))
  
dis_fig = ggplotly(ggplot(data = dis_data, aes(x =health_district , y = n,fill = lineage_PANGO_lineage)) + 
  geom_bar(position = "fill", stat = 'identity') + 
  scale_fill_manual(values = wes_palette(n=20, name = "Darjeeling1", type = "continuous")) + 
  labs(x = "Health District", y = "Proportion", title = "Proportion of Variants in Virginia by Health District within the last 60 days", colour = "Lineage") + 
  theme(axis.text.x = element_text(angle = -90)))


###proportion of variants by health region within the last 60 days

reg_data = past_60 %>% 
  group_by(health_region) %>% 
  count(lineage_PANGO_lineage)
reg_data %>% 
  group_by(health_region,lineage_PANGO_lineage) %>% 
  summarize(num = sum(n))

reg_fig = ggplotly(ggplot(data = reg_data, aes(x= health_region , y = n,fill = lineage_PANGO_lineage)) + 
                     geom_bar(position = "fill", stat = 'identity') + 
                     scale_fill_manual(values = wes_palette(n=20, name = "Darjeeling1", type = "continuous")) + 
                     labs(x = "Health Region", y = "Proportion", title = "Proportion of Variants in Virginia by Health Regions within the last 60 days", colour = "Lineage") + 
                     theme(axis.text.x = element_text(angle = -90)))
                           
                           


###loading and preparing Geojson file

#  vdh_districts = geojson_sf("VDH_Health_Districts.geojson")
# # 
# # leaflet(vdh_districts) %>% 
# #   addPolygons(
# #     stroke = TRUE, 
# #     color = 'White', 
# #     weight = 1.5, 
# #   )
# # my_labels = paste(
# #   "Health District: ", vdh_districts$vdh_hd, "<br/>",
# #   "B.1.1.7: ", reg_data$lineage
# # )
# # 
# 
# vdis_data = joined_locdata %>% 
#   group_by(health_district) %>% 
#   count(lineage_PANGO_lineage) %>% 
#   select(vdh_hd = health_district, lineage_PANGO_lineage, n)
# 
#   
# vdis_data %>% 
#   group_by(vdh_hd,lineage_PANGO_lineage) %>% 
#   summarize(num = sum(n))
# 
# 
# all_vdis = vdis_data %>% 
#   group_by(vdh_hd) %>% 
#   summarise(totalvars = sum(n))
# 
# 
# 
# B117 = vdis_data %>% 
#   filter(lineage_PANGO_lineage == "B.1.1.7") %>% 
#   group_by(vdh_hd,lineage_PANGO_lineage) %>% 
#   summarize(num = sum(n))
# 
# B1351 = vdis_data %>% 
#   filter(lineage_PANGO_lineage == "B.1.351") %>% 
#   group_by(vdh_hd,lineage_PANGO_lineage) %>% 
#   summarize(num = sum(n)) 
# 
# 
# B16172 = vdis_data %>% 
#   #group_by(lineage_PANGO_lineage) %>% 
#   filter(lineage_PANGO_lineage == "B.1.617.2") %>% 
#   group_by(vdh_hd,lineage_PANGO_lineage) %>% 
#   summarize(num = sum(n))
#   
# P1 = vdis_data %>% 
#   filter(lineage_PANGO_lineage == "P.1") %>% 
#   group_by(vdh_hd,lineage_PANGO_lineage) %>% 
#   summarize(num = sum(n))
# 
# pal_fun = colorQuantile("YlOrRd", NULL, n = 5)
# 
# mergevdh = inner_join(vdh_districts,all_vdis, by ="vdh_hd" )
# 
# 
# testmerge = full_join(mergevdh, B117, by = "vdh_hd")
# testmerge = testmerge %>%
#   rename(B117 = num)
# 
# testmerge2 = full_join(testmerge, B1351, by ="vdh_hd")
# testmerge2 = testmerge2 %>%
#   rename(B1351 = num)
#   
# 
# testmerge3 = full_join(testmerge2, B16172, by ="vdh_hd")
# testmerge3 = testmerge3 %>%
#   rename(B16172 = num)
# 
# testmerge4 = full_join(testmerge3, P1, by = "vdh_hd")
# testmerge4 = testmerge4 %>%
#   rename(P1 = num)
# 
# 
# 
# 
# p_popup = with(testmerge4, paste("Health District : ", testmerge4$vdh_hd, '<br>',
#                                     "B.1.1.7: ", testmerge4$B117, '<br>',
#                                     "B.1.617.2: ", testmerge4$B16172, '<br>',
#                                     "B.1.351: ", testmerge4$B1351, '<br>',
#                                     "P1: ", testmerge4$P1, '<br>'))
# 
# plotHD = leaflet(testmerge4) %>% 
#   addPolygons(
#     stroke = TRUE,
#     fillColor = ~pal_fun(totalvars),
#     fillOpacity = 1, smoothFactor = 0.5,
#     popup = p_popup) %>% 
#   addTiles() %>%  
#   addLegend("bottomright",
#             pal = pal_fun,
#             values= ~totalvars,
#             title = 'percent sequenced')
