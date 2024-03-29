library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(wesanderson)
library(lubridate)
library(sqldf)
library(stringr)
#library(outbreakinfo)
###read in raw data

raw_d_data = read_csv("updatedv1216.csv", col_types = cols_only(taxon = col_guess(), lineage = col_guess()))

raw_mstrlist = read_csv("mstr_lst122821.csv", col_types = cols_only(seq_id = col_guess(),
                                                                  r_num = col_guess(),
                                                                  collection_date = col_date(format = "%m/%d/%Y"),
                                                                  `City/County` = col_guess(), `Health District` = col_guess(),
                                                                  `Health Region` = col_guess()))

raw_g_data =read_csv("gisaid_122921.csv", col_types = cols_only(`Virus name` = col_guess(), 
                                                             `Collection date` = col_date(format = "%m/%d/%Y"), 
                                                             Lineage = col_guess()))
#####################################################################################################################
### DATA CLEAN UP ###
form_d_data = raw_d_data %>% 
  filter(str_detect(taxon, "DCLS")) %>% 
  mutate(taxon = gsub('_.*',"",taxon)) %>% 
  select(seq_id = taxon, lineage_PANGO_lineage = lineage)
combd_dat = full_join(raw_mstrlist, form_d_data, by = "seq_id")

#seperating DCLS GISAID data from all other submitters
form_g_data = raw_g_data %>% 
  select(seq_id = `Virus name` ,collection_date = `Collection date`, lineage_PANGO_lineage = "Lineage") %>% 
  filter(str_detect(seq_id, "DCLS"))

#all gisaid data  
all_gdata = raw_g_data %>% 
  select(seq_id = `Virus name` ,collection_date = `Collection date`, lineage_PANGO_lineage = "Lineage")


####################################################################################################################
## Establish some static lineage information ##

#grab all of the possible AYs for each tibble

all_gay = all_gdata[grepl("AY", all_gdata$lineage_PANGO_lineage),]
all_gay = all_gay[-2]
all_day = form_d_data[grepl("AY", form_d_data$lineage_PANGO_lineage),]
all_ay = rbind(all_gay,all_day)

list_ay = unique(all_ay$lineage_PANGO_lineage)


#current list of lineages based on WHO Nomenclature

who_variants = list(
  'Alpha' = c("B.1.1.7","Q.1","Q.2","Q.3","Q.4","Q.5","Q.6",
              "Q.7","Q.8"),
  'Beta' = c("B.1.351","B.1.351.2","B.1.351.3"),
  'Gamma' = c("P.1","P.1.1","P.1.2"),
  'Delta' = c("B.1.617.2", list_ay),
  'Epsilon' = c("B.1.427","B.1.429"),
  'Eta' = c("B.1.525"),
  'Iota' = c("B.1.526"),
  'Kappa' = c("B.1.617.1"),
  'Lambda' = c("C.37","C.37.1"),
  'Mu' = c("B.1.621","B.1.621.1"),
  'Zeta' = c("P.2"),
  'Omicron' = c("B.1.1.529","BA.1", "BA.2")
)
whovar_df = map_df(who_variants, ~as.data.frame(.x), .id="variant")
names(whovar_df)[2] = "lineage_PANGO_lineage"

WHO_VOC = c(
  'Delta',
  'Omicron'
)

WHO_VBM = c(
  'Alpha',
  'Beta',
  'Gamma',
  'Epsilon',
  'Zeta',
  'Eta',
  'Iota',
  'Kappa',
  'Lambda',
  'Mu'
)

VOC_list = c(who_variants$Delta,who_variants$Omicron)

VBM_list = c("B.1.617.3", who_variants$Alpha, who_variants$Beta, who_variants$Gamma,
               who_variants$Epsilon, who_variants$Eta, who_variants$Iota, who_variants$Kappa,
               who_variants$Mu, who_variants$Zeta)

########################################################################################################################
##final clean up
#only DCLS data
final_d_data_var = combd_dat %>% 
  drop_na(r_num,lineage_PANGO_lineage,collection_date) %>% 
  distinct(seq_id, .keep_all = TRUE) %>% 
  group_by(lineage_PANGO_lineage) %>%
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% unlist(who_variants, use.names = F), 
                                        lineage_PANGO_lineage,"Other"))
final_d_data_var2 = left_join(final_d_data_var, whovar_df, by = "lineage_PANGO_lineage")
final_d_data_var2$variant = final_d_data_var2$variant %>% 
  replace_na("Other")
#Only DCLS data in GISAID
final_dg_data = form_g_data %>% 
  distinct(seq_id, .keep_all = TRUE) %>% 
  group_by(lineage_PANGO_lineage) %>%
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% unlist(who_variants, use.names = F), 
                                        lineage_PANGO_lineage,"Other"))
final_dg_datavar2 = left_join(final_dg_data,whovar_df, by = "lineage_PANGO_lineage")
final_dg_datavar2$variant = final_dg_datavar2$variant %>% 
  replace_na("Other")  

#All GISAID data
final_g_data_var = all_gdata %>% 
  distinct(seq_id, .keep_all = TRUE) %>% 
  group_by(lineage_PANGO_lineage) %>%
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% unlist(who_variants, use.names = F), 
                                        lineage_PANGO_lineage,"Other"))
final_g_data_var2 = left_join(final_g_data_var, whovar_df, by = "lineage_PANGO_lineage")
final_g_data_var2$variant = final_g_data_var2$variant %>% 
  replace_na("Other")
  
