library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(wesanderson)
library(lubridate)
library(sqldf)
library(stringr)




raw_gdata <- read_tsv("gisaid_seq_tech.ts", col_types = cols_only(`Collection date` = col_date(),   #loading in ALL data
                                                                 Lineage = col_guess(), `Virus name` = col_guess()))


raw_d_data = read_csv("dcls_1123.csv", col_types = cols_only(taxon = col_guess(), lineage = col_guess()))

raw_mstrlist = read_csv("mstrlst_1123.csv", col_types = cols_only(seq_id = col_guess(),
                                                                              r_num = col_guess(),
                                                                                collection_date = col_date(format = "%m/%d/%Y"),
                                                                                `City/County` = col_guess(), `Health District` = col_guess(),
                                                                                `Health Region` = col_guess()))

form_d_data = raw_d_data %>% 
  filter(str_detect(taxon, "DCLS")) %>% 
  mutate(taxon = gsub('_.*',"",taxon)) %>% 
  select(seq_id = taxon, lineage_PANGO_lineage = lineage)

combd_dat = full_join(raw_mstrlist, form_d_data, by = "seq_id")

alpha_subs = c("Q.3", "Q.4", "Q.8")
beta_subs = c("B.1.351.1", "B.1.351.2", "B.1.351.3")
gamma_subs = c("P.1", "P.1.2", "P.1.10", "P.1.7")

cleaned_subs = combd_dat %>% 
  group_by(lineage_PANGO_lineage) %>% 
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% alpha_subs, "B.1.1.7", lineage_PANGO_lineage)) %>%
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% gamma_subs, "P.1", lineage_PANGO_lineage)) %>% 
  drop_na(r_num,lineage_PANGO_lineage) %>% 
  distinct(seq_id, .keep_all = TRUE) %>% 
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage != "B.1.1.7" & lineage_PANGO_lineage != "B.1.351" &
                                          lineage_PANGO_lineage != "B.1.617.2" & lineage_PANGO_lineage != "B.1.617.1" &
                                          lineage_PANGO_lineage != "B.1.525" & lineage_PANGO_lineage != "B.1.526" &
                                          lineage_PANGO_lineage != "P.1" & lineage_PANGO_lineage != "B.1.617.3" & 
                                          lineage_PANGO_lineage != "P.2" & lineage_PANGO_lineage != "B.1.427" &
                                          lineage_PANGO_lineage != "B.1.429" & lineage_PANGO_lineage != "B.1.621" & 
                                          lineage_PANGO_lineage != "B.1.621.1" & lineage_PANGO_lineage != "B.1.1.529" & 
                                          !str_detect(lineage_PANGO_lineage,"AY"),"Other", lineage_PANGO_lineage))
 
dcls_data = cleaned_subs

gisaid_dat = raw_gdata %>% 
  select(virus_name = `Virus name` ,collection_date = `Collection date`, lineage_PANGO_lineage = "Lineage") %>% 
  filter(str_detect(virus_name,"DCLS")) %>% 
  group_by(lineage_PANGO_lineage) %>% 
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% alpha_subs, "B.1.1.7", lineage_PANGO_lineage)) %>%
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% delta_subs, "B.1.617.2", lineage_PANGO_lineage)) %>% 
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage %in% gamma_subs, "P.1", lineage_PANGO_lineage)) %>% 
  drop_na(collection_date) %>% 
  distinct(virus_name, .keep_all = TRUE) %>% 
  mutate(lineage_PANGO_lineage = ifelse(lineage_PANGO_lineage != "B.1.1.7" & lineage_PANGO_lineage != "B.1.351" &
                                          lineage_PANGO_lineage != "B.1.617.2" & lineage_PANGO_lineage != "B.1.617.1" &
                                          lineage_PANGO_lineage != "B.1.525" & lineage_PANGO_lineage != "B.1.526" &
                                          lineage_PANGO_lineage != "P.1" & lineage_PANGO_lineage != "B.1.617.3" & 
                                          lineage_PANGO_lineage != "P.2" & lineage_PANGO_lineage != "B.1.427" &
                                          lineage_PANGO_lineage != "B.1.429" & lineage_PANGO_lineage != "B.1.621" & 
                                          lineage_PANGO_lineage != "B.1.621.1" & lineage_PANGO_lineage != "B.1.1.529" & 
                                          !str_detect(lineage_PANGO_lineage,"AY"),"Other", lineage_PANGO_lineage))
ay_subs = cleaned_subs %>% 
  filter(str_detect(lineage_PANGO_lineage, "AY")) %>%
  group_by(lineage_PANGO_lineage) %>% 
  select(lineage_PANGO_lineage) %>% 
  distinct(lineage_PANGO_lineage)
  