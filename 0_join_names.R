pacman::p_load(tidyverse,magrittr,ggrepel)

url <- "https://wahlen.admin.ch/ogd/sd-t-17.02-NRW2019-kandidierende-appendix.csv"
results <- read.csv(url, sep = ";", encoding = 'UTF-8', stringsAsFactors = F)


setwd("C:/Users/dermont/Documents/Git/digdemlab/chvote19_accounts")
accounts <- read.csv("2019_chvote_nationalcouncil.csv", fileEncoding = 'UTF-8')

join <- full_join(results %>% 
                    select(name, vorname, kanton_nummer, liste_nummer_kanton) %>% 
                    mutate(fullname = paste0(name, " ", vorname)), 
                  accounts %>% 
                    select(firstname, lastname, district, list) %>% 
                    mutate(fullname = paste0(lastname, " ", firstname)), 
                  by = c("fullname")) %>% 
  select(fullname, name, vorname, lastname, firstname, kanton_nummer, liste_nummer_kanton, district, list) %>% 
  arrange(fullname)

#write.table(join %>% filter(is.na(name) | is.na(lastname)), 
#          file = "191111_join_check.csv", sep = ";", fileEncoding = 'UTF-8', row.names = F)

join_check <- read.csv("191111_join_check.csv", fileEncoding = 'UTF-8', sep = ';')

join <- bind_rows(
  join %>% filter(!is.na(name) & !is.na(lastname)), 
  
  # double, corrected names
  join_check %>% 
    filter(!is.na(lastname)) %>% 
    group_by(lastname, firstname) %>% 
    mutate(nx = n()) %>% 
    filter(nx == 2) %>% 
    filter(!is.na(name)) %>% 
    select(-nx),
  
  # single, smartvote list
  join_check %>% 
    filter(!is.na(lastname)) %>% 
    group_by(lastname, firstname) %>% 
    mutate(nx = n()) %>% 
    filter(nx == 1) %>% 
    select(-nx),
  
  # single, official results
  join_check %>% 
    filter(is.na(lastname))
)

colnames(join) <- c("fullname", "name_bfs", "vorname_bfs", "name_smvt", "vorname_smvt", 
                    "kanton_nummer", "liste_nummer_kanton", "district_smvt", "list_smvt")

write.table(x %>% filter(is.na(name) | is.na(lastname)), 
            file = "191111_join.csv", sep = ";", fileEncoding = 'UTF-8', row.names = F)

###############################################################################
# update first csv file, add the bfs names

accounts %<>% 
  full_join(join %>% select(name_smvt, vorname_smvt, name_bfs, vorname_bfs), 
            by = c("lastname" = "name_smvt", "firstname" = "vorname_smvt"))

write.table(accounts, "2019_chvote_nationalcouncil.csv", sep = ";", fileEncoding = 'UTF-8', row.names = F)

