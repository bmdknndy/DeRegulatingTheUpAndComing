##### Clean Data #####

# Set Up

setwd("~/Desktop/DRTU&C")
library(tidyverse)
library(data.table)
library(GWalkR)
library(janitor)
library(fixest)
library(ggfixest)
library(ggplot2)
library(arsenal)
library(gginnards)

# Load And Clean Raw Data

 
CR <- fread("./data/raw/CSV/CR.csv", col.names = c("QNy", "QNcr","MNy", "MNcr", "BXy", "BXcr","BKy", "BKcr")) 
SV <- fread("./data/raw/CSV/SV.csv", col.names = c("BKy", "BKsv", "MNy", "MNsv", "BXy", "BXsv", "QNy", "QNsv")) 
PPU <- fread("./data/raw/CSV/perupriceNEW.csv", col.names = c("BXy", "BXsv", "BKy", "BKsv", "MNy", "MNsv", "QNy", "QNsv")) 
rentgap <- fread("./data/raw/CSV/rentgap.csv", col.names = c("MRyear","MR","RSyear","RS")) 
Rdates <- fread("./data/raw/CSV/Fed Recession Dates.csv", col.names = c("Date","RecessionOrNot"))
seMARall <- fread("./data/raw/CSV/seMARall.csv") %>% clean_names()

CR_cln <- CR %>%
  mutate_all(~as.numeric(.x)) %>%
  mutate_all(~round(.x, digits = 2)) %>%
  pivot_longer(c("BKy", "MNy", "BXy", "QNy"), names_to =  "boro", values_to = "Year") %>%
  select(-boro) %>%
  pivot_longer(!Year, names_to =  "Borough", values_to = "BoroCapRate") %>%
  filter(!is.na(Year),!is.na(Borough),!is.na(CapRate)) %>%
  distinct() %>%
  group_by(Year) %>%
  mutate(NYCcr = mean(CityCapRate)) %>%
  ungroup() %>%
  write_csv("./data/processed/CSV/CRclean.csv") 

SV_cln <- SV[-c(1, 2),] %>%
  mutate_all(~as.numeric(.x)) %>%
  mutate_all(~round(.x, digits = 2)) %>%
  pivot_longer(c("BKy", "MNy", "BXy", "QNy"), names_to =  "boro", values_to = "Year") %>%
  select(-boro) %>%
  pivot_longer(!Year, names_to =  "Borough", values_to = "BoroSalesVolume") %>%
  filter(!is.na(Year),!is.na(Borough)) %>%
  distinct() %>%
  group_by(as.factor(Year)) %>%
  mutate(CitySalesVolume = sum(BoroSalesVolume)) %>%
  ungroup() %>%
  write_csv("./data/processed/CSV/SVclean.csv") 


PPU_clean <- PPU %>% mutate_all(~as.numeric(.x)) %>%
  mutate_all(~round(.x, digits = 2)) %>%
  pivot_longer(c("BKy", "MNy", "BXy", "QNy"), names_to =  "boro", values_to = "Year") %>%
  select(-boro) %>%
  pivot_longer(!Year, names_to =  "Borough", values_to = "BoroPPU") %>%
  filter(!is.na(Year),!is.na(Borough)) %>%
  distinct() %>%
  group_by(as.factor(Year)) %>%
  mutate(CityPPU = mean(BoroPPU)) %>%
  ungroup() %>%
  write_csv("./data/processed/CSV/PPUclean.csv")

rentgap_cln <- rentgap %>%
  slice_tail(n = -2) %>%
  mutate_all(~as.numeric(.x)) %>%
  mutate_all(~round(.x, digits = 3)) %>%
  pivot_longer(c("MRyear","RSyear"), names_to =  "x", values_to = "Year") %>%
  select(-x) %>%
  pivot_longer(!Year, names_to =  "UnitType", values_to = "MeanRent") %>%
  filter(!is.na(Year),!is.na(MeanRent)) %>%
  distinct() %>%
  mutate(MeanRent = MeanRent*6000) %>%
  write_csv("./data/processed/CSV/MNrentgapclean.csv")

Rdates_cln <- Rdates %>%
  filter(Date >= as.IDate("2005-01-01")) %>%
  write_csv("./data/processed/CSV/Rdates_cln.csv")


SAtoMS <- function(SA, 
                   FE1 = "FE 1", 
                   FE1X = "", 
                   FE2 = "FE 1", 
                   FE2X = "",
                   FE3 = "FE 1", 
                   FE3X = "",
                   title = "", 
                   notes = "", 
                   output = "./SAtoMS.png"){
  
  df <- as.data.frame(coeftable(SA, agg = "att")) 
  
  df <- df %>%
    mutate(term = row.names(df)) %>%
    mutate(term = if_else(term == ".theta","Theta",term))
  
  ti <- data.frame(
    term = df$term,
    estimate = df$Estimate,
    std.error = df$`Std. Error`,
    p.value = df$`Pr(>|t|)`)
  
  gl <- data.frame(FE1 = FE1X,
                   FE2 = FE2X) 
  
  ms <- list(
    tidy = ti,
    glance = gl)  
  
  class(ms) <- "modelsummary_list"
  
  modelsummary(list("Raw IWATT" = ms, "IRR" = ms),
               exponentiate = c(FALSE,TRUE),
               stars = TRUE,
               title = title,
               notes = notes,
               output = output)
}


