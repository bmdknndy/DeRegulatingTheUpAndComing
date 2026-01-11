##### Run Models #####

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


gent_zips_lag <- fread("./data/processed/CSV/gent_zips_311_nova.csv")

gent_zip_lag_zippie <- gent_zips_lag 


NoFixedFX = fepois(n_rsu_cln ~ sunab(gent_year, year, bin.rel=list("-5"=-12:-5,"5"=5:12)), 
                   data = gent_zips_lag,
                   #family = "quasipoisson",
                   #weights = ~weights, 
                   #offset = ~log(unitsres), 
                   vcov = ~bbl)

WithFixedFX = fepois(n_rsu_cln ~ sunab(gent_year, year, bin.rel=list("-5"=-12:-5,"5"=5:12)) | year + bbl, 
                     data = gent_zips_lag, 
                     #family = "quasipoisson", 
                     #weights = ~weights, 
                     #offset = ~log(unitsres),
                     vcov = ~bbl)

BySpec = fepois(n_rsu_cln ~ sunab(gent_year, year, bin.rel=list("-7"=-12:-7,"7"=7:12)) | year + bbl, 
                data = gent_zips_lag,
                #family = "quasipoisson",
                #weights = ~weights,
                #offset = ~log(unitsres), 
                split = ~spec_or_not,
                vcov = ~bbl)

dict = c(
  n_rsu_cln = "nRentStabUnits",
  bbl = "BBL",
  year = "Year",
  boro = "Borough",
  zipcode = "ZipCode",
  spec_or_not = "SpecSaleOrNot",
  "spec_or_not::SpecSale" = "SpecSale",
  "spec_or_not::NoSpecSale" = "NoSpecSale",
  "0" = "NoSpecSale",
  "1" = "SpecSale",
  ATT = "IW ATT"
)


rsu_etable <- etable(NoFixedFX, WithFixedFX, BySpec, 
                     agg = "att", 
                     dict = dict,
                     depvar = FALSE,
                     export = "./exports/Tables/RSUetable.png")

#rsu_etable

rsu_aer_table <- etable(NoFixedFX, WithFixedFX, BySpec, 
                        agg = "att", 
                        dict = dict,
                        depvar = FALSE,
                        tex = TRUE, 
                        style.tex = style.tex('aer'),
                        export = "./exports/Tables/RSUaertable.png")

#rsu_aer_table

RSUfeCE <- as.data.frame(coeftable(WithFixedFX)) 

RSUfeCE_cln <- RSUfeCE %>%
  clean_names() %>%
  mutate(year = row.names(RSUfeCE)) %>%
  mutate(year = as.numeric(str_remove(year, "year::"))) %>%
  mutate(exp_est = exp(estimate)) %>%
  mutate(exp_diff_est = exp_est - 1) 

ref_year <- as.data.frame(list(c(0),c(0),c(0),c(0),c(-1),c(0),c(0)))
colnames(ref_year) <- colnames(RSUfeCE_cln)  

RSUfeCE_clnr <- RSUfeCE_cln %>%
  bind_rows(ref_year) %>%
  write_csv("./data/processed/CSV/RSUfeCE_clnr.csv")

gwalkr(RSUfeCE_clnr)

BSfeCE <- as.data.frame(coeftable(BySpec))

BSfeCEcln <- BSfeCE %>%
  clean_names() %>%
  mutate(year = as.numeric(str_remove(coefficient, "year::"))) %>%
  mutate(exp_est = exp(estimate)) %>%
  mutate(exp_diff_est = exp_est - 1) 


ref_yearNS <- as.data.frame(list(c(1),c("spec_or_not"),c("0"),c(NA),c(0),c(0),c(0),c(0),c(-1),c(0),c(0)))
ref_yearS <- as.data.frame(list(c(2),c("spec_or_not"),c("1"),c(NA),c(0),c(0),c(0),c(0),c(-1),c(0),c(0)))

colnames(ref_yearNS) <- colnames(BSfeCEcln) 
colnames(ref_yearS) <- colnames(BSfeCEcln) 

BSfeCEclnr <- BSfeCEcln %>%
  bind_rows(ref_yearNS) %>%
  bind_rows(ref_yearS) %>%
  write_csv("./data/processed/CSV/BSfeCEclnr.csv")

gwalkr(RSUfeCE_clnr)

# Illustration of treatment effect given 0.5% base loss per year
# Ex) exp(δ+β)=exp(−0.005−0.0118)=exp(−0.0168)≈0.9833

sunab_examp <- fread("./data/raw/CSV/treated_control_comparisonRSUcoeffTWFE.csv") %>%
  clean_names() %>%
  mutate(base_rate = 0.005) %>%
  mutate(treat_rate = 0.9833) %>%
  mutate(year_t = year_t - 1)

View(sunab_examp)

new_row <- list()

new_year = as.numeric(sunab_examp[5,1] + 1)

gent_zip_diff19_11 <- gent_zips_lag %>%
  select(bbl, year, n_rsu_cln, did_gent) %>%
  mutate(key = row.names(gent_zips_lag)) %>%
  group_by(bbl) %>%
  pivot_wider(id_cols = c("key","bbl","did_gent"), names_from = "year", values_from = "n_rsu_cln",names_prefix = "rsu") %>%
  mutate(x1911diff = rsu2019 - rsu2011) %>%
  mutate(x1911diff = if_else(is.na(x1911diff),rsu2018 - rsu2010,x1911diff)) %>%
  mutate(x1911diff = if_else(is.na(x1911diff),rsu2017 - rsu2009,x1911diff)) %>%
  mutate(x1911diff = if_else(is.na(x1911diff),rsu2020 - rsu2012,x1911diff)) %>%
  distinct()




