options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(arrow)

source("scripts/facts_dims.R")

datos <- get_fics() %>%  
    group_by(cod) %>% 
    mutate(max_date = max(fecha_corte)) %>% 
    ungroup() %>% 
    filter(max_date == max(fecha_corte)) %>% 
    group_by(cod, tipo_participacion) %>%
    mutate(max_date = max(fecha_corte),
           n = n()) %>% 
    ungroup() %>% 
    filter(max_date == max(fecha_corte)) %>% 
    filter(n >= 365) %>% 
    select(-max_date,-n)  

max_cli <- datos %>% 
    filter(fecha_corte == max(fecha_corte)) %>% 
    group_by(cod) %>% 
    filter(numero_inversionistas == max(numero_inversionistas)) %>% 
    filter(valor_fondo_cierre_dia_t == max(valor_fondo_cierre_dia_t)) %>% 
    ungroup() %>% 
    select(cod, tipo_participacion) %>% 
    distinct(cod, .keep_all = TRUE) %>% 
    mutate(max_cli = 1)

datos <- datos %>% 
    left_join(max_cli, by = c("cod", "tipo_participacion")) %>% 
    replace_na(list(max_cli = 0))
    
dims <- get_dim_fics() %>% 
    filter(cod %in% datos$cod)

write_parquet(datos, "bases_pbi/fics_facts.parquet")
write_parquet(dims, "bases_pbi/fics_dims.parquet")
