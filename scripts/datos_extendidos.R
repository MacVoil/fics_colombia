options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(TTR)
library(readxl)

source("scripts/facts_dims.R")

datos <- get_fics_extended()



dims <- get_dim_fics()


segmentos_base <- read_xlsx("Auxiliares/segmentos_base.xlsx") %>% 
    #filter(Columna1 %in% c("MONEY MARKET"
                           #,"1525"
                           #, "DCE 30 A 120 DIAS"
                           #,"BALANCEADO"
                           # ,"DCE CERRADOS"
                           #,"RENTA VARIABLE"
                           #,"PLAZO RENTA FIJA"
                           #,"INTERNACIONALES"
                           #,"DCE 1 A 3 AÑOS"
                           # ,"BALANCEADO GLOBAL"
                           # ,"DCE 120 A 365 DIAS"
                     #      )) %>%
    filter(nombre_entidad %in% c("ALIANZA FIDUCIARIA S.A.",
                              "FIDUCIARIA BOGOTA S.A.",
                              #"BBVA ASSET MANAGEMENT S.A. SOCIEDAD FIDUCIARIA",
                              "FIDUCIARIA DAVIVIENDA S.A.",
                              #"CREDICORP CAPITAL",
                              "SKANDIA FIDUCIARIA S.A.",
                              "FIDUCIARIA BANCOLOMBIA S.A. SOCIEDAD FIDUCIARIA"
                              )) %>%
    pull(cod)



# datos_base %>% filter(cod %in% segmentos_base) %>%  
#     drop_na() %>% 
#     plot_time_series(fecha_corte, rent_365, .color_var = cod, .smooth = FALSE)   


datos2 <- datos %>% 
    filter(cod %in% segmentos_base) %>% 
    group_by(cod) %>% 
    mutate(conteo = ifelse(rm != 0,1,0),
           n = n(),
           per = sum(conteo)/max(n())) %>% 
    ungroup()

datos_fil <- datos2 %>% 
    filter(fecha_corte > today()-months(13)) %>% 
    group_by(cod) %>% 
    mutate(min_rent = min(rent_365, na.rm = TRUE),
           min_ema = min(EMA30, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(fecha_corte == max(fecha_corte)) %>% 
    filter(min_rent>0) %>% 
    filter(min_ema>-0.0002) %>% 
    filter(per>0.5) %>% 
    #filter(rent_365>mean(rent_365)) %>% 
    #filter(EMA30>mean(EMA30)) %>% 
    #filter(vol_365>mean(vol_365)) %>% 
    #filter(vol_30>mean(vol_30)) %>% 
    pull(cod)

datos2 %>% filter(cod %in% datos_fil) %>%  
    drop_na() %>% 
    arrange(cod, fecha_corte) %>% 
    group_by(cod) %>% 
    mutate(l_ema = last(rent_365)) %>% 
    ungroup() %>% 
    mutate(rank = dense_rank(desc(l_ema))) %>%
    #filter(rank <=5) %>%
    left_join(dims) %>% 
    plot_time_series(fecha_corte, rent_30, .color_var = nombre_patrimonio, .smooth = FALSE)   


