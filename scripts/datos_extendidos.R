options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(TTR)
library(readxl)

source("scripts/facts_dims.R")

datos_fics_fcp <- get_fics() %>% 
    group_by(cod,
             tipo_participacion) %>% 
    pad_by_time(fecha_corte,
                "day") %>% 
    fill(valor_fondo_cierre_dia_t) %>%
    fill(numero_unidades_fondo_cierre) %>% 
    fill(numero_inversionistas) %>% 
    mutate(rendimientos_abonados = replace_na(rendimientos_abonados,0),
           rm = replace_na(rm,0),
           precierre_fondo_dia_t = if_else(is.na(precierre_fondo_dia_t),
                                           valor_fondo_cierre_dia_t,
                                           precierre_fondo_dia_t)) %>%
    ungroup() 

datos_preb <- datos_fics_fcp

max_date <- max(datos_preb$fecha_corte)

max_date_minus_4_weeks <- max_date - weeks(4)


datos_hist <- datos_preb %>% 
    filter(fecha_corte < max_date_minus_4_weeks)


datos_mes <- datos_preb %>% 
    filter(fecha_corte >= max_date_minus_4_weeks) %>% 
    group_by(cod,
             tipo_participacion) %>% 
    pad_by_time(fecha_corte,
                "day", 
                .end_date = max_date) %>% 
    fill(valor_fondo_cierre_dia_t) %>%
    fill(numero_unidades_fondo_cierre) %>% 
    fill(numero_inversionistas) %>% 
    mutate(rendimientos_abonados = replace_na(rendimientos_abonados,0),
           rm = replace_na(rm,0),
           precierre_fondo_dia_t = if_else(is.na(precierre_fondo_dia_t),
                                           valor_fondo_cierre_dia_t,
                                           precierre_fondo_dia_t)) %>%
    ungroup() 

#######################
datos_base <- bind_rows(datos_hist,datos_mes) %>% 
    group_by(cod, tipo_participacion) %>% 
    mutate(n_fondo = n()) %>% 
    ungroup() %>% 
    group_by(cod) %>% 
    mutate(n_fondo = max(n_fondo)) %>% 
    ungroup() %>% 
    filter(n_fondo > 365) %>%  # Fondos con un año o más de creados
    select(-n_fondo)
    
fondos_activos <- datos_base %>% 
    group_by(cod) %>% 
    mutate(max_fecha = max(fecha_corte)) %>% 
    ungroup() %>% 
    filter(max_fecha == max(fecha_corte)) %>% 
    pull(cod) %>% 
    unique()
 
datos_base_activos <- datos_base %>% 
    filter(cod %in% fondos_activos)

datos_base_parti_basica <- datos_base_activos %>% 
    group_by(cod, tipo_participacion) %>% 
    mutate(max_fecha = max(fecha_corte),
           n = n()) %>% 
    ungroup() %>% 
    filter(max_fecha == max(fecha_corte)) %>% 
    filter(n>365) %>% 
    group_by(cod, tipo_participacion) %>% 
    arrange(cod,fecha_corte) %>% 
    mutate(rendimientos_abonados = round(rendimientos_abonados, 1),
           precierre_fondo_dia_t = round(precierre_fondo_dia_t, 1),
           crecimiento_dia = precierre_fondo_dia_t/(precierre_fondo_dia_t-rendimientos_abonados),
           crecimiento_dia = if_else(is.infinite(crecimiento_dia)|is.nan(crecimiento_dia),
                                     1,
                                     crecimiento_dia)) %>%
    mutate(rent_30 = slidify_vec(
        .x      = crecimiento_dia,
        .period = 30,
        .f      = ~ (prod(.)^(365/30))-1,
        .align  = "rigth")) %>% 
    ungroup()
    
fondos_base <- datos_base_parti_basica %>% 
    drop_na() %>% 
    filter(fecha_corte == max(fecha_corte)) %>% 
    group_by(cod) %>% 
    filter(rent_30 == min(rent_30)) %>% 
    filter(numero_inversionistas >0) %>% 
    filter(valor_fondo_cierre_dia_t == max(valor_fondo_cierre_dia_t)) %>% 
    ungroup() %>% 
    select(cod, tipo_participacion) %>% 
    distinct(cod, .keep_all = TRUE)

datos_base_parti_basica_2 <- datos_base_parti_basica %>% 
    semi_join(fondos_base, by = c("cod", "tipo_participacion")) %>% 
    group_by(cod, tipo_participacion) %>% 
    mutate(
        rent_90 = slidify_vec(
        .x      = crecimiento_dia,
        .period = 90,
        .f      = ~ (prod(.)^(365/90))-1,
        .align  = "rigth"),
        rent_180 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 180,
            .f      = ~ (prod(.)^(365/180))-1,
            .align  = "rigth"),
        rent_365 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 365,
            .f      = ~ (prod(.))-1,
            .align  = "rigth"),
        vol_30 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 30,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        vol_90 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 90,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        vol_180 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 180,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        vol_365 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 365,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        EMA30 = EMA((crecimiento_dia-1), n=30),
        EMA90 = EMA((crecimiento_dia-1), n=90),
        EMA180 = EMA((crecimiento_dia-1), n=180),
        EMA365 = EMA((crecimiento_dia-1), n=365),
        cobert_ries_30 = rent_30/vol_30,
        cobert_ries_90 = rent_90/vol_90,
        cobert_ries_180 = rent_180/vol_180
        ) %>% 
    ungroup()
#####################
datos_base <- bind_rows(datos_hist,datos_mes) %>% 
    group_by(fecha_corte, cod) %>% 
    summarise(across(rendimientos_abonados:rm, ~ sum(.))) %>% 
    ungroup() %>%
    mutate(rendimientos_abonados = round(rendimientos_abonados, 1),
           precierre_fondo_dia_t = round(precierre_fondo_dia_t, 1),
           crecimiento_dia = precierre_fondo_dia_t/(precierre_fondo_dia_t-rendimientos_abonados),
           crecimiento_dia = if_else(is.infinite(crecimiento_dia)|is.nan(crecimiento_dia), 
                                     1,
                                     crecimiento_dia)) %>% 
    group_by(cod) %>% 
    arrange(cod,fecha_corte) %>% 
    mutate(n = n()) %>% 
    filter(n>=365) %>% 
    mutate(
        rent_30 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 30,
            .f      = ~ (prod(.)^(365/30))-1,
            .align  = "rigth"),
        rent_90 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 90,
            .f      = ~ (prod(.)^(365/90))-1,
            .align  = "rigth"),
        rent_180 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 180,
            .f      = ~ (prod(.)^(365/180))-1,
            .align  = "rigth"),
        rent_365 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 365,
            .f      = ~ (prod(.))-1,
            .align  = "rigth"),
        vol_30 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 30,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        vol_90 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 90,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        vol_180 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 180,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        vol_365 = slidify_vec(
            .x      = crecimiento_dia,
            .period = 365,
            .f      = ~sd(.-1)*sqrt(365),
            .align  = "rigth"),
        EMA30 = EMA((crecimiento_dia-1), n=30),
        EMA90 = EMA((crecimiento_dia-1), n=90),
        EMA180 = EMA((crecimiento_dia-1), n=180),
        EMA365 = EMA((crecimiento_dia-1), n=365),
        cobert_ries_30 = rent_30/vol_30,
        cobert_ries_90 = rent_90/vol_90,
        cobert_ries_180 = rent_180/vol_180,
        cobert_ries_365 = rent_365/vol_365,
        rm = round(rm, 1),
        valor_fondo_cierre_dia_t = round(valor_fondo_cierre_dia_t, 1),
        crecimiento_rm = valor_fondo_cierre_dia_t/(valor_fondo_cierre_dia_t-rm),
        crecimiento_rm = if_else(is.infinite(crecimiento_rm)|is.nan(crecimiento_rm), 
                                  1,
                                 crecimiento_rm),
        cre_rm_30 = slidify_vec(
            .x      = rm,
            .period = 30,
            .f      = ~ (sum(.)),
            .align  = "rigth"),
        cre_rm_30 = cre_rm_30/lag(valor_fondo_cierre_dia_t,30)
    ) %>% 
    ungroup()

segmentos_base <- read_xlsx("Auxiliares/segmentos_base.xlsx") %>% 
    filter(Columna1 %in% c("MONEY MARKET","1525")) %>% 
    pull(cod)

dims <- get_dim_fics()

datos_base %>% filter(cod %in% segmentos_base) %>%  
    drop_na() %>% 
    plot_time_series(fecha_corte, rent_365, .color_var = cod, .smooth = FALSE)   


datos_base_parti_basica_2 %>% filter(cod %in% segmentos_base) %>%  
    mutate(EMA15 = EMA((crecimiento_dia-1), n=15)) %>% 
    drop_na() %>% 
    arrange(cod, fecha_corte) %>% 
    group_by(cod) %>% 
    mutate(l_ema = last(EMA30)) %>% 
    ungroup() %>% 
    mutate(rank = dense_rank(desc(l_ema))) %>% 
    filter(rank <=5) %>% 
    left_join(dims) %>% 
    plot_time_series(fecha_corte, rent_365, .color_var = nombre_patrimonio, .smooth = FALSE)   


