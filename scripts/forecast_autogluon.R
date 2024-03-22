options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(plotly)
library(tidymodels)
library(modeltime.h2o)

source("scripts/facts_dims.R")

datos_forecast <- get_fics(
    floor_date(today(), "years") - years(5),
    today())


datos_forecast_limpieza <-  datos_forecast %>% 
    select(fecha_corte, cod, tipo_participacion, rendimientos_abonados, precierre_fondo_dia_t, valor_fondo_cierre_dia_t, numero_inversionistas) %>% 
    group_by(cod,
             tipo_participacion) %>% 
    pad_by_time(fecha_corte,
                "day") %>% 
    fill(valor_fondo_cierre_dia_t) %>%
    fill(numero_inversionistas) %>%
    mutate(rendimientos_abonados = replace_na(rendimientos_abonados,0),
           precierre_fondo_dia_t = if_else(is.na(precierre_fondo_dia_t),
                                           valor_fondo_cierre_dia_t,
                                           precierre_fondo_dia_t)) %>%
    ungroup() 

max_date <- max(datos_forecast_limpieza$fecha_corte)

max_date_minus_4_weeks <- max_date - weeks(4)

datos_hist <- datos_forecast_limpieza %>% 
    filter(fecha_corte < max_date_minus_4_weeks)

datos_mes <- datos_forecast_limpieza %>% 
    filter(fecha_corte >= max_date_minus_4_weeks) %>% 
    group_by(cod,
             tipo_participacion) %>% 
    pad_by_time(fecha_corte,
                "day", 
                .end_date = max_date) %>% 
    fill(valor_fondo_cierre_dia_t) %>%
    fill(numero_inversionistas) %>%
    mutate(rendimientos_abonados = replace_na(rendimientos_abonados,0),
           precierre_fondo_dia_t = if_else(is.na(precierre_fondo_dia_t),
                                           valor_fondo_cierre_dia_t,
                                           precierre_fondo_dia_t))

datos_forecast_limpieza_base <- bind_rows(datos_hist,datos_mes)

datos_forecast_limpieza_base_activos <- datos_forecast_limpieza_base %>% 
    group_by(cod, tipo_participacion) %>%
    mutate(max_fecha = max(fecha_corte),
           n = n()) %>% 
    ungroup() %>% 
    filter(max_fecha == max(fecha_corte)) %>% 
    filter(n > 365)

fondos_base <- datos_forecast_limpieza_base_activos %>% 
    filter(fecha_corte == max(fecha_corte)) %>% 
    group_by(cod) %>% 
    filter(numero_inversionistas == max(numero_inversionistas)) %>% 
    filter(numero_inversionistas >0) %>% 
    filter(valor_fondo_cierre_dia_t == max(valor_fondo_cierre_dia_t)) %>% 
    ungroup() %>% 
    select(cod, tipo_participacion) %>% 
    distinct(cod, .keep_all = TRUE)


# table_datos_dice %>% filter(CSPA == 2) %>% pull(cod)

clust2 <-  c("5_16_1_10936", "5_18_1_11286", "5_18_1_11356", "5_18_1_31508", "5_20_1_53954",
             "5_20_2_13174", "5_21_1_90300", "5_21_1_9438", "5_21_1_9448", "5_21_1_9453",
             "5_22_1_10659", "5_22_1_22969", "5_23_1_10648", "5_23_1_48153", "5_25_1_11627",
             "5_25_2_17653", "5_31_1_2852", "5_31_1_3895", "5_33_1_11014", "5_33_1_54573",
             "5_33_1_99740", "5_38_1_11714", "5_38_1_36570", "5_3_1_11962", "5_3_1_87342",
             "5_40_1_11149", "5_7_1_51953", "85_91_1_60275")

datos_forecast_limpieza_base_activos_major_n_inv <- datos_forecast_limpieza_base_activos %>% 
    semi_join(fondos_base, by = c("cod", "tipo_participacion")) %>% 
    group_by(cod, tipo_participacion) %>% 
    arrange(cod,fecha_corte) %>% 
    mutate(rendimientos_abonados = round(rendimientos_abonados, 1),
           precierre_fondo_dia_t = round(precierre_fondo_dia_t, 1),
           crecimiento_dia = (precierre_fondo_dia_t/(precierre_fondo_dia_t-rendimientos_abonados)-1),
           crecimiento_dia = if_else(is.infinite(crecimiento_dia)|is.nan(crecimiento_dia),
                                     0,
                                     crecimiento_dia)) %>% 
    ungroup() %>% 
    select(fecha_corte, cod, crecimiento_dia) %>% 
    filter(cod %in% clust2)

ibr <- openxlsx::read.xlsx("Auxiliares/ibr.xlsx", detectDates = TRUE) %>% 
    mutate(ibr = standardize_vec(ibr))

trm <- openxlsx::read.xlsx("Auxiliares/trm.xlsx", detectDates = TRUE) %>% 
    mutate(trm = standardize_vec(trm))

ipc <- openxlsx::read.xlsx("Auxiliares/ipc.xlsx", detectDates = FALSE) %>% 
    select(fecha, ipc) %>% 
    mutate(fecha = ymd(fecha, truncated = TRUE)) %>% 
    pad_by_time(fecha,
                "day", .fill_na_direction = "down", .end_date = ceiling_date(max(.$fecha), unit = "month")) %>% 
    mutate(ipc = standardize_vec(ipc)) 
    

datos_completos <- datos_forecast_limpieza_base_activos_major_n_inv %>%
    rename(creci_dia = crecimiento_dia) %>%
    #transformer_function() %>% 
    group_by(cod) %>%
    #filter(!is.na(ma7)) %>%
    future_frame(fecha_corte, .length_out = 35, .bind_data = TRUE) %>%
    ungroup() %>%
    left_join(ibr, by = c("fecha_corte" = "fecha")) %>%
    left_join(trm, by = c("fecha_corte" = "fecha")) %>%
    left_join(ipc, by = c("fecha_corte" = "fecha")) %>%
    mutate(ipc_lag = lag(ipc, 136)) %>%
    mutate(trm_lag = lag(trm, 117)) %>%
    filter(!is.na(ipc_lag)) %>%
    filter(!is.na(trm_lag)) %>%
    select(cod, fecha_corte, creci_dia, ipc_lag, ibr, trm_lag) 
    
datos_forecast <- datos_completos %>% 
    filter(is.na(creci_dia)) %>%
    arrange(cod,fecha_corte) %>%
    select(cod,fecha_corte,creci_dia,ipc_lag,ibr,trm_lag) %>% 
    ungroup()

datos_modelar <- datos_completos %>% 
    filter(!is.na(creci_dia)) %>%
    arrange(cod,fecha_corte) %>%
    select(cod,fecha_corte,creci_dia,ipc_lag,ibr,trm_lag) %>% 
    ungroup()


library(reticulate)
use_condaenv(condaenv = "C:\\Users\\user\\miniconda3\\envs\\ag\\python.exe")

datos_splits <- time_series_split(datos_modelar, assess = 35, cumulative = TRUE)

datos_splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(fecha_corte, creci_dia)

datos_train <- training(datos_splits) %>%
    arrange(cod,fecha_corte) %>%
    select(cod,fecha_corte,creci_dia,ipc_lag,ibr,trm_lag) %>% 
    ungroup()

datos_test <- testing(datos_splits) %>%
    arrange(cod,fecha_corte) %>%
    select(cod,fecha_corte,creci_dia,ipc_lag,ibr,trm_lag) %>% 
    ungroup()


####
datos_pred <- py$predictions_full %>% 
    rename(cod = item_id, fecha_corte = timestamp, creci_dia = mean) %>% 
    mutate(fecha_corte = ymd(as.Date(fecha_corte))) %>% 
    select(-`0.5`)
           

datos_plot <- datos_modelar %>% 
    select(-ibr, -trm_lag, -ipc_lag) %>% 
    bind_rows(datos_pred)

datos_modeltime <- datos_plot %>% 
    mutate(.model_id = if_else(is.na(`0.95`), NA, 1) ,
           .model_desc = if_else(is.na(`0.95`), "ACTUAL", "AutoGluon_TS_KC"),
           .key = if_else(is.na(`0.95`), "actual", "prediction")) %>% 
    rename(.index = fecha_corte, .value = creci_dia, .conf_lo = `0.05`, .conf_hi = `0.95`)

datos_modeltime %>% 
    group_by(cod) %>% 
    filter(.index >= today()-months(3)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                        .trelliscope_params = list(width = 1000))

data_rmse <- datos_modelar %>% 
    select(-ibr, -trm_lag, -ipc_lag) %>% 
    rename(actual = creci_dia) %>% 
    inner_join(datos_pred %>% 
                   rename(pred = creci_dia),
               by = join_by(cod, fecha_corte))

rmse(data_rmse, truth = actual, estimate = pred)



