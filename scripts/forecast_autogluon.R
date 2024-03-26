options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(plotly)
library(tidymodels)
library(openxlsx)
library(modeltime)

library(reticulate)
use_condaenv(condaenv = "C:\\Users\\user\\miniconda3\\envs\\ag\\python.exe")

source("scripts/facts_dims.R")

datos_forecast <- get_fics(
    floor_date(floor_date(today()-years(5), "year")),
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

# max_date <- max(datos_forecast_limpieza$fecha_corte)
# 
# max_date_minus_4_weeks <- max_date - weeks(4)
# 
# datos_hist <- datos_forecast_limpieza %>% 
#     filter(fecha_corte < max_date_minus_4_weeks)
# 
# datos_mes <- datos_forecast_limpieza %>% 
#     filter(fecha_corte >= max_date_minus_4_weeks) %>% 
#     group_by(cod,
#              tipo_participacion) %>% 
#     pad_by_time(fecha_corte,
#                 "day", 
#                 .end_date = max_date) %>% 
#     fill(valor_fondo_cierre_dia_t) %>%
#     fill(numero_inversionistas) %>%
#     mutate(rendimientos_abonados = replace_na(rendimientos_abonados,0),
#            precierre_fondo_dia_t = if_else(is.na(precierre_fondo_dia_t),
#                                            valor_fondo_cierre_dia_t,
#                                            precierre_fondo_dia_t))
# 
# datos_forecast_limpieza_base <- bind_rows(datos_hist,datos_mes)

datos_xlsx <- read.xlsx("Auxiliares/reporteRentabilidades.xlsx", detectDates = TRUE) %>% 
    as_tibble() %>% 
    select(Fecha.corte, 
           Tipo.Entidad,
           Cód..Entidad, 
           Cód..Negocio, 
           Subtipo.Negocio,
           Cons..id.Part.,
           Valor.fondo.al.cierre.del.día.t, 
           Núm..Invers., 
           Rentab..dia,
           `Tipo.Part..<sup>1<sup/>`) %>% 
    filter(Subtipo.Negocio != "FONDOS DE CAPITAL PRIVADO") %>% 
    mutate(Fecha.corte = dmy(Fecha.corte),
           Núm..Invers. = as.numeric(Núm..Invers.),
           Rentab..dia = ((1+(as.numeric(Rentab..dia)/100))^(1/365)),
           Valor.fondo.al.cierre.del.día.t = as.numeric(str_replace_all(Valor.fondo.al.cierre.del.día.t, "[^0-9\\.]", "")),
           Subtipo.Negocio = case_when(
               Subtipo.Negocio == "FIC BURSATILES" ~ 1,
               Subtipo.Negocio == "FIC DE MERCADO MONETARIO" ~ 2,
               Subtipo.Negocio == "FIC INMOBILIARIAS" ~ 3,
               Subtipo.Negocio == "FIC DE TIPO GENERAL" ~ 1
           ),
           cod = str_c(Tipo.Entidad,Cód..Entidad,Subtipo.Negocio,Cód..Negocio, sep = "_"),
           tipo_participacion = str_c(`Tipo.Part..<sup>1<sup/>`, Cons..id.Part.)) %>% 
    select(Fecha.corte,Valor.fondo.al.cierre.del.día.t,Núm..Invers.,Rentab..dia,cod,tipo_participacion) %>% 
    arrange(Fecha.corte) %>% 
    group_by(cod, tipo_participacion) %>% 
    mutate(precierre_fondo_dia_t = lag(Valor.fondo.al.cierre.del.día.t)* Rentab..dia,
           rendimientos_abonados = precierre_fondo_dia_t-lag(Valor.fondo.al.cierre.del.día.t)) %>% 
    slice(-1) %>% 
    select(Fecha.corte, cod, tipo_participacion, rendimientos_abonados, precierre_fondo_dia_t, Valor.fondo.al.cierre.del.día.t, Núm..Invers.) %>% 
    ungroup() %>% 
    rename(fecha_corte = Fecha.corte, valor_fondo_cierre_dia_t = Valor.fondo.al.cierre.del.día.t, numero_inversionistas = Núm..Invers.) %>% 
    semi_join(datos_forecast_limpieza %>% 
                  filter(fecha_corte >= floor_date(today()%m-%months(1), "month")), 
              by =join_by(cod, tipo_participacion)) %>% 
    anti_join(datos_forecast_limpieza, by =join_by(fecha_corte, cod, tipo_participacion))
  
datos_forecast_limpieza_base <- bind_rows(datos_forecast_limpieza,datos_xlsx)

datos_forecast_limpieza_base_activos <- datos_forecast_limpieza_base %>% 
    group_by(cod, tipo_participacion) %>%
    mutate(max_fecha = max(fecha_corte),
           n = n()) %>% 
    ungroup() %>% 
    filter(max_fecha == max(fecha_corte)) %>% 
    filter(n > 365*2)

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

datos_forecast_limpieza_base_activos_major_n_inv %>% 
    group_by(cod) %>% 
    plot_time_series(fecha_corte, crecimiento_dia, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000), .smooth = FALSE,
                     .facet_ncol = 3, .facet_nrow = 3)



datos_forecast_limpieza_base_activos_major_n_inv_mean <- datos_forecast_limpieza_base_activos_major_n_inv %>% 
  group_by(fecha_corte) %>% 
  summarise(mean_creci = quantile(crecimiento_dia, 0.5)
  ) %>% 
  ungroup()

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  plot_time_series(fecha_corte, mean_creci, .smooth = FALSE)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
  plot_stl_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = c("observed", "season", "trend", "remainder"), .interactive = F)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
  drop_na() %>% 
  plot_acf_diagnostics(.date_var = fecha_corte, .value = mean_creci, .lags = 2:365)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
  plot_seasonal_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = "wday.lbl", .geom = "violin")

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
  plot_seasonal_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = "month.lbl", .geom = "violin" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
  plot_seasonal_diagnostics(.date_var = fecha_corte, .value = mean_creci, .feature_set = "year", .geom = "violin" )


#######################
ibr <- read.xlsx("Auxiliares/ibr.xlsx", detectDates = TRUE) 

trm <- read.xlsx("Auxiliares/trm.xlsx", detectDates = TRUE) %>% 
  select(fecha, trm)

ipc <- read.xlsx("Auxiliares/ipc.xlsx", detectDates = FALSE) %>% 
  select(fecha, ipc) %>% 
  mutate(fecha = ymd(fecha, truncated = TRUE)) %>% 
  pad_by_time(fecha,
              "day", .fill_na_direction = "down", .end_date = ceiling_date(max(.$fecha), unit = "month")) 

creci_trend <- datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  tk_stl_diagnostics(.date_var = fecha_corte, .value = mean_creci) %>% 
  select(fecha_corte, trend) %>% 
  rename(creci_trend = trend) %>% 
  left_join(ibr, by = c("fecha_corte" = "fecha")) %>% 
  left_join(trm, by = c("fecha_corte" = "fecha")) %>% 
  left_join(ipc, by = c("fecha_corte" = "fecha")) %>% 
  mutate(across(c(creci_trend, ibr, trm, ipc), standardize_vec)) 

creci_trend %>% 
  pivot_longer(-fecha_corte) %>% 
  plot_time_series(.date_var = fecha_corte, .value = value, .color_var = name,.smooth = FALSE)

datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors <- datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
  left_join(ibr, by = c("fecha_corte" = "fecha")) %>% 
  left_join(trm, by = c("fecha_corte" = "fecha")) %>% 
  left_join(ipc, by = c("fecha_corte" = "fecha")) 

datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors %>% 
  filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
  drop_na() %>% 
  plot_acf_diagnostics(.date_var = fecha_corte, .value = mean_creci, .ccf_vars = c(ipc, trm, ibr), .show_ccf_vars_only = TRUE,  .facet_ncol = 1)

festivos <- read.xlsx("Auxiliares/festivos.xlsx", detectDates = TRUE) 

#######################
datos_completos <- datos_forecast_limpieza_base_activos_major_n_inv %>%
  rename(creci_dia = crecimiento_dia) %>%
  group_by(cod) %>%
  future_frame(fecha_corte, .length_out = 28, .bind_data = TRUE) %>%
  ungroup() %>%
  left_join(ibr, by = c("fecha_corte" = "fecha")) %>%
  left_join(trm, by = c("fecha_corte" = "fecha")) %>%
  left_join(ipc, by = c("fecha_corte" = "fecha")) %>% 
  left_join(festivos, by = c("fecha_corte" = "fecha")) %>% 
  replace_na(list(festivo = FALSE)) %>% 
  mutate(dia_semana = wday(fecha_corte, week_start = 1),
         fin_de_semama = dia_semana %in% 6:7,
         dia_no_habil = as.numeric(festivo|fin_de_semama)) %>% 
  select(-c(festivo:fin_de_semama)) %>% 
  mutate(across(ibr:ipc, standardize_vec))
  
    
datos_forecast <- datos_completos %>% 
    filter(is.na(creci_dia)) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

datos_modelar <- datos_completos %>% 
    filter(!is.na(creci_dia)) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

datos_splits <- time_series_split(datos_modelar, assess = 28, cumulative = TRUE)

datos_splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(fecha_corte, creci_dia)

datos_train <- training(datos_splits) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

datos_test <- testing(datos_splits) %>%
    arrange(cod,fecha_corte) %>%
    ungroup()

time_series_cv(datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
                 filter(fecha_corte >= today() - years(2)),
               date_var    = fecha_corte,
               assess      = "28 days",
               skip        = "28 days",
               cumulative = TRUE,
               slice_limit = 13) %>%
  plot_time_series_cv_plan(fecha_corte, mean_creci, .interactive = FALSE, .facet_ncol = 3, .facet_dir = "v")

####
datos_pred <- py$predictions_full %>% 
    rename(cod = item_id, fecha_corte = timestamp, creci_dia = mean) %>% 
    mutate(fecha_corte = ymd(as.Date(fecha_corte))) %>% 
    select(-`0.05`, -`0.95`)
           

datos_plot <- datos_modelar %>% 
    select(-ibr, -trm, -ipc, dia_no_habil) %>% 
    bind_rows(datos_pred)

datos_modeltime <- datos_plot %>% 
    mutate(.model_id = if_else(is.na(`0.25`), NA, 1) ,
           .model_desc = if_else(is.na(`0.25`), "ACTUAL", "AutoGluon_TS_KC"),
           .key = if_else(is.na(`0.25`), "actual", "prediction")) %>% 
    rename(.index = fecha_corte, .value = creci_dia, .conf_lo = `0.25`, .conf_hi = `0.75`)

datos_modeltime %>% 
    group_by(cod) %>% 
    filter(.index >= today()-months(6)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                        .trelliscope_params = list(width = 1000))

data_rmse <- datos_modelar %>% 
    select(-ibr, -trm, -ipc, -dia_no_habil) %>% 
    rename(actual = creci_dia) %>% 
    inner_join(datos_pred %>% 
                   rename(pred = creci_dia),
               by = join_by(cod, fecha_corte))

rmse(data_rmse, truth = actual, estimate = pred)

plot_pyoject <- datos_modeltime %>% 
  group_by(cod) %>% 
  mutate(across(.conf_lo:.conf_hi, ~coalesce(.,.value)),
         across(c(.value,.conf_lo:.conf_hi), ~ slidify_vec(
           .x      = .,
           .period = 30,
           .f      = ~ (prod(.+1)^(365/30))-1,
           .align  = "rigth")))

plot_pyoject %>% 
  group_by(cod) %>% 
  filter(.index >= today()-months(6)) %>% 
  plot_modeltime_forecast(.trelliscope = TRUE, 
                          .trelliscope_params = list(width = 1000))
