options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(TTR)
library(readxl)
library(dtwclust)
library(furrr)
library(ggbiplot)
library(umap)
library(plotly)
library(mclust)
library(diceR)
library(dbscan)
library(modeltime)


datos_cluster <- representacion_clustes %>% 
    select(CSPA, fecha_corte, q2) %>% 
    mutate(q2 = q2-1)

datos_cluster %>% 
    filter(CSPA == 5) %>% 
plot_stl_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = c("observed", "season", "trend", "remainder"))

datos_cluster %>% 
    group_by(CSPA) %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = q2, .lags = 2:30)

datos_mercado <- datos %>% 
    group_by(fecha_corte) %>% 
    summarise(precierre_fondo_dia_t = sum(precierre_fondo_dia_t), rendimientos_abonados = sum(rendimientos_abonados)) %>% 
    ungroup() %>% 
    mutate(crecimiento_dia = precierre_fondo_dia_t/(precierre_fondo_dia_t-rendimientos_abonados))

datos_mercado %>% 
    plot_stl_diagnostics(.date_var = fecha_corte, .value = crecimiento_dia-1, .feature_set = c("observed", "season", "trend", "remainder"))


datos_mercado %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = crecimiento_dia-1, .lags = 2:30)


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
                "day") %>% 
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

datos_forecast_limpieza_base_activos_major_n_inv <- datos_forecast_limpieza_base_activos %>% 
    semi_join(fondos_base, by = c("cod", "tipo_participacion")) %>% 
    group_by(cod, tipo_participacion) %>% 
    arrange(cod,fecha_corte) %>% 
    mutate(rendimientos_abonados = round(rendimientos_abonados, 1),
           precierre_fondo_dia_t = round(precierre_fondo_dia_t, 1),
           crecimiento_dia = precierre_fondo_dia_t/(precierre_fondo_dia_t-rendimientos_abonados),
           crecimiento_dia = if_else(is.infinite(crecimiento_dia)|is.nan(crecimiento_dia),
                                     1,
                                     crecimiento_dia)) %>% 
    ungroup() %>% 
    select(fecha_corte, cod, crecimiento_dia)

datos_forecast_limpieza_base_activos_major_n_inv_mean <- datos_forecast_limpieza_base_activos_major_n_inv %>% 
    group_by(fecha_corte) %>% 
    summarise(q1 = quantile(crecimiento_dia, 0.25),
              q2 = quantile(crecimiento_dia, 0.5),
              q3 = quantile(crecimiento_dia, 0.75),
    ) %>% 
    ungroup()

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_time_series(fecha_corte, q2,  .smooth = FALSE) 

p <- datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    ggplot(aes(x =fecha_corte, y = q2)) +
    geom_line() +
    geom_ribbon(aes(ymin=q1, ymax=q3), alpha = 0.5, fill = "blue")

ggplotly(p, dynamicTicks = TRUE)


datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_stl_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = c("observed", "season", "trend", "remainder"), .interactive = F)


datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = q2, .lags = 2:30)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "wday.lbl" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "week" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "month.lbl" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "quarter" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "year" )

datos_forecast_limpieza_base_activos_major_n_inv %>% 
    group_by(cod) %>% 
    plot_time_series(fecha_corte, crecimiento_dia, .smooth = FALSE, .facet_ncol = 3, .facet_nrow = 3, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000),) 

datos_forecast_limpieza_base_activos_major_n_inv_complete <- datos_forecast_limpieza_base_activos_major_n_inv %>% 
    group_by(cod) %>% 
    pad_by_time(fecha_corte,
        .by = "day", 
        .pad_value = 1,
        .start_date = min(datos_forecast_limpieza_base_activos_major_n_inv$fecha_corte) # <- Add this to pad each group from the beginning
    )

datos_forecast_limpieza_base_activos_major_n_inv_complete %>% 
    group_by(cod) %>% 
    plot_time_series(fecha_corte, crecimiento_dia, .smooth = FALSE, .facet_ncol = 3, .facet_nrow = 3, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000)) 




