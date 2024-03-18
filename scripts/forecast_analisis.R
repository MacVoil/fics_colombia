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
    filter(cod %in% clust2) %>% 
    mutate(cod = as_factor(cod))



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
    #filter(fecha_corte >= "2021-01-01") %>% 
    plot_stl_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = c("observed", "season", "trend", "remainder"), .interactive = F)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    #filter(fecha_corte >= "2021-01-01") %>% 
    plot_stl_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = c("trend"), .interactive = T)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% tk_stl_diagnostics(.date_var = fecha_corte, .value = q2) %>% 
    plot_time_series(fecha_corte, trend, .smooth_period = 365*2)


datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = q2, .lags = 2:30)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = q2, .lags = 2:30)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "wday.lbl" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "month.lbl" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "year" )


ibr <- openxlsx::read.xlsx("Auxiliares/ibr.xlsx", detectDates = TRUE) 

trm <- openxlsx::read.xlsx("Auxiliares/trm.xlsx", detectDates = TRUE)

ipc <- openxlsx::read.xlsx("Auxiliares/ipc.xlsx", detectDates = FALSE) %>% 
    select(fecha, ipc) %>% 
    mutate(fecha = ymd(fecha, truncated = TRUE)) %>% 
    pad_by_time(fecha,
                "day", .fill_na_direction = "down", .end_date = ceiling_date(max(.$fecha), unit = "month"))


datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors <- datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    left_join(ibr, by = c("fecha_corte" = "fecha")) %>% 
    left_join(trm, by = c("fecha_corte" = "fecha")) %>% 
    left_join(ipc, by = c("fecha_corte" = "fecha")) 

datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors %>% 
    filter(fecha_corte <= "2020-03-01" | fecha_corte >= "2020-04-01") %>% 
    drop_na() %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = q2, .ccf_vars = c(ipc, trm, ibr), .show_ccf_vars_only = TRUE, .lags = 0:365, .facet_ncol = 4)

# datos_completos <- datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors %>% 
#     future_frame(fecha_corte, .length_out = 30, .bind_data = TRUE) %>% 
#     mutate(ipc_lag = lag(ipc, 136)) %>% 
#     filter(!is.na(ipc_lag)) %>% 
#     select(fecha_corte, q2, ibr,ipc_lag) %>% 
#     rename(creci_dia = q2)

transformer_function <- function(data) {
    data %>%
        group_by(cod) %>%
        mutate(
        ma7 = slidify_vec(
            .x      = lag(creci_dia),
            .period = 7,
            .f      = mean,
            .align  = "rigth")) %>%
        ungroup()
}

# datos_completos <- datos_forecast_limpieza_base_activos_major_n_inv %>% 
#     group_by(cod) %>% 
#     future_frame(fecha_corte, .length_out = 30, .bind_data = TRUE) %>% 
#     ungroup() %>% 
#     left_join(ibr, by = c("fecha_corte" = "fecha")) %>% 
#     left_join(trm, by = c("fecha_corte" = "fecha")) %>% 
#     left_join(ipc, by = c("fecha_corte" = "fecha")) %>% 
#     mutate(ipc_lag = lag(ipc, 136)) %>% 
#     mutate(trm_lag = lag(trm, 117)) %>% 
#     filter(!is.na(ipc_lag)) %>% 
#     filter(!is.na(trm_lag)) %>% 
#     rename(creci_dia = crecimiento_dia) %>% 
#     select(cod, fecha_corte, creci_dia, ipc_lag, ibr, trm_lag)


datos_completos <- datos_forecast_limpieza_base_activos_major_n_inv %>%
    rename(creci_dia = crecimiento_dia) %>%
    transformer_function() %>% 
    group_by(cod) %>%
    filter(!is.na(ma7)) %>%
    future_frame(fecha_corte, .length_out = 30, .bind_data = TRUE) %>%
    ungroup() %>%
    left_join(ibr, by = c("fecha_corte" = "fecha")) %>%
    left_join(trm, by = c("fecha_corte" = "fecha")) %>%
    left_join(ipc, by = c("fecha_corte" = "fecha")) %>%
    mutate(ipc_lag = lag(ipc, 136)) %>%
    mutate(trm_lag = lag(trm, 117)) %>%
    filter(!is.na(ipc_lag)) %>%
    filter(!is.na(trm_lag)) %>%
    select(cod, fecha_corte, creci_dia, ipc_lag, ibr, trm_lag, ma7)


datos_forecast <- datos_completos %>% 
    filter(is.na(creci_dia))

datos_modelar <- datos_completos %>% 
    filter(!is.na(creci_dia))


# splits ----

datos_splits <- time_series_split(datos_modelar, assess = 30, cumulative = TRUE)

datos_splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(fecha_corte, creci_dia)

#####################################
# Sys.setenv(JAVA_HOME="C:/java/jdk-17.0.2")
# h2o.init(max_mem_size = "24g")
h2o.init(max_mem_size = "10g")

model_spec_aml_h2o <- automl_reg(mode = 'regression') %>%
    set_engine(
        engine                     = 'h2o',
        max_runtime_secs           = 3600, 
        #max_runtime_secs_per_model = 5,
        max_models                 = 100,
        nfolds                     = 10,
        #exclude_algos              = c("DeepLearning"),
        verbosity                  = "info",
        seed                       = 4981
    ) 

# recetas1 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    step_rm(trm_lag) %>% 
    step_rm(ma7)

receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o", overwrite = TRUE)

model_fitted_aml_h2o <- load_h2o_model("modelos/model_fitted_aml_h2o")

modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas2 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    step_rm(ma7) %>% 
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors())

receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 


model_fitted_aml_h2o_tssignature %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature", overwrite = TRUE)
    
modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o,
    model_fitted_aml_h2o_tssignature
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas3 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_fourier(fecha_corte, period = c(7, 365), K = 2)

receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_fourier <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_fourier %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_fourier", overwrite = TRUE)

modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o,
    model_fitted_aml_h2o_tssignature,
    model_fitted_aml_h2o_tssignature_fourier
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))


# recetas4 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns")

modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o,
    model_fitted_aml_h2o_tssignature,
    #model_fitted_aml_h2o_tssignature_fourier,
    model_fitted_aml_h2o_tssignature_ns
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas5 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    # step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_lag <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns_lag %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_lag", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_lag <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_lag")

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    # model_fitted_aml_h2o_tssignature_ns,
    model_fitted_aml_h2o_tssignature_ns_lag
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))


# recetas 6 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    # step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_ibr <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns_ibr %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_ibr", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_ibr <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_ibr")

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    # model_fitted_aml_h2o_tssignature_ns,
    model_fitted_aml_h2o_tssignature_ns_lag,
    model_fitted_aml_h2o_tssignature_ns_ibr
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas 7 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    # step_rm(ipc_lag) %>% 
    # step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_ibr_lag <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns_ibr_lag %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_ibr_lag", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_ibr_lag <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_ibr_lag")

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    # model_fitted_aml_h2o_tssignature_ns,
    model_fitted_aml_h2o_tssignature_ns_lag,
    model_fitted_aml_h2o_tssignature_ns_ibr,
    model_fitted_aml_h2o_tssignature_ns_ibr_lag
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas 8 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    # step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_trm <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns_trm %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_trm", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_trm <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_trm")

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    # model_fitted_aml_h2o_tssignature_ns,
    model_fitted_aml_h2o_tssignature_ns_lag,
    #model_fitted_aml_h2o_tssignature_ns_ibr,
    #model_fitted_aml_h2o_tssignature_ns_ibr_lag
    model_fitted_aml_h2o_tssignature_ns_trm
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas 9 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    #step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    #step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_ipc_trm <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns_ipc_trm %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_ipc_trm", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_ipc_trm <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_ipc_trm")

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    # model_fitted_aml_h2o_tssignature_ns,
    #model_fitted_aml_h2o_tssignature_ns_lag,
    #model_fitted_aml_h2o_tssignature_ns_ibr,
    #model_fitted_aml_h2o_tssignature_ns_ibr_lag
    #model_fitted_aml_h2o_tssignature_ns_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_trm
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas 10 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    #step_rm(ibr) %>% 
    #step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_ibr_trm <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns_ibr_trm %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_ibr_trm", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_ibr_trm <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_ibr_trm")

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    # model_fitted_aml_h2o_tssignature_ns,
    #model_fitted_aml_h2o_tssignature_ns_lag,
    #model_fitted_aml_h2o_tssignature_ns_ibr,
    #model_fitted_aml_h2o_tssignature_ns_ibr_lag
    #model_fitted_aml_h2o_tssignature_ns_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_trm,
    model_fitted_aml_h2o_tssignature_ns_ibr_trm
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, rsq))

# recetas 11 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    #step_rm(ipc_lag) %>% 
    #step_rm(ibr) %>% 
    #step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits)) 

model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm")

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    # model_fitted_aml_h2o_tssignature_ns,
    #model_fitted_aml_h2o_tssignature_ns_lag,
    #model_fitted_aml_h2o_tssignature_ns_ibr,
    #model_fitted_aml_h2o_tssignature_ns_ibr_lag
    #model_fitted_aml_h2o_tssignature_ns_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_trm,
    #model_fitted_aml_h2o_tssignature_ns_ibr_trm
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, mae))

# recetas 12 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    #step_rm(ipc_lag) %>% 
    #step_rm(ibr) %>% 
    #step_rm(trm_lag) %>%
    #step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7 <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits))  %>%
    recursive(
        id         = "cod",
        transform  = transformer_function,
        train_tail = panel_tail(training(datos_splits), cod, 30)
    )

model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7 %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7 <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7")

modeltime_tbl <- modeltime_table(
    #model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # #model_fitted_aml_h2o_tssignature_fourier,
    #model_fitted_aml_h2o_tssignature_ns,
    # model_fitted_aml_h2o_tssignature_ns_lag,
    # model_fitted_aml_h2o_tssignature_ns_ibr,
    # model_fitted_aml_h2o_tssignature_ns_ibr_lag
    # model_fitted_aml_h2o_tssignature_ns_trm,
    # model_fitted_aml_h2o_tssignature_ns_ipc_trm,
    # model_fitted_aml_h2o_tssignature_ns_ibr_trm
    #model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, mae))

# recetas 13 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    #step_rm(ipc_lag) %>% 
    #step_rm(ibr) %>% 
    #step_rm(trm_lag) %>%
    step_rm(ma7) %>%
    #step_timeseries_signature(fecha_corte) %>% 
    #step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) #%>% 
    #step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_regresors <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits))  #%>%
    # recursive(
    #     id         = "cod",
    #     transform  = transformer_function,
    #     train_tail = panel_tail(training(datos_splits), cod, 30)
    # )

model_fitted_aml_h2o_regresors %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_regresors", overwrite = TRUE)

model_fitted_aml_h2o_regresors <- load_h2o_model("modelos/model_fitted_aml_h2o_regresors")

modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # model_fitted_aml_h2o_tssignature_fourier,
    model_fitted_aml_h2o_tssignature_ns,
    # model_fitted_aml_h2o_tssignature_ns_lag,
    # model_fitted_aml_h2o_tssignature_ns_ibr,
    # model_fitted_aml_h2o_tssignature_ns_ibr_lag
    # model_fitted_aml_h2o_tssignature_ns_trm,
    # model_fitted_aml_h2o_tssignature_ns_ipc_trm,
    # model_fitted_aml_h2o_tssignature_ns_ibr_trm
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7,
    model_fitted_aml_h2o_regresors
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, mae))

# recetas 14 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    #step_rm(ma7) %>%
    #step_timeseries_signature(fecha_corte) %>% 
    #step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) #%>% 
#step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_ma <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits))  %>%
    recursive(
        id         = "cod",
        transform  = transformer_function,
        train_tail = panel_tail(training(datos_splits), cod, 30)
        )

model_fitted_aml_h2o_ma %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_ma", overwrite = TRUE)

model_fitted_aml_h2o_ma <- load_h2o_model("modelos/model_fitted_aml_h2o_ma")

modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # model_fitted_aml_h2o_tssignature_fourier,
    model_fitted_aml_h2o_tssignature_ns,
    # model_fitted_aml_h2o_tssignature_ns_lag,
    # model_fitted_aml_h2o_tssignature_ns_ibr,
    # model_fitted_aml_h2o_tssignature_ns_ibr_lag
    # model_fitted_aml_h2o_tssignature_ns_trm,
    # model_fitted_aml_h2o_tssignature_ns_ipc_trm,
    # model_fitted_aml_h2o_tssignature_ns_ibr_trm
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7,
    model_fitted_aml_h2o_regresors,
    model_fitted_aml_h2o_ma
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, mae))


# recetas 15 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    step_rm(ipc_lag) %>% 
    step_rm(ibr) %>% 
    step_rm(trm_lag) %>%
    #step_rm(ma7) %>%
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_tssignature_ma <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits))  %>%
    recursive(
        id         = "cod",
        transform  = transformer_function,
        train_tail = panel_tail(training(datos_splits), cod, 30)
    )

model_fitted_aml_h2o_tssignature_ma %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_tssignature_ma", overwrite = TRUE)

model_fitted_aml_h2o_tssignature_ma <- load_h2o_model("modelos/model_fitted_aml_h2o_tssignature_ma")

modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # model_fitted_aml_h2o_tssignature_fourier,
    model_fitted_aml_h2o_tssignature_ns,
    # model_fitted_aml_h2o_tssignature_ns_lag,
    # model_fitted_aml_h2o_tssignature_ns_ibr,
    # model_fitted_aml_h2o_tssignature_ns_ibr_lag
    # model_fitted_aml_h2o_tssignature_ns_trm,
    # model_fitted_aml_h2o_tssignature_ns_ipc_trm,
    # model_fitted_aml_h2o_tssignature_ns_ibr_trm
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7,
    model_fitted_aml_h2o_regresors,
    model_fitted_aml_h2o_ma,
    model_fitted_aml_h2o_tssignature_ma
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, mae))


# recetas 16 ----

receta_ml <- recipe(creci_dia ~ ., data = datos_modelar) %>% 
    #step_rm(ipc_lag) %>% 
    #step_rm(ibr) %>% 
    #step_rm(trm_lag) %>%
    #step_rm(ma7) %>%
    #step_timeseries_signature(fecha_corte) %>% 
    #step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) #%>% 
    #step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE)  


receta_ml %>% prep() %>% juice() %>% glimpse()

wlfw_spec_aml_h2o <-  workflow() %>% 
    add_model(model_spec_aml_h2o) %>% 
    add_recipe(receta_ml) 

model_fitted_aml_h2o_reg_ma <- wlfw_spec_aml_h2o %>%
    fit(training(datos_splits))  %>%
    recursive(
        id         = "cod",
        transform  = transformer_function,
        train_tail = panel_tail(training(datos_splits), cod, 30)
    )

model_fitted_aml_h2o_reg_ma %>% 
    save_h2o_model(path = "modelos/model_fitted_aml_h2o_reg_ma", overwrite = TRUE)

model_fitted_aml_h2o_reg_ma <- load_h2o_model("modelos/model_fitted_aml_h2o_reg_ma")

modeltime_tbl <- modeltime_table(
    model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature,
    # model_fitted_aml_h2o_tssignature_fourier,
    model_fitted_aml_h2o_tssignature_ns,
    # model_fitted_aml_h2o_tssignature_ns_lag,
    # model_fitted_aml_h2o_tssignature_ns_ibr,
    # model_fitted_aml_h2o_tssignature_ns_ibr_lag
    # model_fitted_aml_h2o_tssignature_ns_trm,
    # model_fitted_aml_h2o_tssignature_ns_ipc_trm,
    # model_fitted_aml_h2o_tssignature_ns_ibr_trm
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm,
    model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7,
    model_fitted_aml_h2o_regresors,
    model_fitted_aml_h2o_ma,
    model_fitted_aml_h2o_tssignature_ma,
    model_fitted_aml_h2o_reg_ma
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))

modeltime_tbl_cal %>% 
    modeltime_forecast(
        new_data    = testing(datos_splits),
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>% 
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

modeltime_tbl_cal %>% modeltime_accuracy(metric_set = metric_set(rmse, mae))

# Refit ----

modeltime_tbl <- modeltime_table(
    # model_fitted_aml_h2o,
    # model_fitted_aml_h2o_tssignature_ns_ipc_ibr_trm_ma7
    model_h2o_solo_base_refit,
    fecha_regressors_ma_refit
) 

modeltime_tbl_cal <- modeltime_tbl %>% 
    modeltime_calibrate(testing(datos_splits))


refit_tbl <- modeltime_tbl_cal %>%
    modeltime_refit(datos_modelar)

refit_tbl %>%
    modeltime_forecast(
        new_data    = datos_forecast,
        actual_data = datos_modelar,
        keep_data   = TRUE
    ) %>%
    group_by(cod) %>% 
    filter(.index >= today()-years(1)) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE, 
                            .trelliscope_params = list(width = 1000))

refit_tbl %>% modeltime_accuracy(metric_set = metric_set(rmse, mae))

refit_tbl$.model[[1]] %>% 
    save_h2o_model(path = "modelos/solo_base_refit", overwrite = TRUE)

model_h2o_solo_base_refit <- load_h2o_model("modelos/solo_base_refit")

refit_tbl$.model[[2]] %>% 
    save_h2o_model(path = "modelos/fecha_regressors_ma_refit", overwrite = TRUE)

fecha_regressors_ma_refit <- load_h2o_model("modelos/fecha_regressors_ma_refit")

h2o.shutdown(prompt = FALSE)


