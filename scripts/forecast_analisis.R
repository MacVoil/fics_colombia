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
library(tidymodels)
library(future)
library(doFuture)

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
           crecimiento_dia = (precierre_fondo_dia_t/(precierre_fondo_dia_t-rendimientos_abonados)-1),
           crecimiento_dia = if_else(is.infinite(crecimiento_dia)|is.nan(crecimiento_dia),
                                     0,
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
    #filter(fecha_corte >= "2021-01-01") %>% 
    plot_stl_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = c("observed", "season", "trend", "remainder"), .interactive = F)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    #filter(fecha_corte >= "2021-01-01") %>% 
    plot_stl_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = c("trend"), .interactive = T)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% tk_stl_diagnostics(.date_var = fecha_corte, .value = q2) %>% 
    plot_time_series(fecha_corte, trend, .smooth_period = 365*3)



datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte >= "2021-01-01") %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = q2, .lags = 2:30)

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte >= "2021-01-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "wday.lbl" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte >= "2021-01-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "week" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte >= "2021-01-01") %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "month.lbl" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    plot_seasonal_diagnostics(.date_var = fecha_corte, .value = q2, .feature_set = "quarter" )

datos_forecast_limpieza_base_activos_major_n_inv_mean %>% 
    filter(fecha_corte >= "2021-01-01") %>% 
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
    left_join(ipc, by = c("fecha_corte" = "fecha")) %>% 
    mutate(l7 = lag(q2,7))

datos_forecast_limpieza_base_activos_major_n_inv_mean_regresors %>% 
    filter(fecha_corte >= "2021-01-01") %>% 
    drop_na() %>% 
    plot_acf_diagnostics(.date_var = fecha_corte, .value = q2, .ccf_vars = c(ipc, trm, ibr, l7), .show_ccf_vars_only = TRUE, .lags = 0:365, .facet_ncol = 4)

datos_forecast_limpieza_base_activos_major_n_inv %>% 
    group_by(cod) %>% 
    plot_time_series(fecha_corte, crecimiento_dia, .smooth = FALSE, .facet_ncol = 3, .facet_nrow = 3, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000)) 


datos_forecast_limpieza_base_activos_major_n_inv_complete <- datos_forecast_limpieza_base_activos_major_n_inv %>% 
    group_by(cod) %>% 
    pad_by_time(fecha_corte,
        .by = "day", 
        .pad_value = 0,
        .start_date = min(datos_forecast_limpieza_base_activos_major_n_inv$fecha_corte) # <- Add this to pad each group from the beginning
    )

datos_forecast_limpieza_base_activos_major_n_inv_complete %>% 
    group_by(cod) %>% 
    plot_time_series(fecha_corte, crecimiento_dia, .smooth = FALSE, .facet_ncol = 3, .facet_nrow = 3, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000)) 


datos_completos <- datos_forecast_limpieza_base_activos_major_n_inv %>% 
    group_by(cod) %>% 
    future_frame(fecha_corte, .length_out = 30, .bind_data = TRUE) %>% 
    left_join(ibr, by = c("fecha_corte" = "fecha")) %>% 
    left_join(trm, by = c("fecha_corte" = "fecha")) %>% 
    left_join(ipc, by = c("fecha_corte" = "fecha")) %>% 
    mutate(ipc_lag46 = lag(ipc, 46),
           trm_lag56 = lag(trm, 56)) %>% 
    select(-trm, -ipc) %>% 
    ungroup() %>% 
    filter(!is.na(ipc_lag46)) %>% 
    filter(!is.na(trm_lag56))
    
recursive_ma <- function(data){
    data %>%
        group_by(cod) %>%
        tk_augment_slidify(
            .value   = crecimiento_dia,
            .f       = ~mean(.x),
            .period  = c(7),
            .align   = "rigth"
        ) %>%
        ungroup()
}


datos_completos_ma <- datos_completos %>% 
    recursive_ma() %>% 
    group_by(cod) %>% 
    dplyr::slice(-(1:6)) %>% 
    ungroup()

datos_forecast <- datos_completos_ma %>% 
    filter(is.na(crecimiento_dia))

datos_modelar <- datos_completos_ma %>% 
    filter(!is.na(crecimiento_dia))


# recetas ----

receta_ml <- recipe(crecimiento_dia ~ ., data = datos_modelar) %>% 
    step_timeseries_signature(fecha_corte) %>% 
    step_rm(matches("iso|xts|hour|minute|second|am.pm")) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_fourier(fecha_corte, period = c(7, 365/12, 365), K = 2) %>% 
    step_ns(fecha_corte_index.num, deg_free = 4, keep_original_cols =TRUE) %>% 
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
    step_rm(fecha_corte)
    

receta_ml %>% prep() %>% juice() %>% glimpse()

# splits ----

datos_splits <- time_series_split(datos_modelar, assess = 30, cumulative = TRUE)

datos_splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(fecha_corte, crecimiento_dia)

datos_train <- training(datos_splits) %>% 
    group_by(cod) %>%
    mutate(crecimiento_dia = ts_clean_vec(crecimiento_dia, period = 7)) %>%
    ungroup()

datos_train %>% 
    group_by(cod) %>% 
    plot_time_series(fecha_corte, crecimiento_dia, .smooth = FALSE, .facet_ncol = 3, .facet_nrow = 3, .trelliscope = TRUE, 
                     .trelliscope_params = list(width = 1000)) 


# k-folf no secuencial
set.seed(4981)
datos_resamples_kfold <- vfold_cv(datos_train,
                                 v = 10)

datos_resamples_kfold %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(fecha_corte, crecimiento_dia, .facet_ncol = 4)

# k-folf secuencial
# set.seed(4981)
# datos_resamples_kfold_sec <- time_series_cv(datos_train, 
#                                        cumulative = FALSE,
#                                        initial = "3 years",
#                                        assess = "1 month",
#                                        skip = "1 months",
#                                        slice_limit = 10)
# 
# 
# datos_resamples_kfold_sec %>% 
#     tk_time_series_cv_plan() %>% 
#     plot_time_series_cv_plan(fecha_corte, crecimiento_dia, .facet_ncol = 4)

# modelo test ----

model_spec_xgboost_tume <- boost_tree(mode = "regression",
                                      mtry = tune(),
                                      trees = 300,
                                      min_n = tune(),
                                      tree_depth = tune(),
                                      learn_rate = tune(),
                                      loss_reduction = tune()) %>% 
    set_engine("xgboost", device="cuda")
    

wlfw_spec_xgboost <-  workflow() %>% 
    add_model(model_spec_xgboost_tume) %>% 
    add_recipe(receta_ml) 


registerDoFuture()
n_cores <- 10
cl <- parallel::makeCluster(n_cores)

plan(strategy = cluster,
     workers = cl,
     gc = TRUE) 

tictoc::tic()

set.seed(123)
tune_results_xgboost <- tune_grid(object = wlfw_spec_xgboost,
                                  resamples = datos_resamples_kfold,
                                  param_info = extract_parameter_set_dials(wlfw_spec_xgboost) %>% 
                                      update(learn_rate = learn_rate(range = c(0.15,0.5), trans = NULL)) %>% 
                                      update(mtry = mtry(range = c(158L, 214L), trans = NULL)),
                                  grid = 24,
                                  control = control_grid(verbose = TRUE, allow_par = TRUE)
)
tictoc::toc()

parallel::stopCluster(cl)
plan(strategy = sequential, gc = TRUE)
gc()

tune_results_xgboost %>% show_best(metric = "rmse", n = 10)

set.seed(123)
wlfw_fit_xgboost <- wlfw_spec_xgboost %>% 
    finalize_workflow(parameters = select_best(tune_results_xgboost,metric = "rmse")) %>% 
    fit(datos_train) %>% 
    recursive(
        id         = "cod",
        transform  = recursive_ma,
        train_tail = panel_tail(datos_train, cod, 30)
    )

submodel_tbl <- modeltime_table(wlfw_fit_xgboost)

submodel_tbl %>% modeltime_forecast(new_data = testing(datos_splits),
                                                 actual_data = datos_modelar,
                                                 keep_data = TRUE) %>% 
    group_by(cod) %>% 
    plot_modeltime_forecast(.trelliscope = TRUE)
