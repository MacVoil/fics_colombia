options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(TTR)
library(readxl)
library(dtwclust)
library(furrr)

source("scripts/facts_dims.R")

datos <- get_fics_extended()

dims <- get_dim_fics()

datos_dtw <- datos %>% 
    group_by(cod) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n == max(n)) %>% 
    select(fecha_corte, 
           cod,
           rm,
           rent_30,
           rent_365,
           vol_30,
           vol_365) %>% 
    group_by(cod) %>% 
    mutate(rm = rm != 0,
           mov_per_30 = slidify_vec(
               .x      = rm,
               .period = 30,
               .f      = ~ sum(.)/30,
               .align  = "rigth")) %>% 
    ungroup() %>% 
    select(-rm) %>% 
    drop_na()


datos_dtw_std <- datos_dtw %>% 
    mutate(across(c(rent_30:mov_per_30), ~ standardize_vec(., silent = TRUE)))

datos_dtw_std_list <- datos_dtw_std %>%
    select(-fecha_corte) %>%
    group_by(cod) %>%
    group_map(~as.matrix(.x[-1]), .keep = TRUE) %>%
    set_names(unique(datos_dtw_std$cod))

get_civ <- function(k){
    c(k_i = k, tsclust(datos_dtw_std_list, k = k, distance = "dtw2", seed = 4981) %>% 
        cvi())
}

tictoc::tic()
plan(multisession, workers = availableCores(logical = FALSE))

metricas_clust <- future_map_dfr(3:14, ~  get_civ(.x))

plan(sequential)
tictoc::toc()


ggplot(metricas_clust, aes(x = k_i, y = SF)) +
    geom_line() +
    geom_point(shape = 19)


mvc <- tsclust(datos_dtw_std_list, k = 9L, 
               distance = "sdtw", seed = 4981)


plot(mvc)

clusters <- tibble(cod = names(datos_dtw_std_list), cluster = mvc@cluster)
mvc@cluster %>% table()

datos_cluster <- datos_dtw %>% 
    left_join(clusters)

datos_cluster %>% filter(cluster == 3) %>%  
    plot_time_series(fecha_corte, rent_30, .color_var = cod, .smooth = FALSE)   
