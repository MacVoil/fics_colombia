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
           crecimiento_dia,
           rm) %>% 
    group_by(cod) %>% 
    mutate(rm = rm != 0,
           mov_per = sum(rm)/n()) %>% 
    ungroup() %>% 
    select(-rm) %>% 
    drop_na()


datos_dtw_std <- datos_dtw %>% 
    mutate(across(c(crecimiento_dia:mov_per), ~ standardize_vec(., silent = TRUE)))


# DTW ----
datos_dtw_std_list <- datos_dtw_std %>%
    select(-fecha_corte) %>%
    group_by(cod) %>%
    group_map(~as.matrix(.x[-1]), .keep = TRUE) %>%
    set_names(unique(datos_dtw_std$cod))

# get_civ <- function(k){
#     c(k_i = k, tsclust(datos_dtw_std_list, k = k, distance = "dtw2", seed = 4981) %>% 
#         cvi())
# }

# tictoc::tic()
# plan(multisession, workers = availableCores(logical = FALSE))
# 
# metricas_clust <- future_map_dfr(3:14, ~  get_civ(.x))
# 
# plan(sequential)
# tictoc::toc()
# 
# 
# ggplot(metricas_clust, aes(x = k_i, y = SF)) +
#     geom_line() +
#     geom_point(shape = 19)


mvc <- tsclust(datos_dtw_std_list, k = 9L, 
               distance = "sdtw", seed = 4981)


plot(mvc)

clusters <- tibble(cod = names(datos_dtw_std_list), cluster = mvc@cluster)
mvc@cluster %>% table()

datos_cluster <- datos_dtw %>% 
    left_join(clusters)

datos_cluster %>% filter(cluster == 3) %>%  
    plot_time_series(fecha_corte, crecimiento_dia, .color_var = cod, .smooth = FALSE) 

# Tradicional ----

datos_anchos <- datos_dtw_std %>% 
    pivot_wider(names_from = fecha_corte, values_from = c(crecimiento_dia:mov_per)) %>% 
    select(cod:`mov_per_2022-02-18`)

datos_pca <- prcomp(datos_anchos[,-1])

as.data.frame(round(summary(datos_pca)[[6]],3))

pca_matrix <- datos_pca$x[,1:13]

row.names(pca_matrix) <- datos_anchos$cod

as.data.frame(round(pca_matrix,2))

umap_data <- umap(pca_matrix, n_components = 2, random_state = 4981)

umap_data$layout %>% head()

umap_data_tibble <- as_tibble(umap_data$layout) %>% 
    bind_cols(datos_anchos %>% select(cod))


plot_ly(umap_data_tibble, 
        x = ~V1, 
        y = ~V2,
        type = 'scatter', 
        mode = 'markers',
        text = ~cod) 

umap_data_tibble_dtw <- umap_data_tibble %>% 
    left_join(clusters) %>% 
    mutate(cluster = as_factor(cluster))

plot_ly(umap_data_tibble_dtw, 
        x = ~V1, 
        y = ~V2, 
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster) 

# PAM ----

# get_sil <- function(k){
#   tibble(k_i = k, sil = cluster::pam(pca_matrix,k)$silinfo$avg.width)
# }
# 
# 
# pam_sil <- map_dfr(3:15, ~ get_sil(.x))
# 
# ggplot(pam_sil, aes(x = k_i, y = sil)) +
#     geom_line() +
#     geom_point(shape = 19)

datos_pam <- cluster::pam(pca_matrix,9)
datos_pam$clustering %>% table()
table_datos_pam <- tibble(cod = datos_pam$clustering %>% names, cluster = datos_pam$clustering)

umap_data_tibble_pam <- umap_data_tibble %>% 
    left_join(table_datos_pam) %>% 
    mutate(cluster = as_factor(cluster))

plot_ly(umap_data_tibble_pam, 
        x = ~V1, 
        y = ~V2,
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        colors = RColorBrewer::brewer.pal(11, "Set3"))

## Hierarchical

datos_dist <- dist(pca_matrix)

datos_hc <- hclust(datos_dist,  "ward.D2") %>% 
    cutree(k = 9)

datos_hc %>% table()

table_datos_hc <- tibble(cod = datos_hc %>% names(), cluster = datos_hc)

umap_data_tibble_hc <- umap_data_tibble %>% 
    left_join(table_datos_hc) %>% 
    mutate(cluster = as_factor(cluster))

plot_ly(umap_data_tibble_hc, 
        x = ~V1, 
        y = ~V2, 
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        colors = RColorBrewer::brewer.pal(11, "Set3"))

## Mclust ----

datos_mc <- Mclust(pca_matrix, 9)$classification

datos_mc %>% table()

table_datos_mc <- tibble(cod = datos_mc %>% names(), cluster = datos_mc)

umap_data_tibble_mc <- umap_data_tibble %>% 
    left_join(table_datos_mc) %>% 
    mutate(cluster = as_factor(cluster))

plot_ly(umap_data_tibble_mc, 
        x = ~V1, 
        y = ~V2, 
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        colors = RColorBrewer::brewer.pal(11, "Set3"))

## dice (ensemble) ----

# datos_dice <- dice(pca_matrix,
#                 nk = 11,
#                 algorithms = c("hc","pam","gmm"),
#                 hc.method = "ward.D2",
#                 trim = T,
#                 reweigh = T,
#                 n = 2,
#                 cons.funs = "CSPA",
#                 nmf.method = "lee",
#                 prep.data = "none",
#                 reps = 20)

datos_dice <- dice(pca_matrix,
                   nk = 5,
                   #algorithms = c("hc","diana","km"),
                   #algorithms = c("hc","pam","diana","km","som"),
                   #algorithms = c("hc",  "gmm", "ap",  "som"),
                   algorithms = c("pam", "ap"),
                   hc.method = "ward.D2",
                   trim = T,
                   reweigh = T,
                   n = 11,
                   cons.funs = "CSPA",
                   nmf.method = "lee",
                   prep.data = "none",
                   reps = 100,
                   seed = 4981,
                   seed.data = 4981)


datos_dice$clusters %>% table()

table_datos_dice <- tibble(cod = rownames(datos_dice$clusters))  %>% bind_cols(as_tibble(datos_dice$clusters))

umap_data_tibble_dice <- umap_data_tibble %>% 
    left_join(table_datos_dice) %>% 
    mutate(cluster = as_factor(CSPA))

plot_ly(umap_data_tibble_dice, 
        x = ~V1, 
        y = ~V2, 
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        colors = RColorBrewer::brewer.pal(11, "Set3")) 

datos_cluster_dice <- datos %>% 
    drop_na() %>% 
    left_join(table_datos_dice)

datos_cluster_dice %>% filter(CSPA == 11) %>%  
    plot_time_series(fecha_corte, rent_365, .color_var = cod, .smooth = FALSE) 



######

# # Elementos dados
# elementos <- c("hc", "pam", "gmm", "diana", "km", "ap", "som", "cmeans")
# 
# # Crear todas las combinaciones posibles de longitud 1 a la longitud de elementos
# todas_combinaciones <- unlist(lapply(1:length(elementos), function(x) {
#     combn(elementos, x, simplify = FALSE)
# }), recursive = FALSE)
# 
# 
# # Imprimir la lista de todas las combinaciones
# print(todas_combinaciones)
# 
# get_sil <- function(i){
#     tibble(n=i) %>% bind_cols(dice(umap_data$layout,
#                            nk = 5,
#                            algorithms = todas_combinaciones[[i]],
#                            hc.method = "ward.D2",
#                            trim = T,
#                            reweigh = T,
#                            n = 11,
#                            cons.funs = "CSPA",
#                            nmf.method = "lee",
#                            prep.data = "none",
#                            reps = 100,
#                            seed = 4981,
#                            seed.data = 4981)$indices$ii$`5`["CSPA",-1])
# 
# }
# 
# get_sil(225)
# 
# 
# tictoc::tic()
# plan(multisession, workers = availableCores())
# 
# alg_table <- future_map_dfr(9:255, ~  get_sil(.x))
# 
# plan(sequential)
# tictoc::toc()
# 
# 
# alg_table_ranks <- alg_table %>%
#     select(n,
#            calinski_harabasz,
#            dunn,
#            gamma,
#            silhouette,
#            davies_bouldin,
#            sd,
#            s_dbw,
#            c_index,
#            Compactness,
#            Connectivity
#     ) %>%
#     mutate(across(calinski_harabasz:silhouette, ~ dense_rank(desc(.))),
#            across(davies_bouldin:Connectivity, ~ dense_rank(.)),
#            mean_rank = (calinski_harabasz + dunn + gamma + c_index + silhouette + Compactness + Connectivity + davies_bouldin + sd + s_dbw)/10
# 
#     )
# # # 

datos_pam_umap <- cluster::pam(umap_data$layout,7)
datos_pam_umap$clustering %>% table()
table_datos_pam_umap <- tibble(cod = datos_pam_umap$clustering %>% names(), cluster = datos_pam_umap$clustering)

umap_data_tibble_pam_umap <- umap_data_tibble %>% 
    left_join(table_datos_pam_umap) %>% 
    mutate(cluster = as_factor(cluster))

plot_ly(umap_data_tibble_pam_umap, 
        x = ~V1, 
        y = ~V2,
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        colors = RColorBrewer::brewer.pal(11, "Set3"))

datos_cluster_dice <- datos %>% 
    drop_na() %>% 
    left_join(table_datos_pam_umap)

datos_cluster_dice %>% filter(cluster == 7) %>%  
    plot_time_series(fecha_corte, rent_365, .color_var = cod, .smooth = FALSE) 

