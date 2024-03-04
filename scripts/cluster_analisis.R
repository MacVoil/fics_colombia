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


source("scripts/facts_dims.R")

datos <- get_fics_extended()

dims <- get_dim_fics()

datos_dtw <- datos %>% 
  group_by(cod) %>% 
  mutate(n = n()) %>% 
  mutate(any_mov = rm != 0,
         mov_per = sum(any_mov)/n) %>% 
  filter(mov_per >= 0.4) %>% 
  ungroup() %>%
  filter(n == max(n)) %>% 
      select(fecha_corte, 
             cod,
             crecimiento_dia) %>%
  drop_na()


datos_dtw_std <- datos_dtw %>% 
    mutate(across(crecimiento_dia, ~ standardize_vec(., silent = TRUE)))


# DTW ----
datos_dtw_std_list <- datos_dtw_std %>%
    pivot_wider(names_from = fecha_corte, values_from = crecimiento_dia)


get_civ <- function(k){
    c(k_i = k, tsclust(datos_dtw_std_list[,-1], k = k, distance = "sdtw", seed = 4981) %>%
        cvi())
}

tictoc::tic()

metricas_clust <- future_map_dfr(3:12, ~  get_civ(.x))

tictoc::toc()


ggplot(metricas_clust, aes(x = k_i, y = DBstar)) +
    geom_line() +
    geom_point(shape = 19)


mvc <- tsclust(datos_dtw_std_list[,-1], k = 8L, 
               distance = "sdtw", seed = 4981)


plot(mvc)

clusters <- tibble(cod = datos_dtw_std_list$cod, cluster = mvc@cluster)
mvc@cluster %>% table()

datos_cluster <- datos %>% 
    left_join(clusters) %>% 
  drop_na()


datos_cluster %>% 
  group_by(cluster) %>% 
  plot_time_series(fecha_corte, rent_30, .color_var = cod, .smooth = FALSE, .facet_ncol = 3) %>%
  layout(showlegend = FALSE)


umap_data <- umap(datos_dtw_std_list[,-1], n_components = 2, random_state = 4981)
umap_data$layout %>% head()

umap_data_tibble <- as_tibble(umap_data$layout) %>% 
  bind_cols(datos_dtw_std_list %>% select(cod))


plot_ly(umap_data_tibble, 
        x = ~V1, 
        y = ~V2,
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        size = 1) 

umap_data_tibble_dtw <- umap_data_tibble %>% 
  left_join(clusters) %>% 
  mutate(cluster = as_factor(cluster))

plot_ly(umap_data_tibble_dtw, 
        x = ~V1, 
        y = ~V2, 
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        marker = list(size = 15),
        opacity = 0.75)

# Tradicional ----


# datos_pca <- prcomp(datos_anchos[,-1])
# 
# as.data.frame(round(summary(datos_pca)[[6]],3))
# 
# pca_matrix <- datos_pca$x[,1:37]
# 
# row.names(pca_matrix) <- datos_anchos$cod
# 
# as.data.frame(round(pca_matrix,2))


#########################################
# clust_hd <- hdbscan(datos_dtw_std_list[,-1], 2)
# clust_hd$cluster %>% table()
# plot(clust_hd)
# 
# clusters <- tibble(cod = datos_anchos$cod, cluster = clust_hd$cluster)
# 
# datos_cluster <- datos %>% 
#   left_join(clusters)
# 
# datos_cluster %>% filter(cluster == 0) %>%  
#   drop_na() %>% 
#   plot_time_series(fecha_corte, rent_365, .color_var = cod, .smooth = FALSE) 
# 
# 
# umap_data_tibble_dtw <- umap_data_tibble %>% 
#   left_join(clusters) %>% 
#   mutate(cluster = as_factor(cluster))
# 
# plot_ly(umap_data_tibble_dtw, 
#         x = ~V1, 
#         y = ~V2, 
#         type = 'scatter', 
#         mode = 'markers',
#         text = ~cod,
#         color = ~cluster) 


##################
matrix_datos_anchos <- datos_dtw_std_list[,-1]
rownames(matrix_datos_anchos) <-  datos_dtw_std_list$cod

# PAM ----

get_sil <- function(k){
  tibble(k_i = k, sil = cluster::pam(matrix_datos_anchos,k)$silinfo$avg.width)
}


pam_sil <- map_dfr(3:12, ~ get_sil(.x))

ggplot(pam_sil, aes(x = k_i, y = sil)) +
    geom_line() +
    geom_point(shape = 19)

datos_pam <- cluster::pam(matrix_datos_anchos,6)
datos_pam$clustering %>% table()
table_datos_pam <- tibble(cod = datos_pam$clustering %>% names(), cluster = datos_pam$clustering)

umap_data_tibble_pam <- umap_data_tibble %>% 
    left_join(table_datos_pam) %>% 
    mutate(cluster = as_factor(cluster))

datos_cluster <- datos %>% 
  left_join(table_datos_pam) %>% 
  drop_na()


datos_cluster %>% 
  group_by(cluster) %>% 
  plot_time_series(fecha_corte, rent_30, .color_var = cod, .smooth = FALSE, .facet_ncol = 3) %>%
  layout(showlegend = FALSE)


plot_ly(umap_data_tibble_pam, 
        x = ~V1, 
        y = ~V2,
        type = 'scatter', 
        mode = 'markers',
        text = ~cod,
        color = ~cluster,
        colors = RColorBrewer::brewer.pal(11, "Set3"),
        marker = list(size = 15),
        opacity = 0.75)

#########
umap_data_clust <- umap(datos_dtw_std_list[,-1], n_components = 6, random_state = 4981)
umap_data_clust$layout %>% head()

matrix_umap <- umap_data_clust$layout
rownames(matrix_umap) <-  datos_dtw_std_list$cod
##########


datos_pam <- cluster::pam(matrix_umap,6)
datos_pam$clustering %>% table()
table_datos_pam <- tibble(cod = datos_pam$clustering %>% names(), cluster = datos_pam$clustering)

datos_cluster <- datos %>% 
  left_join(table_datos_pam) %>% 
  drop_na()


datos_cluster %>% 
  group_by(cluster) %>% 
  plot_time_series(fecha_corte, rent_30, .color_var = cod, .smooth = FALSE, .facet_ncol = 3) %>%
  layout(showlegend = FALSE)


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
        colors = RColorBrewer::brewer.pal(11, "Set3"),
        marker = list(size = 15),
        opacity = 0.75)


#########

# Elementos dados
elementos <- c("hc", "pam", "gmm", "diana", "km", "ap", "som", "cmeans")

# Crear todas las combinaciones posibles de longitud 1 a la longitud de elementos
todas_combinaciones <- unlist(lapply(1:length(elementos), function(x) {
  combn(elementos, x, simplify = FALSE)
}), recursive = FALSE)


# Imprimir la lista de todas las combinaciones
print(todas_combinaciones)


comb_2 <- expand.grid(36:255, 3:9)

get_sil <- function(i){
  comb <- comb_2[i,1]
  k <- comb_2[i,2]
  
  umap_data_clust <- umap(datos_dtw_std_list[,-1], n_components = k, random_state = 4981)
  matrix_umap <- umap_data_clust$layout
  rownames(matrix_umap) <-  datos_dtw_std_list$cod
  
  
  tibble(n=i) %>% bind_cols(calinski_harabasz = dice(matrix_umap,
                                 nk = k,
                                 algorithms = todas_combinaciones[[comb]],
                                 hc.method = "ward.D2",
                                 trim = T,
                                 reweigh = T,
                                 n = 11,
                                 cons.funs = "CSPA",
                                 nmf.method = "lee",
                                 prep.data = "none",
                                 reps = 100,
                                 seed = 4981,
                                 seed.data = 4981)$indices$ii[[1]]["CSPA",-1]$calinski_harabasz)
  
}

get_sil(dim(comb_2)[1])


tictoc::tic()
plan(multisession, workers = availableCores())

alg_table <- future_map_dfr(1:dim(comb_2)[1], ~  get_sil(.x))

plan(sequential)
tictoc::toc()

best_calinski_harabasz <- alg_table %>% 
  filter(calinski_harabasz == max(calinski_harabasz)) %>% 
  arrange(n) %>% 
  distinct(calinski_harabasz, .keep_all = TRUE) %>% 
  pull(n)



# alg_table_ranks <- alg_table %>%
#   select(n,
#          calinski_harabasz,
#          dunn,
#          gamma,
#          silhouette,
#          davies_bouldin,
#          sd,
#          s_dbw,
#          c_index,
#          Compactness,
#          Connectivity
#   ) %>%
#   mutate(across(calinski_harabasz:silhouette, ~ dense_rank(desc(.))),
#          across(davies_bouldin:Connectivity, ~ dense_rank(.)),
#          mean_rank = (calinski_harabasz + dunn + gamma + c_index + silhouette + Compactness + Connectivity + davies_bouldin + sd + s_dbw)/10
#          
#   )
# # # # 


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

umap_data_clust <- umap(datos_dtw_std_list[,-1], 
                        n_components = comb_2[best_calinski_harabasz,]$Var2, 
                        random_state = 4981)
matrix_umap <- umap_data_clust$layout
rownames(matrix_umap) <-  datos_dtw_std_list$cod

datos_dice <- dice(matrix_umap,
                   nk = comb_2[best_calinski_harabasz,]$Var2,
                   algorithms = todas_combinaciones[[n_components = comb_2[best_calinski_harabasz,]$Var1]],
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
        colors = RColorBrewer::brewer.pal(12, "Set3"),
        marker = list(size = 15),
        opacity = 0.75) 

datos_cluster_dice <- datos %>% 
  left_join(table_datos_dice) %>% 
  left_join(dims) %>% 
  drop_na() 

datos_cluster_dice %>% 
  group_by(CSPA) %>% 
  plot_time_series(fecha_corte, rent_30, .color_var = nombre_patrimonio, .smooth = FALSE, .facet_ncol = 1, .trelliscope = TRUE, 
                   .trelliscope_params = list(width = 1000)) 

representacion_clustes <- datos_cluster_dice %>% 
  group_by(CSPA, fecha_corte) %>% 
  summarise(q1 = quantile(rent_30, 0.25),
         q2 = quantile(rent_30, 0.5),
         q3 = quantile(rent_30, 0.75),
         ) %>% 
  ungroup()

representacion_clustes %>% 
  plot_time_series(fecha_corte, q2, .color_var = CSPA, .smooth = FALSE) 

p <- representacion_clustes %>% 
  ggplot(aes(x =fecha_corte, y = q2, col = factor(CSPA), group =  factor(CSPA), fill= factor(CSPA))) +
  geom_line() +
  geom_ribbon(aes(ymin=q1, ymax=q3), alpha = 0.5)

ggplotly(p, dynamicTicks = TRUE)

######


