get_fics <- function(
        from = today() - years(2),
        to = today()) {
    
    # Transformanos str a date
    from <- ymd(from)
    to <- ymd(to)
    
    # Estructura url
    url_head <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT fecha_corte, tipo_entidad, codigo_entidad, subtipo_negocio, codigo_negocio, tipo_participacion, rendimientos_abonados, precierre_fondo_dia_t, numero_unidades_fondo_cierre, aportes_recibidos, retiros_redenciones, anulaciones, valor_fondo_cierre_dia_t, numero_inversionistas where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' and subtipo_negocio not in('7')  LIMIT 100000000") %>% 
        URLencode()
    
    # A data frame
    fromJSON(url_head) %>% 
        mutate(across(rendimientos_abonados:numero_inversionistas, as.numeric),
               fecha_corte = ymd_hms(fecha_corte) %>% 
                   ymd()) %>%  
        arrange(fecha_corte,
                codigo_entidad,
                subtipo_negocio,
                codigo_negocio,
                tipo_participacion) %>% 
        unite("cod", tipo_entidad, codigo_entidad, subtipo_negocio, codigo_negocio) %>% 
        mutate(rm = aportes_recibidos-retiros_redenciones+anulaciones) %>% 
        select(-aportes_recibidos, -retiros_redenciones, -anulaciones)
}

get_dim_fics <- function(
        from = today() - years(2),
        to = today()) {
    
    # Transformanos str a date
    from <- ymd(from)
    to <- ymd(to)


dim_entidad <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT distinct tipo_entidad, nombre_tipo_entidad, codigo_entidad, nombre_entidad LIMIT 100000000") %>% 
    URLencode() %>% 
    fromJSON() %>% 
    arrange(codigo_entidad) %>% 
    mutate(cod_entidad = str_c(tipo_entidad, "_", codigo_entidad)) %>% 
    select(cod_entidad, tipo_entidad, codigo_entidad, nombre_tipo_entidad, nombre_entidad) %>% 
    mutate(across(4:5, str_to_upper)) %>% 
    distinct(cod_entidad, .keep_all = T) %>% 
    select(-cod_entidad) %>% 
    distinct(tipo_entidad, codigo_entidad, .keep_all = TRUE)


dim_producto <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT distinct subtipo_negocio, nombre_subtipo_patrimonio LIMIT 100000000") %>% 
    URLencode() %>% 
    fromJSON() %>% 
    distinct(subtipo_negocio, .keep_all = TRUE)

dim_negocio <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT distinct tipo_entidad, codigo_entidad, codigo_negocio, nombre_patrimonio LIMIT 100000000") %>% 
    URLencode() %>% 
    fromJSON() %>% 
    distinct(tipo_entidad, codigo_entidad, codigo_negocio, .keep_all = TRUE) %>% 
    mutate(nombre_patrimonio = str_to_upper(nombre_patrimonio))
    
    # Estructura url
    url_head <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT distinct tipo_entidad, codigo_entidad, subtipo_negocio, codigo_negocio where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' LIMIT 100000000") %>% 
        URLencode()
    
    # A data frame
    dim_fics <- fromJSON(url_head) 

dim_fics %>% 
    distinct() %>% 
    left_join(dim_entidad,
              by = c("tipo_entidad", "codigo_entidad")) %>% 
    left_join(dim_producto,
              by = "subtipo_negocio") %>% 
    left_join(dim_negocio,
              by = c("tipo_entidad", "codigo_entidad", "codigo_negocio")) %>% 
    unite("cod", 
          tipo_entidad,
          codigo_entidad,
          subtipo_negocio,
          codigo_negocio)
}

get_fics_extended <- function(
        from = today() - years(2),
        to = today(),
        min_par = TRUE
) {
    from <- ymd(from)
    to <- ymd(to)
    
    datos_fics <- get_fics(from = from, 
                           to = to) %>% 
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
    
    max_date <- max(datos_fics$fecha_corte)
    
    max_date_minus_4_weeks <- max_date - weeks(4)
    
    datos_hist <- datos_fics %>% 
        filter(fecha_corte < max_date_minus_4_weeks)
    
    
    datos_mes <- datos_fics %>% 
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
    
    datos_base <- bind_rows(datos_hist,datos_mes)
    
    if (min_par) {
        fondos_activos <- datos_base %>% 
            group_by(cod) %>% 
            mutate(max_fecha = max(fecha_corte)) %>% 
            ungroup() %>% 
            filter(max_fecha == max(fecha_corte)) %>% 
            pull(cod) %>% 
            unique()
        
        datos <- bind_rows(datos_hist,datos_mes) %>% 
            group_by(cod, tipo_participacion) %>% 
            mutate(n_fondo = n()) %>% 
            ungroup() %>% 
            group_by(cod) %>% 
            mutate(n_fondo = max(n_fondo)) %>% 
            ungroup() %>% 
            filter(n_fondo > 365) %>%  # Fondos con un año o más de creados
            select(-n_fondo) %>% 
            filter(cod %in% fondos_activos) %>% 
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
            ungroup()
        
        fondos_base <- datos %>% 
            drop_na() %>% 
            filter(fecha_corte == max(fecha_corte)) %>% 
            group_by(cod) %>% 
            filter(numero_inversionistas == max(numero_inversionistas)) %>% 
            filter(numero_inversionistas >0) %>% 
            filter(valor_fondo_cierre_dia_t == max(valor_fondo_cierre_dia_t)) %>% 
            ungroup() %>% 
            select(cod, tipo_participacion) %>% 
            distinct(cod, .keep_all = TRUE)
        
        datos <- datos %>% 
            semi_join(fondos_base, by = c("cod", "tipo_participacion")) %>% 
            group_by(cod, tipo_participacion)
    } else {
        datos <- datos_base %>% 
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
            filter(n>=365)
    }
    
    datos %>% 
        mutate(
            rent_15 = slidify_vec(
                .x      = crecimiento_dia,
                .period = 15,
                .f      = ~ (prod(.)^(365/15))-1,
                .align  = "rigth"),
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
            vol_15 = slidify_vec(
                .x      = crecimiento_dia,
                .period = 15,
                .f      = ~sd(.-1)*sqrt(365),
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
            EMA15 = EMA((crecimiento_dia-1), n=15),
            EMA30 = EMA((crecimiento_dia-1), n=30),
            EMA90 = EMA((crecimiento_dia-1), n=90),
            EMA180 = EMA((crecimiento_dia-1), n=180),
            EMA365 = EMA((crecimiento_dia-1), n=365),
            cobert_ries_15 = rent_15/vol_15,
            cobert_ries_30 = rent_30/vol_30,
            cobert_ries_90 = rent_90/vol_90,
            cobert_ries_180 = rent_180/vol_180,
            cobert_ries_365 = rent_365/vol_365,
            rm = round(rm, 1),
            cre_rm_30 = slidify_vec(
                .x      = rm,
                .period = 30,
                .f      = ~ (sum(.)),
                .align  = "rigth"),
            cre_rm_30 = cre_rm_30/lag(valor_fondo_cierre_dia_t,30)
        ) %>% 
        ungroup() %>% 
        select(-n)
}
