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
