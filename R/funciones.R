# From: https://crimebythenumbers.com/scrape-table.html
scrape_pdf <- function(list_of_tables,
                      table_number,
                      number_columns,
                      column_names) {
  data <- list_of_tables[table_number]
  data <- gsub('°', ' ', data)
  data <- trimws(data)
  data <- strsplit(data, "\n")
  data <- data[[1]]
  # data <- data[grep("Miami", data):
  #                grep("Nationwide Total", data)]
  data <- str_split_fixed(data, " {2,}", number_columns)
  data <- data.frame(data)
  names(data) <- column_names
  return(data)
}
# Limpiar coordenadas
limpiar_coord <- function(mi_vector = NULL, sufijo = NULL) {
  library(parzer)
  utm_x_valida <- !grepl(' ', mi_vector) &
    nchar(as.character(as.integer(mi_vector))) == 6 &
    grepl(paste0('^', as.character(18:65), collapse='|'), mi_vector)
  utm_y_valida <- !grepl(' ', mi_vector) &
    nchar(as.character(as.integer(mi_vector))) == 7 &
    grepl(paste0('^', as.character(185:225), collapse='|'), mi_vector)
  lon_valida <- grepl(' ', mi_vector) &
    grepl(paste0('^', as.character(68:72), collapse='|'), mi_vector)
  lat_valida <- grepl(' ', mi_vector) &
    grepl(paste0('^', as.character(17:21), collapse='|'), mi_vector)
  lat_espacios <- ifelse(lat_valida,parse_lat(mi_vector), NA) # Sexagesimal con espacios
  lon_espacios <- ifelse(lon_valida, parse_lon(mi_vector), NA) # Sexagesimal con espacios
  lon_espacios <- ifelse(lon_espacios < 0, lon_espacios, (-1) * lon_espacios)
  utm_x <- ifelse(utm_x_valida, mi_vector, NA)
  utm_y <- ifelse(utm_y_valida, mi_vector, NA)
  foo <- cbind(
    lon_dd = if(any(lon_valida)) lon_espacios else NULL,
    lat_dd = if(any(lat_valida)) lat_espacios else NULL,
    utm_x = if(any(utm_x_valida)) utm_x else NULL,
    utm_y = if(any(utm_y_valida)) utm_y else NULL)
  salida <- sapply(as.data.frame(foo), as.numeric)
  colnames(salida) <- paste0(colnames(salida), '_', sufijo)
  return(salida)
}

# Objeto "fuente", para categorías
# fuente <- c('marginalmente idóneo', 'moderadamente idóneo', 'idóneo', 'altamente idóneo')
fuente <- c('marginalmente prioritario', 'moderadamente prioritario', 'prioritario', 'imprescindible')
fuente_eng <- c('marginally prioritized', 'moderately prioritized', 'prioritized', 'essential')

# Función de reclasificación
reclasificar <- function(vectorial, campo, umbrales, campo_indice = 'hex_id',
                         ord_cat = 'in', nombre = NULL){
  #' @param vectorial Objeto de clase \code{sf} que contiene
  #'   la representación territorial de la variable.
  #' @param campo Cadena de caracteres conteniendo el nombre del campo a reclasificar.
  #' @param umrables Vector de límites de superiores de cada categoría.
  #'   Los límites mínimo y máximo no necesitan incluirse. La función los deriva.
  #' @param ord_cat Caracter definiendo la opción de ordenación de categorías.
  #'   'in': altamente idóneo, mod. idóneo, marg. idóneo, no idóneo
  #'   'ni': no idóneo, marg. idóneo, mod. idóneo, altamente idóneo
  #'   'nin': no idóneo, altamente idóneo, mod. idóneo, marg. idóneo, no idóneo
  #'   'nin_rev': no idóneo, marg. idóneo, mod. idóneo, altamente idóneo, no idóneo

  #' @example
  # source('R/funciones.R')
  # library(sf)
  # res_h3 <- 7 #Escribir un valor entre 4 y 7, ambos extremos inclusive
  # ruta_ez_gh <- 'https://raw.githubusercontent.com/geofis/zonal-statistics/'
  # ez_ver <- 'da5b4ed7c6b126fce15f8980b7a0b389937f7f35/'
  # ind_esp_url <- paste0(ruta_ez_gh, ez_ver, 'out/all_sources_all_variables_res_', res_h3, '.gpkg')
  # ind_esp_url
  # if(!any(grepl('^ind_esp$', ls()))){
  #   ind_esp <- st_read(ind_esp_url, optional = T)
  #   st_geometry(ind_esp) <- "geometry"
  #   ind_esp <- st_transform(ind_esp, 32619)
  # }
  # if(!any(grepl('^pais_url$', ls()))){
  #   pais_url <- paste0(ruta_ez_gh, ez_ver, 'inst/extdata/dr.gpkg')
  #   pais <- st_read(pais_url, optional = T, layer = 'pais')
  #   st_geometry(pais) <- "geometry"
  #   pais <- st_transform(pais, 32619)
  # }
  # if(!any(grepl('^ind_esp_inters$', ls()))){
  #   ind_esp_inters <- st_intersection(pais, ind_esp)
  #   colnames(ind_esp_inters) <- colnames(ind_esp)
  #   ind_esp_inters$area_sq_m <- units::drop_units(st_area(ind_esp_inters))
  #   ind_esp_inters$area_sq_km <- units::drop_units(st_area(ind_esp_inters))/1000000
  #   ind_esp_inters
  # }
  # # Para "OSM-DIST mean"
  # if(!any(grepl('^osm_rcl$', ls()))){
  #   osm_rcl <- reclasificar(vectorial = ind_esp_inters, campo = 'OSM-DIST mean',
  #                           umbrales = c(50, 200, 500, 5000),
  #                           nombre = 'Distancia a accesos OSM',
  #                           ord_cat = 'nin_rev')
  #   osm_rcl$mapa +
  #     geom_sf(data = pais, fill = 'transparent', lwd = 0.5, color = 'grey50')
  #   osm_rcl$intervalos_y_etiquetas
  # }
  # if(any(grepl('^osm_rcl$', ls()))){
  #   clipr::write_clip(osm_rcl$intervalos_y_etiquetas)
  # }
  
  # Paquetes
  library(sf)
  library(ggplot2)
  library(dplyr)
  
  # Definir nombre
  if(is.null(nombre)) nombre <- campo
  
  # Función para construir categorías
  construir_categorias <- function(ordenacion = ord_cat){
    categorias <- switch(ordenacion,
           'mim' = c(#Antiguo 'nin'
             fuente, fuente[1]
             ),
           'mim_rev' = c(#Antiguo 'nin_rev'
             fuente[1], rev(fuente)
           ),
           'im' = rev(fuente), #Antiguo 'in'
           'mi' = fuente #Antiguo 'ni'
    )
    return(categorias)
  }
  
  # Construir categorías
  categorias <- construir_categorias()
  
  # Añadir umbrales extremos
  umbrales_ampliados <- c(min(vectorial[[campo]], na.rm = T), umbrales, max(vectorial[[campo]], na.rm = T))
  umbrales_ampliados
  if(length(umbrales) + 1 != length(categorias)) 
    stop('El número de categorías es incorrecto')
  
  # Crear intervalos
  campo_salida_interv <- paste(campo, 'intervalos')
  campo_salida_etiq <- paste(campo, 'etiquetas')
  vectorial[[campo_salida_interv]] <- cut(
    vectorial[[campo]],
    umbrales_ampliados, include.lowest = T)
  vectorial[[campo_salida_etiq]] <- factor(vectorial[[campo_salida_interv]],
                                           labels = categorias)
  # vectorial[[paste(campo, 'enteros_ordenados', sep = '_')]] <- as.integer(
  vectorial[[paste(campo, 'puntuación')]] <- as.integer(
    factor(vectorial[[campo_salida_etiq]],
           levels = fuente))
  # vectorial <- vectorial[c(campo_indice, campo_salida_interv, campo_salida_etiq, paste(campo, 'enteros_ordenados', sep = '_'))]
  vectorial <- vectorial[c(campo_indice, campo_salida_interv, campo_salida_etiq, paste(campo, 'puntuación'))]
  
  # Crear mapa
  val_col <- c("imprescindible" = "#018571", "prioritario" = "#80cdc1",
               "moderadamente prioritario" = "#dfd2b3", "marginalmente prioritario" = "#a6611a")
  val_col_cat <- val_col[match(categorias, names(val_col))]
  mapa <- vectorial %>% ggplot +
    aes(fill = .data[[campo_salida_etiq]]) +
    geom_sf(lwd=0) + 
    scale_fill_manual(nombre, values = val_col) +
    labs(title = paste('Reclasificación de valores de', nombre)) +
    theme_bw() +
    theme(
      legend.position = 'bottom',
      legend.key.size = unit(0.5, 'cm'), #change legend key size
      legend.key.height = unit(0.5, 'cm'), #change legend key height
      legend.key.width = unit(0.5, 'cm'), #change legend key width
      legend.title = element_text(size=8), #change legend title font size
      legend.text = element_text(size=6) #change legend text font size
      )
    
  # Intervalos y etiquetas
  extraer_digitos_interv <- function(col) {
    as.numeric(gsub('(\\(|\\[)(\\d.*)(,.*)', '\\2', col))
  }
  intervalos_y_etiquetas <- vectorial %>% st_drop_geometry %>%
    select(all_of(campo_salida_interv),
           all_of(campo_salida_etiq),
           all_of(paste(campo, 'puntuación'))) %>% 
           # all_of(paste(campo, 'enteros_ordenados', sep = '_'))) %>%
    distinct %>% 
    na.omit() %>% 
    mutate_at(.vars = campo_salida_interv,
              .funs = list(ord = extraer_digitos_interv)) %>% 
    arrange(ord) %>%
    select(-ord)
  
  # Salida
  return(list(vectorial = vectorial, mapa = mapa, intervalos_y_etiquetas = intervalos_y_etiquetas))
}

generar_resumen_grafico_estadistico_criterios <- function(
    variable = NULL, nombre = NULL, umbrales = NULL, 
    ord_cat = NULL, kable_caption = paste('Intervalos de', nombre)){
  internal <- variable
  resumen_estadistico <- tryCatch(
    success <- summary(ind_esp_inters[, internal, drop=T]),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  print(resumen_estadistico)
  violin <- tryCatch(
    success <- ggplot(ind_esp_inters) +
      aes(x = '', y = !!sym(internal)) +
      geom_boxplot(alpha = 0, width = 0.3) +
      geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "#00BA38") +
      scale_y_continuous(trans = 'pseudo_log') +
      theme_bw() +
      ylab(nombre) +
      theme(axis.title.x = element_blank(),
            plot.margin = margin(1, 6, 1, 6, "cm"),
            plot.background = element_rect(fill = "white")
            ),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  reclasificacion <- tryCatch(
    success <- reclasificar(
      vectorial = ind_esp_inters, campo = internal,
      umbrales = umbrales,
      nombre = nombre,
      ord_cat = ord_cat),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  vectorial <- tryCatch(
    success <- reclasificacion$vectorial %>% 
      rename_with(~ stringr::str_replace(.x, 
                                         pattern = internal, 
                                         replacement = nombre), 
                  matches(internal)),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  area_proporcional <- tryCatch(
    success <- vectorial %>%
      mutate(
        área = units::drop_units(st_area(geometry)),
        `área total` = sum(units::drop_units(st_area(geometry)))) %>%
      st_drop_geometry %>%
      group_by(across(all_of(matches('etiquetas')))) %>%
      summarise(proporción = sum(área, na.rm = T)/first(`área total`)*100) %>%
      na.omit() %>%
      mutate(proporción = as.numeric(scale(proporción, center = FALSE,
                                scale = sum(proporción, na.rm = TRUE)/100))),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  area_proporcional_kable <- tryCatch(
    success <- area_proporcional %>% 
      kable(format = 'html', escape = F, booktabs = T, digits = 2,
            caption = paste('Áreas proporcionales de', nombre)) %>%
      kable_styling(bootstrap_options = c("hover", "condensed"), full_width = T),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  mapa_con_pais <- tryCatch(
    success <- reclasificacion$mapa +
      geom_sf(data = pais, fill = 'transparent', lwd = 0.5, color = 'grey50'),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  reclasificacion$intervalos_y_etiquetas <- tryCatch(
    success <- reclasificacion$intervalos_y_etiquetas %>%
      rename_with(~ stringr::str_replace(.x, 
                                       pattern = internal, 
                                       replacement = nombre), 
                matches(internal)),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  intervalos_y_etiquetas_kable <- tryCatch(
    success <- reclasificacion$intervalos_y_etiquetas %>% 
      kable(format = 'html', escape = F, booktabs = T, digits = 2, caption = kable_caption) %>%
      kable_styling(bootstrap_options = c("hover", "condensed"), full_width = T),
    error = function(cond) {
      message('Caught an error. This is the error message: ', cond, appendLF = TRUE)
      return(NA)
    }
  )
  return(list(
    resumen_estadistico = resumen_estadistico,
    violin = violin,
    vectorial = vectorial,
    area_proporcional = area_proporcional,
    area_proporcional_kable = area_proporcional_kable,
    mapa = reclasificacion[['mapa']],
    mapa_con_pais = mapa_con_pais,
    intervalos_y_etiquetas = reclasificacion[['intervalos_y_etiquetas']],
    intervalos_y_etiquetas_kable = intervalos_y_etiquetas_kable
  ))
}

generar_indice <- function(geom = NULL, res = NULL, buffer_size = NULL){
  library(h3jsr)
  foo <- st_union(geom) %>% st_buffer(dist = buffer_size) %>% st_transform(crs = 4326)
  bar <- polygon_to_cells(geom = foo, res = res, simple = TRUE)
  result <- cell_to_polygon(as.character(unlist(bar)), simple = FALSE) %>% st_transform(crs = st_crs(geom))
  return(result)
}

# Inspired in: convex hull
# https://stackoverflow.com/questions/48925086/choosing-subset-of-farthest-points-in-given-set-of-points
generar_centroides_distantes <- function(
    geom = NULL, numero_total_de_puntos = NULL,
    km2_por_puntos = NULL, km_de_separacion = NULL,
    circular = T, solo_calculos = F, silencioso = T){
  # Paquete sf
  library(sf)
  
  # Área total
  area_total <- geom %>% st_area %>% sum %>% units::drop_units()/1000000
  cat('El área total (en kilómetros cuadrados) es', area_total, '\n')
  
  # Condicionales de los parámetros
  if(!is.null(numero_total_de_puntos)){
    km2_por_puntos <- NULL
    km_de_separacion <- NULL
    numero_total_de_puntos <- ceiling(numero_total_de_puntos)
    cat('Se usarán', numero_total_de_puntos, 'puntos en total\n')
  } else if(is.null(km2_por_puntos) && is.null(km_de_separacion)){
    stop('Debe aportarse número total de puntos; alternativamente, se puede aportar o kilómetros cuadrados por estación, o kilómetros de separación')
  } else if(!is.null(km2_por_puntos) && !is.null(km_de_separacion)){
    stop('Debe aportarse sólo uno de estos parámetros: kilómetros cuadrados por estación o kilómetros de separación')
  } else if(!is.null(km2_por_puntos) && is.null(km_de_separacion)) {
    numero_total_de_puntos <- ceiling(area_total/km2_por_puntos)
    cat('Calculando para', km2_por_puntos, 'kilómetros cuadrados por estación. El numero total de puntos a colocar es', numero_total_de_puntos, '\n')
  } else if(is.null(km2_por_puntos) && !is.null(km_de_separacion)) {
    if(circular){
      area_por_punto <- pi*(km_de_separacion/2)^2
      numero_total_de_puntos <- ceiling(area_total/area_por_punto)
      cat('Se aportó kilómetros de separación por círculos, cada punto cubre', area_por_punto, 'km cuad. a la redonda. El numero total de puntos a colocar es', numero_total_de_puntos, '\n')
    } else {
      area_por_punto <-  3*((km_de_separacion/2)^2)*sqrt(3)/2
      numero_total_de_puntos <- ceiling(area_total/area_por_punto)
      cat('Se aportó kilómetros de separación por hexágonos regulares, cada punto cubre', area_por_punto, 'km cuad. El numero total de puntos a colocar es', numero_total_de_puntos, '\n')
    }
  }
  
  # Salida a destiempo
  if(solo_calculos) {
    result <- data.frame(`Área total` = area_total,
                   `Número total de puntos` = numero_total_de_puntos,
                   `Distancia esperada entre vecinos` = 2*sqrt((area_total/numero_total_de_puntos)/pi),
                   check.names = F)
    return(invisible(result))
  }
  
  # Objetos internos
  p <- numero_total_de_puntos
  g <- geom
  
  # Centroides
  points <- invisible(suppressWarnings(g %>% st_centroid %>% st_coordinates))
  
  # Número total de puntos
  N <- nrow(points)
  
  # Encontrar la envolvente convexa 
  hull <- chull(points)
  
  # Extraer los vértices que forman la envolvente convexa
  hullpoints <- points[hull, ]
  
  # Distancias entre puntos de la envolvente
  hdist <- dist(hullpoints)

  # Encontrar los más distintes ("el mejor par inicial")
  bestpair <- rownames(which(as.matrix(hdist) == max(hdist), arr.ind = T))
  
  # El mejor par inicial en un data.frame
  P <- rbind(hullpoints[as.numeric(bestpair[1]),], hullpoints[as.numeric(bestpair[2]),])
  
  # Buscar los subconjuntos óptimos
  if(!silencioso) cat("Buscando los subconjuntos óptimos...\n")
  while (nrow(P) < p) {# Bucle while: evalúa si el subconjunto alcanzó el número deseado
    if(!silencioso) cat(sprintf("Tamaño del subconjunto = %d\n", nrow(P)))
    # Distancia de los puntos originales al subconjunto
    distance_to_P <- as.matrix(proxy::dist(points, P))
    # Mínima distancia del subconjunto
    minimum_to_each_of_P <- apply(distance_to_P, 1, min)
    # Índice del mejor punto nuevo (distancia maximizada)
    best_new_point_idx <- which.max(minimum_to_each_of_P)
    # Mejor punto nuevo (distancia maximizada)
    best_new_point <- points[best_new_point_idx,]
    # Actualizar el subconjunto P para el bucle
    P <- rbind(P, best_new_point)
  }
  # Generar resultado como objeto sf usando el CRS del provisto
  result <- P %>% as.data.frame %>% st_as_sf(coords = c('X','Y'), crs = st_crs(g))
  return(result)
}

# Inspired in: "drops the point that has the largest row sum in the distance matrix"
# https://stackoverflow.com/questions/22152482/choose-n-most-distant-points-in-r
# seleccionar_por_puntuacion_distancia <- function(geom = NULL,
#                                                  campo_categorias = NULL,
#                                                  categorias = c('altamente idóneo'),
#                                                  distancia = NULL, n = NULL){
#   library(sf)
#   g <- geom
#   g <- g[g[, campo_categorias, drop = T] %in% categorias, ]
#   distancias <- st_distance(geom)
#   distancias <- units::drop_units(distancias)
#   while (nrow(g) > n) {
#     distancias_p_filas <- rowSums(distancias)
#     # distancias_p_filas <- matrixStats::rowMins(distancias)
#     mas_cercano_i <- which(distancias_p_filas == min(distancias_p_filas))[1]
#     g <- g[-mas_cercano_i, ]
#     distancias <- distancias[-mas_cercano_i, -mas_cercano_i]
#   }
#   return(g)
# }

# Inspired in: Factor-2 Approximation
# https://stackoverflow.com/questions/48925086/choosing-subset-of-farthest-points-in-given-set-of-points
# seleccionar_por_puntuacion_distancia <- function(geom = NULL,
#                                                  campo_categorias = NULL,
#                                                  categorias = c('altamente idóneo'),
#                                                  distancia = NULL, n = NULL){
#   library(sf)
#   g <- geom
#   g <- g[g[, campo_categorias, drop = T] %in% categorias, ]
#   # return(g)
#   d <- st_distance(geom)
#   d <- units::drop_units(distancias)
#   N <- nrow(g)
#   p <- n
#   
#   cat("Finding initial edge...\n")
#   maxdist <- 0
#   bestpair <- c(0,0)
#   for(i in 1:(N-1)){
#     for(j in (i+1):N){
#       if(d[i,j] > maxdist){
#         maxdist <- d[i,j]
#         bestpair <- c(i,j)
#       }
#     }
#   }
#   
#   P <- c(bestpair[1], bestpair[2])
#   cat("Finding optimal set...\n")
#   while(length(P) < p){
#     cat(sprintf("P size = %d\n", length(P)))
#     maxdist <- 0
#     vbest <- NULL
#     for(v in 1:N){
#       if(v %in% P){
#         next
#       }
#       for(vprime in P){
#         if(d[v,vprime] > maxdist){
#           maxdist <- d[v,vprime]
#           vbest <- v
#         }
#       }
#     }
#     P <- c(P, vbest)
#   }
#   
#   print(P)
#   return(g[P, ])
# }

estadisticos_distancias_orden_1 <- function(geom){
  library(sf)
  library(spdep)
  library(psych)
  geom <- geom %>% st_transform(crs = 32619)
  k_cercanos_1 <- knearneigh(geom)
  vecindad <- knn2nb(k_cercanos_1)
  distancias_1 <- unlist(nbdists(vecindad, coords = k_cercanos_1$x))/1000 #Distancias en km
  estadisticos_distancias <- describe(distancias_1) #Estadísticos de distancias
  return(estadisticos_distancias)
}

source_rmd_chunks <- function(file, chunk_labels, skip_plots = TRUE, output_temp = FALSE){
  #From: https://gist.github.com/brshallo/e963b9dca5e4e1ab12ec6348b135362e
  library(magrittr)
  library(stringr)
  library(readr)
  library(purrr)
  library(glue)
  library(knitr)
  
  temp <- tempfile(fileext=".R")
  knitr::purl(file, output = temp)
  
  text <- readr::read_file(temp)
  
  text <- purrr::map(chunk_labels, ~stringr::str_extract(text, glue::glue("(## ----{var})(.|[:space:])*?(?=(## ----)|$)", var = .x))) %>% 
    stringr::str_c(collapse = "\n")
  
  readr::write_file(text, temp)
  
  if(skip_plots) {
    old_dev = getOption('device')
    options(device = function(...) {
      .Call("R_GD_nullDevice", PACKAGE = "grDevices")
    })
  }
  
  source(temp)
  
  if(skip_plots) {
    options(device = old_dev)
  }
  
  if(output_temp) temp
}


vector_a_lista <- function(vec) {
  n <- length(vec)
  if (n == 0) {
    return("")
  } else if (n == 1) {
    return(as.character(vec))
  } else if (n == 2) {
    return(paste(vec, collapse = " y "))
  } else {
    last <- paste("y", vec[n])
    rest <- paste(vec[1:(n-1)], collapse = ", ")
    return(paste(rest, last))
  }
}

extraer_dato <- function(
    ruta = NULL, patron = 'total',
    sustituir = '^.*\\|', convertir_num = T) {
  fuente <- readLines(ruta)
  fila_interes <- grep(patron, fuente, value = T)
  dato_interes <- gsub(sustituir, '', fila_interes)
  if(convertir_num) dato_interes <- as.numeric(dato_interes)
  return(dato_interes)
}

conteo_lineas <- function(ruta, saltar = 1, convertir_num = T){
  dato_interes <- system(paste0('tail -n +', 1+saltar, ' ', ruta, ' | wc -l'), intern=T)
  if(convertir_num) dato_interes <- as.numeric(dato_interes)
  return(dato_interes)
}

estilo_kable <- function(df, titulo = '', cubre_anchura = T) {
  df %>% kable(format = 'latex', escape = F, booktabs = T,
               digits = 2, caption = titulo) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  latex_options = "HOLD_position",
                  full_width = cubre_anchura, position = "center")
}

estilo_kable_corto <- function(df, titulo = '', cubre_anchura = T) {
  df %>%
    kable(format = 'latex', escape = F, booktabs = T, digits = 2, caption = titulo) %>%
    kable_styling(bootstrap_options = c("hover", "condensed"),
                  full_width = cubre_anchura)
}

country_map_ws <- function(obj, entity) {
  obj %>%
    filter(tipo == "meteoclimática", entidad %in% entity) %>%
    ggplot() +
    geom_sf(data = haiti, fill = "white", color = "black", alpha = 0.25) +
    geom_sf(data = pais, fill = "lightgrey", color = "black", alpha = 0.25) +
    geom_sf(aes(fill = Estado), shape = 21, size = 2, alpha = 0.8) +
    scale_fill_manual(
      values = c(
        "activa o bueno" = viridis(3)[2],
        "inactiva o no reportada" = viridis(3)[3],
        "regular" = viridis(3)[1]
      ),
      labels = c(
        "activa o bueno" = "Active or Good",
        "inactiva o no reportada" = "Inactive or Not Reported",
        "regular" = "Recoverable"
      ),
      drop = FALSE
    ) +
    labs(title = paste0("Weather Stations of ", entity), fill = "Status") +
    theme_bw() +
    ggspatial::annotation_scale(style = "ticks") +
    theme(
      legend.title = element_text(size = 10),
      legend.position = "bottom",
      legend.text = element_text(margin = margin(r = 10, unit = "pt"))) +
    coord_sf(xlim = lims_rd_buffer[grepl('^x', names(lims_rd_buffer))],
             ylim = lims_rd_buffer[!grepl('^x', names(lims_rd_buffer))])
}
