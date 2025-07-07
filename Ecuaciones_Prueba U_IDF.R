setwd("C:/Users/ASUS/Documents/Octavo Ciclo/Tesis/ProcesamientoReal")

Bernal_TR <- read.csv("Bernal_TR.csv")

Chusis_TR <- read.csv("Chusis_TR.csv")

Miraflores_TR <- read.csv("Miraflores_TR.csv")

SanMiguel_TR <- read.csv("SanMiguel_TR.csv")

Bernal_TR_M <- read.csv("Bernal_TR_M.csv")

Chusis_TR_M <- read.csv("Chusis_TR_M.csv")

Miraflores_TR_M <- read.csv("Miraflores_TR_M.csv")

SanMiguel_TR_M <- read.csv("SanMiguel_TR_M.csv")



Bernal_TR

library(dplyr)
library(tidyr)

# Suponiendo que ya cargaste tu dataframe Bernal_TR
Bernal_long <- Bernal_TR %>%
  pivot_longer(
    cols = -Periodo.Retorno..TR.,
    names_to = "Distribucion",
    values_to = "Intensidad"
  )
# Lista de distribuciones únicas
distribuciones <- unique(Bernal_long$Distribucion)

# Crear una lista con cada distribución como tabla horizontal
lista_tablas <- lapply(distribuciones, function(dist) {
  Bernal_long %>%
    filter(Distribucion == dist) %>%
    select(TR = Periodo.Retorno..TR., Intensidad) %>%
    pivot_wider(names_from = TR, values_from = Intensidad)
})

# Asignar nombres a la lista
names(lista_tablas) <- distribuciones

lista_tablas





#para tr


#-------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# ------------------------------------
# PARTE 1: CONFIGURACIÓN INICIAL
# ------------------------------------

# Duraciones en horas y coeficientes de reducción
duraciones_horas <- c(1, 2, 3, 4, 5, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
coeficientes <- c(0.25, 0.31, 0.38, 0.44, 0.50, 0.56, 0.64, 0.73, 0.79,
                  0.83, 0.87, 0.90, 0.93, 0.97, 1.00)

# Lista de archivos base (estaciones y versiones M)
archivos <- c("Bernal_TR.csv", "Chusis_TR.csv", "Miraflores_TR.csv", "SanMiguel_TR.csv",
              "Bernal_TR_M.csv", "Chusis_TR_M.csv", "Miraflores_TR_M.csv", "SanMiguel_TR_M.csv")

# ------------------------------------
# PARTE 2: PROCESAMIENTO Y EXPORTACIÓN
# ------------------------------------

for (archivo in archivos) {
  
  # Leer archivo
  df <- read_csv(archivo)
  nombre_base <- tools::file_path_sans_ext(archivo)
  
  # Convertir a formato largo
  df_long <- df %>%
    pivot_longer(cols = -1, names_to = "Distribucion", values_to = "P24h")
  
  # Procesar cada distribución
  distribuciones <- unique(df_long$Distribucion)
  
  for (dist in distribuciones) {
    
    df_dist <- df_long %>% filter(Distribucion == dist)
    TR_vals <- df_dist[[1]]
    
    # APLICAR CORRECCIÓN de 1.13
    P24h_vals <- df_dist$P24h * 1.13
    
    # ----------------------------
    # Precipitaciones por horas
    Pd_horas <- sapply(P24h_vals, function(p) p * coeficientes)
    tabla_horas <- as.data.frame(t(Pd_horas))
    colnames(tabla_horas) <- paste0("h", duraciones_horas, "h")
    tabla_horas$TR <- TR_vals
    tabla_horas <- tabla_horas %>% relocate(TR)
    
    # ----------------------------
    # Guardar precipitación acumulada
    archivo_prec <- paste0("Reduccion_", nombre_base, "_", dist, ".csv")
    write_csv(tabla_horas, archivo_prec)
    cat("Generado:", archivo_prec, "\n")
    
    # ----------------------------
    # Calcular intensidades (mm/h)
    prec_solo <- tabla_horas %>% select(-TR)
    intensidades <- sweep(prec_solo, 2, duraciones_horas, "/")
    
    tabla_intensidad <- bind_cols(TR = tabla_horas$TR, intensidades)
    
    # Guardar intensidades
    archivo_intensidad <- paste0("Intensidades_", nombre_base, "_", dist, ".csv")
    write_csv(tabla_intensidad, archivo_intensidad)
    cat("Generado:", archivo_intensidad, "\n")
  }
}



library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Listar todos los archivos de intensidades reales
archivos <- list.files(pattern = "^Intensidades_.*\\.csv$")

for (archivo in archivos) {
  
  # Leer archivo
  datos <- read_csv(archivo, show_col_types = FALSE)
  
  # Convertir de ancho a largo
  datos_long <- datos %>%
    pivot_longer(
      cols = -TR,
      names_to = "Duracion",
      values_to = "Intensidad"
    ) %>%
    mutate(
      # Extraer duración en horas desde columnas tipo "h1h", "h24h"
      D = as.numeric(str_extract(Duracion, "\\d+")) * 60  # en minutos
    )
  
  # Crear nombre base para el gráfico
  nombre_base <- str_replace(archivo, "Intensidades_|\\.csv", "")
  
  # Graficar curvas IDF
  p <- ggplot(datos_long, aes(x = D, y = Intensidad, color = factor(TR))) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = paste("Curvas IDF (intensidades reales) -", nombre_base),
      x = "Duración (minutos)",
      y = "Intensidad (mm/h)",
      color = "TR (años)"
    ) +
    theme_minimal(base_size = 14)
  
  print(p)  # Mostrar en pantalla
  
  readline(prompt = "Presiona [Enter] para continuar con el siguiente gráfico...")
}





# ──────────────────────────────────────────────────────────────
# 0.  LIBRERÍAS
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ──────────────────────────────────────────────────────────────
# 1.  LISTAR ARCHIVOS DE INTENSIDADES
#     (usa el patrón que creaste antes)
# ──────────────────────────────────────────────────────────────
archivos_int <- list.files(pattern = "^Intensidades_.*\\.csv$")

# Vector para almacenar resultados
resumen_param <- list()

# ──────────────────────────────────────────────────────────────
# 2.  BUCLE PRINCIPAL: AJUSTE k-m-n PARA CADA ARCHIVO
# ──────────────────────────────────────────────────────────────
for (archivo in archivos_int) {
  
  # 2.1 Leer intensidades
  datos <- read_csv(archivo, show_col_types = FALSE)
  
  # 2.2 Pasar de ancho a largo: I, D, T
  datos_long <- datos %>%
    pivot_longer(
      cols = -TR,
      names_to  = "Duracion",
      values_to = "Intensidad"
    ) %>%
    mutate(
      # Extraer la duración en horas (ej. "h12h" → 12)
      D = as.numeric(str_extract(Duracion, "(?<=h)\\d+(?=h)"))* 60,
      T = TR
    ) %>%
    filter(Intensidad > 0 & !is.na(D))
  
  # 2.3 Construir variables logarítmicas
  datos_long <- datos_long %>%
    mutate(
      logI = log(Intensidad),
      logT = log(T),
      logD = log(D)
    )
  
  # 2.4 Ajustar modelo lineal múltiple
  mod <- lm(logI ~ logT + logD, data = datos_long)
  
  # 2.5 Extraer parámetros
  a     <- coef(mod)[1]          # intercepto → log k
  m     <- coef(mod)["logT"]     # exponente de T
  c_D   <- coef(mod)["logD"]     # coef. de logD  (≈ –n)
  
  k_val <- exp(a)
  n_val <- -c_D                  # signo cambiado para cumplir la eq. canónica
  
  # 2.6 Mostrar ecuación
  mensaje <- sprintf(
    "\nArchivo: %s\nI = %.4f * T^(%.4f) / D^(%.4f)\n",
    archivo, k_val, m, n_val
  )
  cat(mensaje)
  
  # 2.7 Guardar en lista-resumen
  resumen_param[[archivo]] <- tibble(
    Archivo       = archivo,
    k             = k_val,
    m             = m,
    n             = n_val,
    R2_Ajustado   = summary(mod)$adj.r.squared
  )
}

# ──────────────────────────────────────────────────────────────
# 3.  UNIR Y EXPORTAR RESULTADOS
# ──────────────────────────────────────────────────────────────
parametros_IDF <- bind_rows(resumen_param)

write_csv(parametros_IDF, "Parametros_IDF_kmn.csv")
cat("\nResumen de parámetros guardado en: Parametros_IDF_kmn.csv\n")




# ──────────────────────────────────────────────────────────────
# 0. LIBRERÍAS
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ──────────────────────────────────────────────────────────────
# 1. CARGAR PARÁMETROS IDF
# ──────────────────────────────────────────────────────────────
parametros_IDF <- read_csv("Parametros_IDF_kmn.csv", show_col_types = FALSE)

# ──────────────────────────────────────────────────────────────
# 2. LISTAR ARCHIVOS DE INTENSIDADES
# ──────────────────────────────────────────────────────────────
archivos_int <- list.files(pattern = "^Intensidades_.*\\.csv$")

# ──────────────────────────────────────────────────────────────
# 3. PROCESAR CADA ARCHIVO
# ──────────────────────────────────────────────────────────────
for (archivo in archivos_int) {
  
  # 3.1 Leer intensidades reales
  datos <- read_csv(archivo, show_col_types = FALSE)
  
  # 3.2 Convertir a formato largo
  datos_long <- datos %>%
    pivot_longer(cols = -TR, names_to = "Duracion", values_to = "I_real") %>%
    mutate(
      D = as.numeric(str_extract(Duracion, "\\d+")) * 60,  # Duración en minutos
      T = TR
    )
  
  # 3.3 Obtener parámetros IDF para este archivo
  fila_param <- parametros_IDF %>% filter(Archivo == archivo)
  
  if (nrow(fila_param) == 0) {
    cat("⚠️  No se encontraron parámetros para:", archivo, "\n")
    next
  }
  
  k <- fila_param$k
  m <- fila_param$m
  n <- fila_param$n
  
  # 3.4 Calcular intensidades modeladas
  datos_long <- datos_long %>%
    mutate(
      I_modelada = k * T^m / D^n,
      Error_absoluto = abs(I_real - I_modelada),
      Error_relativo = abs(I_real - I_modelada) / I_real * 100
    )
  
  # 3.5 Guardar tabla de comparación
  nombre_salida <- str_replace(archivo, "Intensidades_", "Comparacion_Intensidades_")
  write_csv(datos_long, nombre_salida)
  
  cat("✅ Comparación guardada:", nombre_salida, "\n")
}


#################
# Todos los archivos
archivos_todos <- list.files(pattern = "^Comparacion_Intensidades_.*\\.csv$")

# Separar por estación
archivos_bernal      <- archivos_todos[str_detect(archivos_todos, "Bernal")]
archivos_chusis      <- archivos_todos[str_detect(archivos_todos, "Chusis")]
archivos_miraflores  <- archivos_todos[str_detect(archivos_todos, "Miraflores")]
archivos_sanmiguel   <- archivos_todos[str_detect(archivos_todos, "SanMiguel")]

# Leer por grupo si lo necesitas
lista_bernal <- lapply(archivos_bernal, read_csv, show_col_types = FALSE)
names(lista_bernal) <- archivos_bernal

lista_chusis <- lapply(archivos_chusis, read_csv, show_col_types = FALSE)
names(lista_bernal) <- archivos_bernal

lista_miraflores <- lapply(archivos_miraflores, read_csv, show_col_types = FALSE)
names(lista_bernal) <- archivos_bernal

lista_sanmiguel <- lapply(archivos_sanmiguel, read_csv, show_col_types = FALSE)
names(lista_bernal) <- archivos_sanmiguel




# ──────────────────────────────────────────────────────────────
# 0. LIBRERÍAS
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(readr)
library(stringr)
library(minpack.lm)  # para nlsLM

# ──────────────────────────────────────────────────────────────
# 1. FUNCIÓN DE AJUSTE IDF (k, m, n)
# ──────────────────────────────────────────────────────────────
ajustar_idf <- function(df) {
  T <- df$T
  D <- df$D
  I_real <- df$I_real
  
  # Estimaciones iniciales
  start_vals <- list(k = 10, m = 0.5, n = 0.5)
  
  # Ajuste no lineal
  modelo <- tryCatch({
    nlsLM(I_real ~ k * T^m / D^n,
          start = start_vals,
          control = list(maxiter = 500))
  }, error = function(e) return(NULL))
  
  if (is.null(modelo)) return(NULL)
  
  # Predicción
  I_mod <- predict(modelo)
  
  # Métricas
  SSE <- sum((I_real - I_mod)^2)
  SST <- sum((I_real - mean(I_real))^2)
  n_obs <- nrow(df)
  p <- length(coef(modelo))
  R2_adj <- 1 - ((SSE / (n_obs - p)) / (SST / (n_obs - 1)))
  ERM <- mean(abs(I_real - I_mod) / I_real) * 100
  
  # Resultado
  params <- as.list(coef(modelo))
  params$R2_adj <- R2_adj
  params$ERM <- ERM
  return(params)
}

# ──────────────────────────────────────────────────────────────
# 2. LEER Y AJUSTAR TODOS LOS ARCHIVOS
# ──────────────────────────────────────────────────────────────
archivos <- list.files(pattern = "^Comparacion_Intensidades_.*\\.csv$")

resultados <- list()

for (archivo in archivos) {
  df <- read_csv(archivo, show_col_types = FALSE)
  
  # Filtrar datos válidos
  df <- df %>% filter(!is.na(I_real), I_real > 0, !is.na(D), !is.na(T))
  
  cat("⏳ Ajustando:", archivo, "\n")
  resultado <- ajustar_idf(df)
  
  if (!is.null(resultado)) {
    resultado$Archivo <- archivo
    resultados[[archivo]] <- resultado
    cat(sprintf("✅ %s → R²=%.4f | ERM=%.2f%%\n",
                archivo, resultado$R2_adj, resultado$ERM))
  } else {
    cat("❌ Falló el ajuste en:", archivo, "\n")
  }
}

# ──────────────────────────────────────────────────────────────
# 3. GUARDAR RESULTADOS FILTRADOS
# ──────────────────────────────────────────────────────────────
df_resultados <- bind_rows(resultados)

# Filtrar por criterios de calidad
df_optimo <- df_resultados %>%
  filter(R2_adj >= 0.98, ERM <= 5)

# Guardar archivo final
write_csv(df_optimo, "IDF_Calibrados_Optimos.csv")
cat("\n📁 Resultados óptimos guardados en: IDF_Calibrados_Optimos.csv\n")






















###################
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# ------------------------------------
# 1. FUNCIÓN PARA CALCULAR Pd
# ------------------------------------
calcular_Pd <- function(P24h, d) {
  P24h * (d / 1440)^0.25
}

# ------------------------------------
# 2. CONFIGURACIÓN
# ------------------------------------
# Duraciones deseadas (en minutos)
duraciones <- c(5, 10, 15, 30, 60, 120, 180, 360, 720, 1440)

# Archivos base
archivos <- c("Bernal_TR.csv", "Chusis_TR.csv", "Miraflores_TR.csv", "SanMiguel_TR.csv",
              "Bernal_TR_M.csv", "Chusis_TR_M.csv", "Miraflores_TR_M.csv", "SanMiguel_TR_M.csv")

# ------------------------------------
# 3. PROCESAMIENTO
# ------------------------------------
for (archivo in archivos) {
  
  df <- read_csv(archivo)
  nombre_base <- tools::file_path_sans_ext(archivo)
  
  df_long <- df %>%
    pivot_longer(cols = -1, names_to = "Distribucion", values_to = "P24h")
  
  distribuciones <- unique(df_long$Distribucion)
  
  for (dist in distribuciones) {
    
    df_dist <- df_long %>% filter(Distribucion == dist)
    TR_vals <- df_dist[[1]]
    P24h_vals <- df_dist$P24h * 1.13  # Aplica corrección
    
    # Calcular Pd e intensidades para cada duración
    Pd_tabla <- sapply(duraciones, function(d) calcular_Pd(P24h_vals, d))
    colnames(Pd_tabla) <- paste0("D", duraciones, "min")
    Pd_df <- as.data.frame(Pd_tabla)
    
    # Calcular intensidades en mm/h
    intensidades_df <- sweep(Pd_df, 2, duraciones, "/") * 60
    colnames(intensidades_df) <- paste0("I", duraciones, "min")
    
    # Unir con TR
    tabla_final <- bind_cols(TR = TR_vals, intensidades_df)
    
    # Guardar resultado
    archivo_salida <- paste0("Intensidades_Recalculadas_", nombre_base, "_", dist, ".csv")
    write_csv(tabla_final, archivo_salida)
    cat("✅ Guardado:", archivo_salida, "\n")
  }
}








# Cargar librerías
library(dplyr)
library(readr)
library(tidyr)
library(purrr)

# Leer archivo con parámetros IDF
parametros <- read_csv("ParametroIDF.csv")

# Definir TR y duraciones
TR <- c(2, 2.33, 5, 10, 25, 50, 100, 140, 200, 500, 1000)
duraciones <- c(
  I5min = 5, I10min = 10, I15min = 15, I30min = 30,
  I60min = 60, I120min = 120, I180min = 180, I360min = 360,
  I720min = 720, I1440min = 1440
)

# Expandir combinaciones
combinaciones <- expand.grid(TR = TR, Duracion = names(duraciones))

# Función para calcular y guardar cada curva en formato ancho (wide)
guardar_curva_idf <- function(Nombre_Archivo, K, m, n) {
  datos <- combinaciones %>%
    mutate(
      Intensidad = (K * (TR ^ m)) / (duraciones[Duracion] ^ n)
    ) %>%
    pivot_wider(names_from = Duracion, values_from = Intensidad) %>%
    arrange(TR)
  
  # Limpiar nombre y definir salida
  nombre_limpio <- gsub("Intensidades_Recalculadas_|\\.csv", "", Nombre_Archivo)
  nombre_salida <- paste0("Curva_IDF_", nombre_limpio, ".csv")
  
  # Guardar CSV
  write_csv(datos, nombre_salida)
  
  # Imprimir mensaje de confirmación
  message("✅ Guardado: ", nombre_salida)
}

# Aplicar la función a cada fila
pwalk(parametros, guardar_curva_idf)





library(dplyr)
library(readr)
library(stringr)
library(tibble)

# Listar archivos disponibles
archivos_eq <- list.files(pattern = "^Curva_IDF_.*\\.csv$")
archivos_recalc <- list.files(pattern = "^Intensidades_Recalculadas_.*\\.csv$")

# Extraer nombre base sin prefijo
extraer_base <- function(nombre) {
  nombre %>%
    str_remove("^Curva_IDF_") %>%
    str_remove("^Intensidades_Recalculadas_") %>%
    str_remove("\\.csv$")
}

# Obtener nombres comparables
nombres_eq <- sapply(archivos_eq, extraer_base)
nombres_recalc <- sapply(archivos_recalc, extraer_base)

# Nombres en común
nombres_comunes <- intersect(nombres_eq, nombres_recalc)

# Comparación global
resultados <- list()

for (nombre in nombres_comunes) {
  archivo_eq <- paste0("Curva_IDF_", nombre, ".csv")
  archivo_recalc <- paste0("Intensidades_Recalculadas_", nombre, ".csv")
  
  datos_eq <- read_csv(archivo_eq, show_col_types = FALSE)
  datos_recalc <- read_csv(archivo_recalc, show_col_types = FALSE)
  
  # Quitar columnas de TR y vectorizar
  intensidades_eq <- datos_eq %>% select(-1) %>% unlist() %>% as.numeric()
  intensidades_recalc <- datos_recalc %>% select(-1) %>% unlist() %>% as.numeric()
  
  # Prueba U
  prueba <- wilcox.test(intensidades_eq, intensidades_recalc, paired = FALSE)
  
  # Separar estación y distribución del nombre
  partes <- str_split(nombre, "_", simplify = TRUE)
  estacion <- partes[1]
  distribucion <- paste(partes[-1], collapse = "_")
  
  resultados[[length(resultados) + 1]] <- tibble(
    Estacion = estacion,
    Distribucion = distribucion,
    P_value = round(prueba$p.value, 6),
    Significativo = ifelse(prueba$p.value < 0.05, "Sí", "No")
  )
}

# Unir resultados y guardar
tabla_resultado <- bind_rows(resultados)
write_csv(tabla_resultado, "Resultado_Prueba_U_MannWhitney.csv")
cat("✅ Resultados guardados en 'Resultado_Prueba_U_MannWhitney.csv'\n")











# Cargar librerías necesarias
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Leer archivo con parámetros IDF
parametros <- read_csv("ParametroIDF.csv")

# Definir duraciones y periodos de retorno
duraciones <- c(
  I5min = 5, I10min = 10, I15min = 15, I30min = 30,
  I60min = 60, I120min = 120, I180min = 180, I360min = 360,
  I720min = 720, I1440min = 1440
)
TR <- c(2, 2.33, 5, 10, 25, 50, 100, 140, 200, 500, 1000)

# Función para generar intensidades
generar_curva_idf <- function(K, m, n) {
  expand.grid(TR = TR, Duracion = names(duraciones)) %>%
    mutate(Intensidad = (K * TR^m) / (duraciones[Duracion]^n)) %>%
    pivot_wider(names_from = Duracion, values_from = Intensidad) %>%
    arrange(TR)
}

# Función principal para comparar curvas de una estación
comparar_curvas_estacion <- function(nombre_estacion, parametros) {
  curvas_param <- parametros %>% filter(str_detect(Nombre_Archivo, nombre_estacion))
  curvas <- map2(curvas_param$K, curvas_param$m, ~ generar_curva_idf(.x, .y, curvas_param$n[which(curvas_param$K == .x)]))
  nombres <- tools::file_path_sans_ext(basename(curvas_param$Nombre_Archivo))
  
  matriz <- matrix(nrow = length(nombres), ncol = length(nombres), dimnames = list(nombres, nombres))
  
  for (i in seq_along(nombres)) {
    for (j in seq_along(nombres)) {
      if (i == j) {
        matriz[i, j] <- "—"
      } else {
        x <- as.numeric(unlist(curvas[[i]][, -1]))
        y <- as.numeric(unlist(curvas[[j]][, -1]))
        p <- wilcox.test(x, y, alternative = "two.sided")$p.value
        matriz[i, j] <- format(round(p, 4), nsmall = 4)
      }
    }
  }
  
  df_matriz <- as.data.frame(matriz)
  write.csv(df_matriz, paste0("Matriz_MannWhitney_", nombre_estacion, ".csv"), row.names = TRUE)
  return(df_matriz)
}

# 🔍 Detectar nombres únicos de estaciones (por ejemplo, extraer "Bernal" de "Intensidades_Recalculadas_Bernal_TR_Gamma.csv")
parametros <- parametros %>%
  mutate(Estacion = str_extract(Nombre_Archivo, "(?<=Recalculadas_)[^_]+"))

# Obtener lista de estaciones únicas
estaciones <- unique(parametros$Estacion)

# Aplicar comparación para todas las estaciones
resultados_estaciones <- map(estaciones, ~ comparar_curvas_estacion(.x, parametros))

# Nombres en consola
names(resultados_estaciones) <- estaciones










# Librerías necesarias
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# Leer archivo con parámetros IDF
parametros <- read_csv("ParametroIDF.csv")

# Definir duraciones y TRs
duraciones <- c(
  I5min = 5, I10min = 10, I15min = 15, I30min = 30,
  I60min = 60, I120min = 120, I180min = 180, I360min = 360,
  I720min = 720, I1440min = 1440
)
TR <- c(2, 2.33, 5, 10, 25, 50, 100, 140, 200, 500, 1000)

# Función para generar la curva IDF
generar_curva_idf <- function(K, m, n) {
  expand.grid(TR = TR, Duracion = names(duraciones)) %>%
    mutate(Intensidad = (K * TR^m) / (duraciones[Duracion]^n)) %>%
    pivot_wider(names_from = Duracion, values_from = Intensidad) %>%
    arrange(TR)
}

# Función para comparar curvas y generar matriz Mann-Whitney
comparar_grupo_curvas <- function(df_parametros, nombre_estacion, sufijo) {
  curvas <- map2(df_parametros$K, df_parametros$m, ~ generar_curva_idf(.x, .y, df_parametros$n[which(df_parametros$K == .x)]))
  nombres <- tools::file_path_sans_ext(basename(df_parametros$Nombre_Archivo))
  
  matriz <- matrix(nrow = length(nombres), ncol = length(nombres), dimnames = list(nombres, nombres))
  
  for (i in seq_along(nombres)) {
    for (j in seq_along(nombres)) {
      if (i == j) {
        matriz[i, j] <- "—"
      } else {
        x <- as.numeric(unlist(curvas[[i]][, -1]))
        y <- as.numeric(unlist(curvas[[j]][, -1]))
        p <- wilcox.test(x, y, alternative = "two.sided")$p.value
        matriz[i, j] <- format(round(p, 4), nsmall = 4)
      }
    }
  }
  
  df_matriz <- as.data.frame(matriz)
  write.csv(df_matriz, paste0("Matriz_MannWhitney_", nombre_estacion, "_", sufijo, ".csv"), row.names = TRUE)
  return(df_matriz)
}

# Clasificar parámetros por estación y tipo (TR vs TR_M)
parametros <- parametros %>%
  mutate(
    Estacion = str_extract(Nombre_Archivo, "(?<=Recalculadas_)[^_]+"),
    Tipo = if_else(str_detect(Nombre_Archivo, "_TR_M_"), "TR_M", "TR")
  )

# Obtener lista de estaciones
estaciones <- unique(parametros$Estacion)

# Aplicar por estación
resultados_estaciones <- list()

for (est in estaciones) {
  param_tr    <- parametros %>% filter(Estacion == est, Tipo == "TR")
  param_tr_m  <- parametros %>% filter(Estacion == est, Tipo == "TR_M")
  
  if (nrow(param_tr) > 1) {
    res_tr <- comparar_grupo_curvas(param_tr, est, "TR")
    resultados_estaciones[[paste0(est, "_TR")]] <- res_tr
  }
  
  if (nrow(param_tr_m) > 1) {
    res_trm <- comparar_grupo_curvas(param_tr_m, est, "TR_M")
    resultados_estaciones[[paste0(est, "_TR_M")]] <- res_trm
  }
}





#######################

library(readr)
library(dplyr)
library(stringr)
library(purrr)

# 1. Leer todos los archivos relevantes
archivos_recalc <- list.files(pattern = "^Intensidades_Recalculadas_.*\\.csv$")
archivos_idf <- list.files(pattern = "^Curva_IDF_.*\\.csv$")

# 2. Función para extraer ID base (estación + distribución + M si aplica)
id_base <- function(nombre, tipo = c("recalc", "idf")) {
  nombre %>%
    str_remove(ifelse(tipo == "recalc", "^Intensidades_Recalculadas_", "^Curva_IDF_")) %>%
    str_remove("\\.csv$")
}

# 3. Crear tabla con pares: ID, archivo_recalc, archivo_idf
df_pares <- tibble(
  id = id_base(archivos_recalc, "recalc"),
  archivo_recalc = archivos_recalc,
  archivo_idf = archivos_idf[match(id_base(archivos_recalc, "recalc"),
                                   id_base(archivos_idf, "idf"))]
) %>% filter(!is.na(archivo_idf))

# 4. Comparar intensidades
comparar_idf_vs_recalc <- function(archivo_recalc, archivo_idf, id) {
  df_r <- read_csv(archivo_recalc, show_col_types = FALSE)
  df_i <- read_csv(archivo_idf, show_col_types = FALSE)
  
  # Columnas comunes (distribuciones)
  columnas <- intersect(names(df_r), names(df_i))
  columnas <- columnas[columnas != "Periodo Retorno (TR)"]
  
  map_dfr(columnas, function(distrib) {
    x <- df_r[[distrib]]
    y <- df_i[[distrib]]
    
    tibble(
      Estacion_Distrib = id,
      Distribucion = distrib,
      R2 = summary(lm(y ~ x))$r.squared,
      Pearson = cor(x, y, method = "pearson")
    )
  })
}

# 5. Ejecutar comparación en todos los pares
resultados_idf_vs_recalc <- pmap_dfr(df_pares, function(id, archivo_recalc, archivo_idf) {
  comparar_idf_vs_recalc(archivo_recalc, archivo_idf, id)
})

# 6. Guardar resultados
write_csv(resultados_idf_vs_recalc, "Comparacion_Recalculado_vs_IDF.csv")
cat("✅ Guardado: Comparacion_Recalculado_vs_IDF.csv\n")


library(readr)
library(dplyr)
library(stringr)
library(purrr)

# 1. Leer todos los archivos
archivos_recalc <- list.files(pattern = "^Intensidades_Recalculadas_.*\\.csv$")
archivos_idf <- list.files(pattern = "^Curva_IDF_.*\\.csv$")

# 2. Extraer ID base
id_base <- function(nombre, tipo = c("recalc", "idf")) {
  nombre %>%
    str_remove(ifelse(tipo == "recalc", "^Intensidades_Recalculadas_", "^Curva_IDF_")) %>%
    str_remove("\\.csv$")
}

# 3. Juntar pares
df_pares <- tibble(
  id = id_base(archivos_recalc, "recalc"),
  archivo_recalc = archivos_recalc,
  archivo_idf = archivos_idf[match(id_base(archivos_recalc, "recalc"),
                                   id_base(archivos_idf, "idf"))]
) %>% filter(!is.na(archivo_idf))

# 4. Comparación por suma total
comparar_totales <- function(archivo_recalc, archivo_idf, id) {
  df_r <- read_csv(archivo_recalc, show_col_types = FALSE)
  df_i <- read_csv(archivo_idf, show_col_types = FALSE)
  
  columnas <- setdiff(intersect(names(df_r), names(df_i)), "Periodo Retorno (TR)")
  
  suma_r <- df_r %>% select(all_of(columnas)) %>% summarise_all(sum, na.rm = TRUE)
  suma_i <- df_i %>% select(all_of(columnas)) %>% summarise_all(sum, na.rm = TRUE)
  
  x <- as.numeric(suma_r)
  y <- as.numeric(suma_i)
  
  tibble(
    Estacion_Distrib = id,
    R2 = summary(lm(y ~ x))$r.squared,
    Pearson = cor(x, y, method = "pearson")
  )
}

# 5. Ejecutar
resultados_totales <- pmap_dfr(df_pares, function(id, archivo_recalc, archivo_idf) {
  comparar_totales(archivo_recalc, archivo_idf, id)
})

# 6. Guardar
write_csv(resultados_totales, "Comparacion_Totales_Recalculado_vs_IDF.csv")
cat("✅ Guardado: Comparacion_Totales_Recalculado_vs_IDF.csv\n")
