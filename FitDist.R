# 1. Instalación y carga de librerías -----------------------------------

paquetes <- c("fitdistrplus", "lmomco", "PearsonDS", "evd")
nuevos   <- paquetes[!paquetes %in% installed.packages()]
if(length(nuevos)) install.packages(nuevos)

lapply(paquetes, library, character.only = TRUE)
setwd("C:/Users/ASUS/Documents/Octavo Ciclo/Tesis/ProcesamientoReal")


df <- read.csv("Chusis_MaxAnual.csv")
colnames(df)[2] <- "Max_Precip"
df <- df %>% filter(!is.na(Max_Precip))
prec <- df$Max_Precip * 1.13
# 2. Carga de datos ------------------------------------------------------

library(lmomco)
library(fitdistrplus)
library(extRemes)
library(e1071)        # para skewness()
library(PearsonDS)
library(evd)
#---- Norma, Gamma, Gumbel---- 
fit_distributions <- function(datos,dist_list = c("norm", "gamma", "gumbel")) {
  library(fitdistrplus)
  library(extRemes)
  
  datos_clean <- datos[is.finite(datos)]
  resultados <- vector("list", length(dist_list))
  names(resultados) <- dist_list
  
  for (dist in dist_list) {
    fit <- NULL
    try({
      if (dist == "norm") {
        fit <- fitdist(datos_clean, "norm")
        
      } else if (dist == "gamma") {
        # No se filtra por >0, se asume que datos negativos/0 serán manejados por fitdist
        fit <- fitdist(datos_clean, "gamma")
        
      } else if (dist == "gumbel") {
        fit <- fevd(datos_clean, type = "Gumbel", method = "MLE")
        
      } 
    }, silent = TRUE)
    resultados[[dist]] <- fit
  }
  return(resultados)
}

#---- LogNormal, LogGumbel----
fit_log_distributions <- function(datos) {

  datos_pos <- datos[is.finite(datos)]  
  # Limitar: debe haber solo valores positivos y finitos
  if (any(datos_pos <= 0)) {
    stop("Todos los datos mayores que cero para Log-Normal y Log-Gumbel.")
  }
  
  # Ajuste Log-Normal 2P
  fit_lnorm <- fitdist(datos_pos, "lnorm")
  
  # Ajuste Log-Gumbel (Gumbel sobre log(datos))
  fit_logg  <- fevd(log(datos_pos), type = "Gumbel", method = "MLE")
  
  list(
    lognormal = fit_lnorm,
    loggumbel = fit_logg
  )
}

#---- LN3 y GAMMA 3P----
fit_lognormal3p_lmom <- function(datos) {
  library(lmomco)
  d <- datos[is.finite(datos)]
  lmoms <- lmom.ub(d)
  pars <- parln3(lmoms)
  # pars es lista con elementos: meanlog, sdlog, threshold
  return(list(distribucion = "LogNormal3P", parameters = pars))
}

fit_gamma3p_lmom <- function(datos) {
  library(lmomco)
  d <- datos[is.finite(datos)]
  lmoms <- lmom.ub(d)
  pars <- parpe3(lmoms)
  # pars es lista con elementos: shape, location, scale
  return(list(distribucion = "Gamma3P", parameters = pars))
}

#---- LP3----
dlPIII <- function(x, shape, location, scale) dpearsonIII(log(x), shape, location, scale, log=FALSE)/x
plPIII <- function(q, shape, location, scale) ppearsonIII(log(q), shape, location, scale, lower.tail = TRUE, log.p = FALSE)
qlPIII <- function(p, shape, location, scale) exp(qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE))

fit_logpearson3p <- function(datos) {
  # 1) Limpiar: quedarnos solo con datos finitos y > 0
  datos <- datos[is.finite(datos)]  
  # Limitar: debe haber solo valores positivos y finitos
  if (any(datos <= 0)) {
    warning("Todos los datos mayores que cero para Log-Pearon.")
  }
  m <- mean(log(datos))
  v <- var(log(datos))
  s <- sd(log(datos))
  g <- e1071::skewness(log(datos), type = 2)
  
  my_shape <- (2 / g) ^ 2
  my_scale <- sqrt(v) / sqrt(my_shape) * sign(g)
  my_location <- m - my_scale * my_shape
  
  start <- list(shape = my_shape, location = my_location, scale = my_scale)
  
  LPIII <- fitdist(datos, "lPIII", start = start, control = list(maxit = 1000))
  
  params_lp <- LPIII$estimate

  params <- data.frame(
    Distribucion = "LogPearson3P",
    Parametro    = names(params_lp),
    Valor        = as.numeric(params_lp),
    stringsAsFactors = FALSE
  )
  
  return(list(
    fit    = LPIII,
    params = params_lp
  ))
}

#---- Extraer Parametros----
extract_parameters <- function(fit_list) {
  params_df <- data.frame(
    Distribucion = character(),
    Parametro    = character(),
    Valor        = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (name in names(fit_list)) {
    fit <- fit_list[[name]]
    if (is.null(fit)) next
    if (inherits(fit, "fitdist")) {
      est <- fit$estimate
    } else if (inherits(fit, "fgev") || inherits(fit, "fevd")) {
      est <- fit$results$par
    } else next
    
    for (p in names(est)) {
      params_df <- rbind(params_df,
                         data.frame(
                           Distribucion = name,
                           Parametro    = p,
                           Valor        = est[p],
                           stringsAsFactors = FALSE
                         ))
    }
  }
  return(params_df)
}

extract_lmom3_parameters <- function(fit_ln3, fit_gamma3p) {
  # Recibe dos objetos de ajuste de lmomco: fit_ln3 y fit_gamma3p
  fits <- list(fit_ln3, fit_gamma3p)
  
  params_list <- lapply(fits, function(fit) {
    if (is.null(fit) || is.null(fit$parameters)) return(NULL)
    pars <- fit$parameters
    # Extraer solo vector numérico de parámetros
    if ("para" %in% names(pars) && is.numeric(pars$para)) {
      values <- pars$para
    } else if (is.numeric(pars)) {
      values <- pars
    } else {
      return(NULL)
    }
    data.frame(
      Distribucion = fit$distribucion,
      Parametro    = names(values),
      Valor        = as.numeric(values),
      stringsAsFactors = FALSE
    )
  })
  
  # Combinar data frames válidos
  params_df <- do.call(rbind, params_list)
  return(params_df)
}


#---- Ejemplo----

#---- Test KS ----

library(fitdistrplus)
library(extRemes)
library(lmomco)
library(e1071)
library(PearsonDS)
library(extremeStat)
# 1) Normal (2P) ----------------------------------------------------------
ks_norm <- function(datos) {
  fit <- fitdist(datos, "norm")
  ks.test(
    datos,
    "pnorm",
    mean = fit$estimate["mean"],
    sd   = fit$estimate["sd"]
  )
}

# 2) Gamma (2P) -----------------------------------------------------------
ks_gamma <- function(datos) {
  fit <- fitdist(datos, "gamma")
  ks.test(
    datos,
    "pgamma",
    shape = fit$estimate["shape"],
    rate  = fit$estimate["rate"]
  )
}

# 3) Gumbel (MLE via extRemes) --------------------------------------------
ks_gumbel <- function(datos) {
  fit <- fevd(datos, type = "Gumbel", method = "MLE")
  par <- fit$results$par
  pG <- function(x) exp(-exp(-(x - par["location"])/par["scale"]))
  ks.test(datos, pG)
}

# 4) Log-Normal (2P) ------------------------------------------------------
ks_lognormal <- function(datos) {
  fit <- fitdist(datos, "lnorm")
  ks.test(
    datos,
    "plnorm",
    meanlog = fit$estimate["meanlog"],
    sdlog   = fit$estimate["sdlog"]
  )
}

# 5) Log-Gumbel -----------------------------------------------------------
ks_loggumbel <- function(datos) {
  fit <- fevd(log(datos), type = "Gumbel", method = "MLE")
  par <- fit$results$par
  pLG <- function(x) exp(-exp(-(log(x) - par["location"])/par["scale"]))
  ks.test(datos, pLG)
}

# 6) Log-Normal 3P (LMOM) -------------------------------------------------

ks_lognormal3p <- function(datos) {
  ks.test(datos, "cdfln3", parln3(lmoms(datos)))
}

# 7) Gamma 3P (LMOM) ------------------------------------------------------
ks_gamma3p <- function(datos) {
  # 1) calculas los lmoments
  LM   <- lmomco::lmom.ub(datos)
  # 2) creas el objeto de parámetros “pe3”
  para <- lmomco::lmom2par(LM, "pe3")
  #      ^– esto es un objeto con $type="pe3" y $para = c(shape,loc,sca)
  
  # 3) ejecutas KS usando la CDF interna
  ks.test(
    datos,
    function(q) lmomco::cdfpe3(q, para),
    exact = FALSE
  )
}

# 8) Log-Pearson III (LP3) -----------------------------------------------
ks_logpearson3p <- function(datos) {
  # Usa tu propia función de ajuste
  fit_lp3 <- fit_logpearson3p(datos)
  par     <- fit_lp3$params
  pLP3 <- function(x) plPIII(x,
                             shape    = as.numeric(par["shape"]),
                             location = as.numeric(par["location"]),
                             scale    = as.numeric(par["scale"]))
  ks.test(datos, pLP3)
}

#---- Ejemplo KS ----

#----------
prettydist(dist.list(type=NULL))
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Normal (2P)
rl_norm <- function(datos, p) {
  fit <- fitdist(datos, "norm")
  qnorm(p,
        mean = fit$estimate["mean"],
        sd   = fit$estimate["sd"])
}

# 2) Gamma (2P)
rl_gamma <- function(datos, p) {
  fit <- fitdist(datos, "gamma")
  qgamma(p,
         shape = fit$estimate["shape"],
         rate  = fit$estimate["rate"])
}

# 3) Gumbel (MLE via extRemes)
rl_gumbel <- function(datos, p) {
  fit <- fevd(datos, type = "Gumbel", method = "MLE")
  par <- fit$results$par
  par["location"] - par["scale"] * log(-log(p))
}

# 4) Log–Normal (2P)
rl_lognormal <- function(datos, p) {
  fit <- fitdist(datos, "lnorm")
  qlnorm(p,
         meanlog = fit$estimate["meanlog"],
         sdlog   = fit$estimate["sdlog"])
}

# 5) Log–Gumbel
rl_loggumbel <- function(datos, p) {
  fit <- fevd(log(datos), type = "Gumbel", method = "MLE")
  par <- fit$results$par
  exp(par["location"] - par["scale"] * log(-log(p)))
}

# 6) Log–Normal 3P (LMOM)
rl_lognormal3p <- function(fit, p) {
  pars_ln3  <- fit$parameters
  qualn3(p, pars_ln3)
}

# 7) Gamma 3P (LMOM)
rl_gamma3p <- function(fit, p) {
  pars_g3p  <- fit$parameters
  quape3(p, pars_g3p)
}


# 8) Log–Pearson III (LP3)
rl_lp3 <- function(datos, p) {
  fit_lp3 <- fit_logpearson3p(datos)$fit
  pars    <- fit_lp3$estimate
  qlPIII(p,
         shape    = pars["shape"],
         location = pars["location"],
         scale    = pars["scale"])
}

# Ejemplo de uso:
TR <- c(2,2.33, 5,10,25,50,100,140, 200,500,1000)
p  <- 1 - (1/TR)


###
datos <- prec
base_fits  <- fit_distributions(datos1)
base_params <- extract_parameters(base_fits)
fit_ln3 <- fit_lognormal3p_lmom(datos1)
fit_pe3 <- fit_gamma3p_lmom(datos1)
params3p <- extract_lmom3_parameters(fit_ln3, fit_pe3)
#Llamar si se sabe que no hay 0
fit_lp3 <- fit_logpearson3p(datos1)
base_fitss  <- fit_log_distributions(datos1)
base_params1 <- extract_parameters(base_fitss)

ks_norm(datos1)
ks_gamma(datos1)
ks_gumbel(datos1)
ks_lognormal(datos1)
ks_loggumbel(datos1)
ks_lognormal3p(datos1)
ks_gamma3p(datos1)
ks_logpearson3p(datos1)

rl_norm(datos1,       p)
rl_gamma(datos1,      p)
rl_gumbel(datos1,     p)
rl_lognormal(datos1, p)
rl_loggumbel(datos1, p)
rl_lognormal3p(fit_ln3, p)
rl_gamma3p(fit_pe3, p)
rl_lp3(datos1, p)









# 4) Pasar a formato largo para ggplot
t1_long <- t1 %>%
  pivot_longer(-TR, names_to = "Distribucion", values_to = "Nivel")

# 5) Graficar curvas de nivel de retorno
ggplot(t1_long, aes(x = TR, y = Nivel, color = Distribucion)) +
  geom_line(size = 1) +
  scale_x_log10(breaks = TR) +
  labs(
    title = "Curvas de Nivel de Retorno",
    x     = "Periodo de Retorno (años)",
    y     = "Precipitación (mm)"
  ) +
  theme_minimal()








data <-read.csv("PMDA.csv")
Bernal_PMDA <- data$Bernal_NA
Bernal_PMDA <- Bernal_PMDA[!is.na(Bernal_PMDA)]

Chusis_PMDA <- data$Chusis_NA
Chusis_PMDA <- Chusis_PMDA[!is.na(Chusis_PMDA)]

Miraflores_PMDA <- data$Miraflores_NA
Miraflores_PMDA <- Miraflores_PMDA[!is.na(Miraflores_PMDA)]

SanMiguel_PMDA <- data$San.Miguel_NA
SanMiguel_PMDA <- SanMiguel_PMDA[!is.na(SanMiguel_PMDA)]

Bernal_PMDA_L <- data$Bernal_Lleno
Chusis_PMDA_L <- data$Chusis_Lleno
Miraflores_PMDA_L <- data$Miraflores_Lleno
SanMiguel_PMDA_L <- data$San.Miguel_Lleno


#-----New Data -----
Alamor <- read.csv("Maximos_Alamor.csv")
Lancones <- read.csv("Maximos_Lancones.csv")
Mallares <- read.csv("Maximos_Mallares_.csv")
Partidor$Partidor <- read.csv("Maximos_Partidor.csv")
Panasca <- read.csv("Maximos_Panasca.csv")
PasajeSur <- read.csv("Maximos_PasajeSur.csv")
CerroArena <- read.csv("Maximos_Cerroarena.csv")


Alamor_PMDA <- Alamor$Alamor
Lancones_PMDA <- Lancones$Lancones
Mallares_PMDA <- Mallares$Mallares
Partidor_PMDA <- Partidor$Partidor
Panasca_PMDA <- Panasca$Panasca
PasajeSur_PMDA <- PasajeSur$PasajeSur
CerroArena_PMDA <- CerroArena$CerroArena


Jayanca <- read.csv("Maximos_Jayanca.csv")
Puchaca <- read.csv("Maximos_Puchaca.csv")

Jayanca_PMDA <- Jayanca$Jayanca
Puchaca_PMDA <- Puchaca$Puchaca


#---- Bernal ----

Bernal_PMDA_fits  <- fit_distributions(Bernal_PMDA)
Bernal_PMDA_fits
Bernal_PMDA_params <- extract_parameters(Bernal_PMDA_fits)
Bernal_PMDAfit_ln3 <- fit_lognormal3p_lmom(Bernal_PMDA)
Bernal_PMDAfit_ln3
Bernal_PMDAfit_pe3 <- fit_gamma3p_lmom(Bernal_PMDA)
Bernal_PMDAfit_pe3
Bernal_PMDAparams3p <- extract_lmom3_parameters(Bernal_PMDAfit_ln3, Bernal_PMDAfit_pe3)
Bernal_PMDAfit_lp3 <- fit_logpearson3p(Bernal_PMDA)
Bernal_PMDAfit_lp3
Bernal_PMDAbase_fitss  <- fit_log_distributions(Bernal_PMDA)
Bernal_PMDAbase_fitss
Bernal_PMDAbase_params1 <- extract_parameters(Bernal_PMDAbase_fitss)

ks_norm(Bernal_PMDA)
ks_gamma(Bernal_PMDA)
ks_gumbel(Bernal_PMDA)
ks_lognormal(Bernal_PMDA)
ks_loggumbel(Bernal_PMDA)
ks_lognormal3p(Bernal_PMDA)
ks_gamma3p(Bernal_PMDA)
ks_logpearson3p(Bernal_PMDA)

rl_norm(Bernal_PMDA,p)
rl_gamma(Bernal_PMDA,p)
rl_gumbel(Bernal_PMDA,p)
rl_lognormal(Bernal_PMDA, p)
rl_loggumbel(Bernal_PMDA, p)
rl_lognormal3p(Bernal_PMDAfit_ln3, p)
rl_gamma3p(Bernal_PMDAfit_pe3, p)
rl_lp3(Bernal_PMDA, p)

#---- Chusis ----
Chusis_PMDA_fits  <- fit_distributions(Chusis_PMDA)
Chusis_PMDA_fits
Chusis_PMDA_params <- extract_parameters(Chusis_PMDA_fits)
Chusis_PMDAfit_ln3 <- fit_lognormal3p_lmom(Chusis_PMDA)
Chusis_PMDAfit_ln3
Chusis_PMDAfit_pe3 <- fit_gamma3p_lmom(Chusis_PMDA)
Chusis_PMDAfit_pe3
Chusis_PMDAparams3p <- extract_lmom3_parameters(Chusis_PMDAfit_ln3, Chusis_PMDAfit_pe3)
Chusis_PMDAfit_lp3 <- fit_logpearson3p(Chusis_PMDA)
Chusis_PMDAfit_lp3
Chusis_PMDAbase_fitss  <- fit_log_distributions(Chusis_PMDA)
Chusis_PMDAbase_fitss
Chusis_PMDAbase_params1 <- extract_parameters(Chusis_PMDAbase_fitss)

ks_norm(Chusis_PMDA)
ks_gamma(Chusis_PMDA)
ks_gumbel(Chusis_PMDA)
ks_lognormal(Chusis_PMDA)
ks_loggumbel(Chusis_PMDA)
ks_lognormal3p(Chusis_PMDA)
ks_gamma3p(Chusis_PMDA)
ks_logpearson3p(Chusis_PMDA)

rl_norm(Chusis_PMDA,p)
rl_gamma(Chusis_PMDA,p)
rl_gumbel(Chusis_PMDA,p)
rl_lognormal(Chusis_PMDA, p)
rl_loggumbel(Chusis_PMDA, p)
rl_lognormal3p(Chusis_PMDAfit_ln3, p)
rl_gamma3p(Chusis_PMDAfit_pe3, p)
rl_lp3(Chusis_PMDA, p)

#---- Miraflores ---- 
Miraflores_PMDA_fits  <- fit_distributions(Miraflores_PMDA)
Miraflores_PMDA_fits
Miraflores_PMDA_params <- extract_parameters(Miraflores_PMDA_fits)
Miraflores_PMDAfit_ln3 <- fit_lognormal3p_lmom(Miraflores_PMDA)
Miraflores_PMDAfit_ln3
Miraflores_PMDAfit_pe3 <- fit_gamma3p_lmom(Miraflores_PMDA)
Miraflores_PMDAfit_pe3
Miraflores_PMDAparams3p <- extract_lmom3_parameters(Miraflores_PMDAfit_ln3, Miraflores_PMDAfit_pe3)
Miraflores_PMDAfit_lp3 <- fit_logpearson3p(Miraflores_PMDA)
Miraflores_PMDAfit_lp3
Miraflores_PMDAbase_fitss  <- fit_log_distributions(Miraflores_PMDA)
Miraflores_PMDAbase_fitss
Miraflores_PMDAbase_params1 <- extract_parameters(Miraflores_PMDAbase_fitss)

ks_norm(Miraflores_PMDA)
ks_gamma(Miraflores_PMDA)
ks_gumbel(Miraflores_PMDA)
ks_lognormal(Miraflores_PMDA)
ks_loggumbel(Miraflores_PMDA)
ks_lognormal3p(Miraflores_PMDA)
ks_gamma3p(Miraflores_PMDA)
ks_logpearson3p(Miraflores_PMDA)

rl_norm(Miraflores_PMDA,p)
rl_gamma(Miraflores_PMDA,p)
rl_gumbel(Miraflores_PMDA,p)
rl_lognormal(Miraflores_PMDA, p)
rl_loggumbel(Miraflores_PMDA, p)
rl_lognormal3p(Miraflores_PMDAfit_ln3, p)
rl_gamma3p(Miraflores_PMDAfit_pe3, p)
rl_lp3(Miraflores_PMDA, p)

#---- San Miguel ---- 
SanMiguel_PMDA_fits  <- fit_distributions(SanMiguel_PMDA)
SanMiguel_PMDA_fits
SanMiguel_PMDA_params <- extract_parameters(SanMiguel_PMDA_fits)
SanMiguel_PMDAfit_ln3 <- fit_lognormal3p_lmom(SanMiguel_PMDA)
SanMiguel_PMDAfit_ln3
SanMiguel_PMDAfit_pe3 <- fit_gamma3p_lmom(SanMiguel_PMDA)
SanMiguel_PMDAfit_pe3
SanMiguel_PMDAparams3p <- extract_lmom3_parameters(SanMiguel_PMDAfit_ln3, SanMiguel_PMDAfit_pe3)
SanMiguel_PMDAfit_lp3 <- fit_logpearson3p(SanMiguel_PMDA)
SanMiguel_PMDAfit_lp3
SanMiguel_PMDAbase_fitss  <- fit_log_distributions(SanMiguel_PMDA)
SanMiguel_PMDAbase_fitss
SanMiguel_PMDAbase_params1 <- extract_parameters(SanMiguel_PMDAbase_fitss)

ks_norm(SanMiguel_PMDA)
ks_gamma(SanMiguel_PMDA)
ks_gumbel(SanMiguel_PMDA)
ks_lognormal(SanMiguel_PMDA)
ks_loggumbel(SanMiguel_PMDA)
ks_lognormal3p(SanMiguel_PMDA)
ks_gamma3p(SanMiguel_PMDA)
ks_logpearson3p(SanMiguel_PMDA)

rl_norm(SanMiguel_PMDA,p)
rl_gamma(SanMiguel_PMDA,p)
rl_gumbel(SanMiguel_PMDA,p)
rl_lognormal(SanMiguel_PMDA, p)
rl_loggumbel(SanMiguel_PMDA, p)
rl_lognormal3p(SanMiguel_PMDAfit_ln3, p)
rl_gamma3p(SanMiguel_PMDAfit_pe3, p)
rl_lp3(SanMiguel_PMDA, p)





#---- Bernal_PMDA_L ----
Bernal_PMDA_L_fits  <- fit_distributions(Bernal_PMDA_L)
Bernal_PMDA_L_fits
Bernal_PMDA_L_params <- extract_parameters(Bernal_PMDA_L_fits)
Bernal_PMDA_Lfit_ln3 <- fit_lognormal3p_lmom(Bernal_PMDA_L)
Bernal_PMDA_Lfit_ln3
Bernal_PMDA_Lfit_pe3 <- fit_gamma3p_lmom(Bernal_PMDA_L)
Bernal_PMDA_Lfit_pe3
Bernal_PMDA_Lparams3p <- extract_lmom3_parameters(Bernal_PMDA_Lfit_ln3, Bernal_PMDA_Lfit_pe3)
Bernal_PMDA_Lfit_lp3 <- fit_logpearson3p(Bernal_PMDA_L)
Bernal_PMDA_Lfit_lp3
Bernal_PMDA_Lbase_fitss  <- fit_log_distributions(Bernal_PMDA_L)
Bernal_PMDA_Lbase_fitss
Bernal_PMDA_Lbase_params1 <- extract_parameters(Bernal_PMDA_Lbase_fitss)

ks_norm(Bernal_PMDA_L)
ks_gamma(Bernal_PMDA_L)
ks_gumbel(Bernal_PMDA_L)
ks_lognormal(Bernal_PMDA_L)
ks_loggumbel(Bernal_PMDA_L)
ks_lognormal3p(Bernal_PMDA_L)
ks_gamma3p(Bernal_PMDA_L)
ks_logpearson3p(Bernal_PMDA_L)

rl_norm(Bernal_PMDA_L,p)
rl_gamma(Bernal_PMDA_L,p)
rl_gumbel(Bernal_PMDA_L,p)
rl_lognormal(Bernal_PMDA_L, p)
rl_loggumbel(Bernal_PMDA_L, p)
rl_lognormal3p(Bernal_PMDA_Lfit_ln3, p)
rl_gamma3p(Bernal_PMDA_Lfit_pe3, p)
rl_lp3(Bernal_PMDA_L, p)

#---- Chusis_PMDA_L ----
Chusis_PMDA_L_fits  <- fit_distributions(Chusis_PMDA_L)
Chusis_PMDA_L_fits
Chusis_PMDA_L_params <- extract_parameters(Chusis_PMDA_L_fits)
Chusis_PMDA_Lfit_ln3 <- fit_lognormal3p_lmom(Chusis_PMDA_L)
Chusis_PMDA_Lfit_ln3
Chusis_PMDA_Lfit_pe3 <- fit_gamma3p_lmom(Chusis_PMDA_L)
Chusis_PMDA_Lfit_pe3
Chusis_PMDA_Lparams3p <- extract_lmom3_parameters(Chusis_PMDA_Lfit_ln3, Chusis_PMDA_Lfit_pe3)
Chusis_PMDA_Lfit_lp3 <- fit_logpearson3p(Chusis_PMDA_L)
Chusis_PMDA_Lfit_lp3
Chusis_PMDA_Lbase_fitss  <- fit_log_distributions(Chusis_PMDA_L)
Chusis_PMDA_Lbase_fitss
Chusis_PMDA_Lbase_params1 <- extract_parameters(Chusis_PMDA_Lbase_fitss)

ks_norm(Chusis_PMDA_L)
ks_gamma(Chusis_PMDA_L)
ks_gumbel(Chusis_PMDA_L)
ks_lognormal(Chusis_PMDA_L)
ks_loggumbel(Chusis_PMDA_L)
ks_lognormal3p(Chusis_PMDA_L)
ks_gamma3p(Chusis_PMDA_L)
ks_logpearson3p(Chusis_PMDA_L)

rl_norm(Chusis_PMDA_L,p)
rl_gamma(Chusis_PMDA_L,p)
rl_gumbel(Chusis_PMDA_L,p)
rl_lognormal(Chusis_PMDA_L, p)
rl_loggumbel(Chusis_PMDA_L, p)
rl_lognormal3p(Chusis_PMDA_Lfit_ln3, p)
rl_gamma3p(Chusis_PMDA_Lfit_pe3, p)
rl_lp3(Chusis_PMDA_L, p)



#---- Miraflores_PMDA_L ----
Miraflores_PMDA_L_fits  <- fit_distributions(Miraflores_PMDA_L)
Miraflores_PMDA_L_fits
Miraflores_PMDA_L_params <- extract_parameters(Miraflores_PMDA_L_fits)
Miraflores_PMDA_Lfit_ln3 <- fit_lognormal3p_lmom(Miraflores_PMDA_L)
Miraflores_PMDA_Lfit_ln3
Miraflores_PMDA_Lfit_pe3 <- fit_gamma3p_lmom(Miraflores_PMDA_L)
Miraflores_PMDA_Lfit_pe3
Miraflores_PMDA_Lparams3p <- extract_lmom3_parameters(Miraflores_PMDA_Lfit_ln3, Miraflores_PMDA_Lfit_pe3)
Miraflores_PMDA_Lfit_lp3 <- fit_logpearson3p(Miraflores_PMDA_L)
Miraflores_PMDA_Lfit_lp3
Miraflores_PMDA_Lbase_fitss  <- fit_log_distributions(Miraflores_PMDA_L)
Miraflores_PMDA_Lbase_fitss
Miraflores_PMDA_Lbase_params1 <- extract_parameters(Miraflores_PMDA_Lbase_fitss)

ks_norm(Miraflores_PMDA_L)
ks_gamma(Miraflores_PMDA_L)
ks_gumbel(Miraflores_PMDA_L)
ks_lognormal(Miraflores_PMDA_L)
ks_loggumbel(Miraflores_PMDA_L)
ks_lognormal3p(Miraflores_PMDA_L)
ks_gamma3p(Miraflores_PMDA_L)
ks_logpearson3p(Miraflores_PMDA_L)

rl_norm(Miraflores_PMDA_L,p)
rl_gamma(Miraflores_PMDA_L,p)
rl_gumbel(Miraflores_PMDA_L,p)
rl_lognormal(Miraflores_PMDA_L, p)
rl_loggumbel(Miraflores_PMDA_L, p)
rl_lognormal3p(Miraflores_PMDA_Lfit_ln3, p)
rl_gamma3p(Miraflores_PMDA_Lfit_pe3, p)
rl_lp3(Miraflores_PMDA_L, p)

#---- SanMiguel_PMDA_L ----
SanMiguel_PMDA_L_fits  <- fit_distributions(SanMiguel_PMDA_L)
SanMiguel_PMDA_L_fits
SanMiguel_PMDA_L_params <- extract_parameters(SanMiguel_PMDA_L_fits)
SanMiguel_PMDA_Lfit_ln3 <- fit_lognormal3p_lmom(SanMiguel_PMDA_L)
SanMiguel_PMDA_Lfit_ln3
SanMiguel_PMDA_Lfit_pe3 <- fit_gamma3p_lmom(SanMiguel_PMDA_L)
SanMiguel_PMDA_Lfit_pe3
SanMiguel_PMDA_Lparams3p <- extract_lmom3_parameters(SanMiguel_PMDA_Lfit_ln3, SanMiguel_PMDA_Lfit_pe3)
SanMiguel_PMDA_Lfit_lp3 <- fit_logpearson3p(SanMiguel_PMDA_L)
SanMiguel_PMDA_Lfit_lp3
SanMiguel_PMDA_Lbase_fitss  <- fit_log_distributions(SanMiguel_PMDA_L)
SanMiguel_PMDA_Lbase_fitss
SanMiguel_PMDA_Lbase_params1 <- extract_parameters(SanMiguel_PMDA_Lbase_fitss)

ks_norm(SanMiguel_PMDA_L)
ks_gamma(SanMiguel_PMDA_L)
ks_gumbel(SanMiguel_PMDA_L)
ks_lognormal(SanMiguel_PMDA_L)
ks_loggumbel(SanMiguel_PMDA_L)
ks_lognormal3p(SanMiguel_PMDA_L)
ks_gamma3p(SanMiguel_PMDA_L)
ks_logpearson3p(SanMiguel_PMDA_L)

rl_norm(SanMiguel_PMDA_L,p)
rl_gamma(SanMiguel_PMDA_L,p)
rl_gumbel(SanMiguel_PMDA_L,p)
rl_lognormal(SanMiguel_PMDA_L, p)
rl_loggumbel(SanMiguel_PMDA_L, p)
rl_lognormal3p(SanMiguel_PMDA_Lfit_ln3, p)
rl_gamma3p(SanMiguel_PMDA_Lfit_pe3, p)
rl_lp3(SanMiguel_PMDA_L, p)


#---- Alamor ----
Alamor_PMDA_fits  <- fit_distributions(Alamor_PMDA)
Alamor_PMDA_fits
Alamor_PMDA_params <- extract_parameters(Alamor_PMDA_fits)
Alamor_PMDAfit_ln3 <- fit_lognormal3p_lmom(Alamor_PMDA)
Alamor_PMDAfit_ln3
Alamor_PMDAfit_pe3 <- fit_gamma3p_lmom(Alamor_PMDA)
Alamor_PMDAfit_pe3
Alamor_PMDAparams3p <- extract_lmom3_parameters(Alamor_PMDAfit_ln3, Alamor_PMDAfit_pe3)
Alamor_PMDAfit_lp3 <- fit_logpearson3p(Alamor_PMDA)
Alamor_PMDAfit_lp3
Alamor_PMDAbase_fitss  <- fit_log_distributions(Alamor_PMDA)
Alamor_PMDAbase_fitss
Alamor_PMDAbase_params1 <- extract_parameters(Alamor_PMDAbase_fitss)

ks_norm(Alamor_PMDA)
ks_gamma(Alamor_PMDA)
ks_gumbel(Alamor_PMDA)
ks_lognormal(Alamor_PMDA)
ks_loggumbel(Alamor_PMDA)
ks_lognormal3p(Alamor_PMDA)
ks_gamma3p(Alamor_PMDA)
ks_logpearson3p(Alamor_PMDA)

rl_norm(Alamor_PMDA, p)
rl_gamma(Alamor_PMDA, p)
rl_gumbel(Alamor_PMDA, p)
rl_lognormal(Alamor_PMDA, p)
rl_loggumbel(Alamor_PMDA, p)
rl_lognormal3p(Alamor_PMDAfit_ln3, p)
rl_gamma3p(Alamor_PMDAfit_pe3, p)
rl_lp3(Alamor_PMDA, p)

station_name_Alamor <- "Alamor"
cat("Procesando", station_name_Alamor, "\n")
Alamor_fit <- fit_and_plot(Alamor_PMDA, station_name_Alamor)
Alamor_fit$dlf$parameter$gev
Alamor_fit$dlf$parameter$glo
Alamor_fit$dlf$parameter$gno
Alamor_fit$dlf$parameter$gpa
Alamor_fit$dlf$parameter$wak
Alamor_fit$dle$returnlev
Alamor_gof <- goodness_of_fit(Alamor_PMDA, Alamor_fit)
print(Alamor_gof)


#---- Lancones ----

Lancones_PMDA_fits  <- fit_distributions(Lancones_PMDA)
Lancones_PMDA_fits
Lancones_PMDA_params <- extract_parameters(Lancones_PMDA_fits)
Lancones_PMDAfit_ln3 <- fit_lognormal3p_lmom(Lancones_PMDA)
Lancones_PMDAfit_ln3
Lancones_PMDAfit_pe3 <- fit_gamma3p_lmom(Lancones_PMDA)
Lancones_PMDAfit_pe3
Lancones_PMDAparams3p <- extract_lmom3_parameters(Lancones_PMDAfit_ln3, Lancones_PMDAfit_pe3)
Lancones_PMDAfit_lp3 <- fit_logpearson3p(Lancones_PMDA)
Lancones_PMDAfit_lp3
Lancones_PMDAbase_fitss  <- fit_log_distributions(Lancones_PMDA)
Lancones_PMDAbase_fitss
Lancones_PMDAbase_params1 <- extract_parameters(Lancones_PMDAbase_fitss)

ks_norm(Lancones_PMDA)
ks_gamma(Lancones_PMDA)
ks_gumbel(Lancones_PMDA)
ks_lognormal(Lancones_PMDA)
ks_loggumbel(Lancones_PMDA)
ks_lognormal3p(Lancones_PMDA)
ks_gamma3p(Lancones_PMDA)
ks_logpearson3p(Lancones_PMDA)

rl_norm(Lancones_PMDA, p)
rl_gamma(Lancones_PMDA, p)
rl_gumbel(Lancones_PMDA, p)
rl_lognormal(Lancones_PMDA, p)
rl_loggumbel(Lancones_PMDA, p)
rl_lognormal3p(Lancones_PMDAfit_ln3, p)
rl_gamma3p(Lancones_PMDAfit_pe3, p)
rl_lp3(Lancones_PMDA, p)

station_name_Lancones <- "Lancones"
cat("Procesando", station_name_Lancones, "\n")
Lancones_fit <- fit_and_plot(Lancones_PMDA, station_name_Lancones)
Lancones_fit$dlf$parameter$gev
Lancones_fit$dlf$parameter$glo
Lancones_fit$dlf$parameter$gno
Lancones_fit$dlf$parameter$gpa
Lancones_fit$dlf$parameter$wak
Lancones_fit$dle$returnlev
Lancones_gof <- goodness_of_fit(Lancones_PMDA, Lancones_fit)
print(Lancones_gof)

#---- Mallares ----


Mallares_PMDA_fits  <- fit_distributions(Mallares_PMDA)
Mallares_PMDA_fits
Mallares_PMDA_params <- extract_parameters(Mallares_PMDA_fits)
Mallares_PMDAfit_ln3 <- fit_lognormal3p_lmom(Mallares_PMDA)
Mallares_PMDAfit_ln3
Mallares_PMDAfit_pe3 <- fit_gamma3p_lmom(Mallares_PMDA)
Mallares_PMDAfit_pe3
Mallares_PMDAparams3p <- extract_lmom3_parameters(Mallares_PMDAfit_ln3, Mallares_PMDAfit_pe3)
Mallares_PMDAfit_lp3 <- fit_logpearson3p(Mallares_PMDA)
Mallares_PMDAfit_lp3
Mallares_PMDAbase_fitss  <- fit_log_distributions(Mallares_PMDA)
Mallares_PMDAbase_fitss
Mallares_PMDAbase_params1 <- extract_parameters(Mallares_PMDAbase_fitss)

ks_norm(Mallares_PMDA)
ks_gamma(Mallares_PMDA)
ks_gumbel(Mallares_PMDA)
ks_lognormal(Mallares_PMDA)
ks_loggumbel(Mallares_PMDA)
ks_lognormal3p(Mallares_PMDA)
ks_gamma3p(Mallares_PMDA)
ks_logpearson3p(Mallares_PMDA)

rl_norm(Mallares_PMDA, p)
rl_gamma(Mallares_PMDA, p)
rl_gumbel(Mallares_PMDA, p)
rl_lognormal(Mallares_PMDA, p)
rl_loggumbel(Mallares_PMDA, p)
rl_lognormal3p(Mallares_PMDAfit_ln3, p)
rl_gamma3p(Mallares_PMDAfit_pe3, p)
rl_lp3(Mallares_PMDA, p)

station_name_Mallares <- "Mallares"
cat("Procesando", station_name_Mallares, "\n")
Mallares_fit <- fit_and_plot(Mallares_PMDA, station_name_Mallares)
Mallares_fit$dlf$parameter$gev
Mallares_fit$dlf$parameter$glo
Mallares_fit$dlf$parameter$gno
Mallares_fit$dlf$parameter$gpa
Mallares_fit$dlf$parameter$wak
Mallares_fit$dle$returnlev
Mallares_gof <- goodness_of_fit(Mallares_PMDA, Mallares_fit)
print(Mallares_gof)

#---- Partidor ----

Partidor_PMDA_fits  <- fit_distributions(Partidor_PMDA)
Partidor_PMDA_fits
Partidor_PMDA_params <- extract_parameters(Partidor_PMDA_fits)
Partidor_PMDAfit_ln3 <- fit_lognormal3p_lmom(Partidor_PMDA)
Partidor_PMDAfit_ln3
Partidor_PMDAfit_pe3 <- fit_gamma3p_lmom(Partidor_PMDA)
Partidor_PMDAfit_pe3
Partidor_PMDAparams3p <- extract_lmom3_parameters(Partidor_PMDAfit_ln3, Partidor_PMDAfit_pe3)
Partidor_PMDAfit_lp3 <- fit_logpearson3p(Partidor_PMDA)
Partidor_PMDAfit_lp3
Partidor_PMDAbase_fitss  <- fit_log_distributions(Partidor_PMDA)
Partidor_PMDAbase_fitss
Partidor_PMDAbase_params1 <- extract_parameters(Partidor_PMDAbase_fitss)

ks_norm(Partidor_PMDA)
ks_gamma(Partidor_PMDA)
ks_gumbel(Partidor_PMDA)
ks_lognormal(Partidor_PMDA)
ks_loggumbel(Partidor_PMDA)
ks_lognormal3p(Partidor_PMDA)
ks_gamma3p(Partidor_PMDA)
ks_logpearson3p(Partidor_PMDA)

rl_norm(Partidor_PMDA, p)
rl_gamma(Partidor_PMDA, p)
rl_gumbel(Partidor_PMDA, p)
rl_lognormal(Partidor_PMDA, p)
rl_loggumbel(Partidor_PMDA, p)
rl_lognormal3p(Partidor_PMDAfit_ln3, p)
rl_gamma3p(Partidor_PMDAfit_pe3, p)
rl_lp3(Partidor_PMDA, p)

#---- Panasca ----

Panasca_PMDA_fits  <- fit_distributions(Panasca_PMDA)
Panasca_PMDA_fits
Panasca_PMDA_params <- extract_parameters(Panasca_PMDA_fits)
Panasca_PMDAfit_ln3 <- fit_lognormal3p_lmom(Panasca_PMDA)
Panasca_PMDAfit_ln3
Panasca_PMDAfit_pe3 <- fit_gamma3p_lmom(Panasca_PMDA)
Panasca_PMDAfit_pe3
Panasca_PMDAparams3p <- extract_lmom3_parameters(Panasca_PMDAfit_ln3, Panasca_PMDAfit_pe3)
Panasca_PMDAfit_lp3 <- fit_logpearson3p(Panasca_PMDA)
Panasca_PMDAfit_lp3
Panasca_PMDAbase_fitss  <- fit_log_distributions(Panasca_PMDA)
Panasca_PMDAbase_fitss
Panasca_PMDAbase_params1 <- extract_parameters(Panasca_PMDAbase_fitss)

ks_norm(Panasca_PMDA)
ks_gamma(Panasca_PMDA)
ks_gumbel(Panasca_PMDA)
ks_lognormal(Panasca_PMDA)
ks_loggumbel(Panasca_PMDA)
ks_lognormal3p(Panasca_PMDA)
ks_gamma3p(Panasca_PMDA)
ks_logpearson3p(Panasca_PMDA)

rl_norm(Panasca_PMDA, p)
rl_gamma(Panasca_PMDA, p)
rl_gumbel(Panasca_PMDA, p)
rl_lognormal(Panasca_PMDA, p)
rl_loggumbel(Panasca_PMDA, p)
rl_lognormal3p(Panasca_PMDAfit_ln3, p)
rl_gamma3p(Panasca_PMDAfit_pe3, p)
rl_lp3(Panasca_PMDA, p)

station_name_Panasca <- "Panasca"
cat("Procesando", station_name_Panasca, "\n")
Panasca_fit <- fit_and_plot(Panasca_PMDA, station_name_Panasca)
Panasca_fit$dlf$parameter$gev
Panasca_fit$dlf$parameter$glo
Panasca_fit$dlf$parameter$gno
Panasca_fit$dlf$parameter$gpa
Panasca_fit$dlf$parameter$wak
Panasca_fit$dle$returnlev
Panasca_gof <- goodness_of_fit(Panasca_PMDA, Panasca_fit)
print(Panasca_gof)

#---- Pasaje ----

PasajeSur_PMDA_fits  <- fit_distributions(PasajeSur_PMDA)
PasajeSur_PMDA_fits
PasajeSur_PMDA_params <- extract_parameters(PasajeSur_PMDA_fits)
PasajeSur_PMDAfit_ln3 <- fit_lognormal3p_lmom(PasajeSur_PMDA)
PasajeSur_PMDAfit_ln3
PasajeSur_PMDAfit_pe3 <- fit_gamma3p_lmom(PasajeSur_PMDA)
PasajeSur_PMDAfit_pe3
PasajeSur_PMDAparams3p <- extract_lmom3_parameters(PasajeSur_PMDAfit_ln3, PasajeSur_PMDAfit_pe3)
PasajeSur_PMDAfit_lp3 <- fit_logpearson3p(PasajeSur_PMDA)
PasajeSur_PMDAfit_lp3
PasajeSur_PMDAbase_fitss  <- fit_log_distributions(PasajeSur_PMDA)
PasajeSur_PMDAbase_fitss
PasajeSur_PMDAbase_params1 <- extract_parameters(PasajeSur_PMDAbase_fitss)

ks_norm(PasajeSur_PMDA)
ks_gamma(PasajeSur_PMDA)
ks_gumbel(PasajeSur_PMDA)
ks_lognormal(PasajeSur_PMDA)
ks_loggumbel(PasajeSur_PMDA)
ks_lognormal3p(PasajeSur_PMDA)
ks_gamma3p(PasajeSur_PMDA)
ks_logpearson3p(PasajeSur_PMDA)

rl_norm(PasajeSur_PMDA, p)
rl_gamma(PasajeSur_PMDA, p)
rl_gumbel(PasajeSur_PMDA, p)
rl_lognormal(PasajeSur_PMDA, p)
rl_loggumbel(PasajeSur_PMDA, p)
rl_lognormal3p(PasajeSur_PMDAfit_ln3, p)
rl_gamma3p(PasajeSur_PMDAfit_pe3, p)
rl_lp3(PasajeSur_PMDA, p)

station_name_PasajeSur <- "PasajeSur"
cat("Procesando", station_name_PasajeSur, "\n")
PasajeSur_fit <- fit_and_plot(PasajeSur_PMDA, station_name_PasajeSur)
PasajeSur_fit$dlf$parameter$gev
PasajeSur_fit$dlf$parameter$glo
PasajeSur_fit$dlf$parameter$gno
PasajeSur_fit$dlf$parameter$gpa
PasajeSur_fit$dlf$parameter$wak
PasajeSur_fit$dle$returnlev
PasajeSur_gof <- goodness_of_fit(PasajeSur_PMDA, PasajeSur_fit)
print(PasajeSur_gof)

#---- Cerro ----

CerroArena_PMDA_fits  <- fit_distributions(CerroArena_PMDA)
CerroArena_PMDA_fits
CerroArena_PMDA_params <- extract_parameters(CerroArena_PMDA_fits)
CerroArena_PMDAfit_ln3 <- fit_lognormal3p_lmom(CerroArena_PMDA)
CerroArena_PMDAfit_ln3
CerroArena_PMDAfit_pe3 <- fit_gamma3p_lmom(CerroArena_PMDA)
CerroArena_PMDAfit_pe3
CerroArena_PMDAparams3p <- extract_lmom3_parameters(CerroArena_PMDAfit_ln3, CerroArena_PMDAfit_pe3)
CerroArena_PMDAfit_lp3 <- fit_logpearson3p(CerroArena_PMDA)
CerroArena_PMDAfit_lp3
CerroArena_PMDAbase_fitss  <- fit_log_distributions(CerroArena_PMDA)
CerroArena_PMDAbase_fitss
CerroArena_PMDAbase_params1 <- extract_parameters(CerroArena_PMDAbase_fitss)

ks_norm(CerroArena_PMDA)
ks_gamma(CerroArena_PMDA)
ks_gumbel(CerroArena_PMDA)
ks_lognormal(CerroArena_PMDA)
ks_loggumbel(CerroArena_PMDA)
ks_lognormal3p(CerroArena_PMDA)
ks_gamma3p(CerroArena_PMDA)
ks_logpearson3p(CerroArena_PMDA)

rl_norm(CerroArena_PMDA, p)
rl_gamma(CerroArena_PMDA, p)
rl_gumbel(CerroArena_PMDA, p)
rl_lognormal(CerroArena_PMDA, p)
rl_loggumbel(CerroArena_PMDA, p)
rl_lognormal3p(CerroArena_PMDAfit_ln3, p)
rl_gamma3p(CerroArena_PMDAfit_pe3, p)
rl_lp3(CerroArena_PMDA, p)

station_name_CerroArena <- "CerroArena"
cat("Procesando", station_name_CerroArena, "\n")
CerroArena_fit <- fit_and_plot(CerroArena_PMDA, station_name_CerroArena)
CerroArena_fit$dlf$parameter$gev
CerroArena_fit$dlf$parameter$glo
CerroArena_fit$dlf$parameter$gno
CerroArena_fit$dlf$parameter$gpa
CerroArena_fit$dlf$parameter$wak
CerroArena_fit$dle$returnlev
CerroArena_gof <- goodness_of_fit(CerroArena_PMDA, CerroArena_fit)
print(CerroArena_gof)


#-------
#

# Función para ajustar distribuciones y graficar
fit_and_plot <- function(data, station_name) {
  # Ajustar distribuciones a los datos
  dlf <- distLfit(data)
  # Calcular periodos de retorno
  dle <- distLextreme(dlf=dlf, RPs=c(2,2.33, 5,10,25,50,100,140, 200,500,1000), gpd=FALSE)
  # Devolver el objeto de ajuste de distribuciones
  return(list(dlf = dlf, dle = dle))
}

# Función para realizar pruebas de bondad de ajuste
goodness_of_fit <- function(data, dlf) {
  # Lista de distribuciones a probar
  distributions <- list(
    GPA = list(cdf = "cdfgpa", par = "pargpa"),
    GEV = list(cdf = "cdfgev", par = "pargev"),
    GLO = list(cdf = "cdfglo", par = "parglo"),
    GNO = list(cdf = "cdfgno", par = "pargno"),
    WAK = list(cdf = "cdfwak", par = "parwak")
  )
  
  # Inicializar un dataframe para almacenar los resultados
  results <- data.frame(Distribucion = character(), Delta = numeric(), D005 = numeric(), Ajuste = character(), stringsAsFactors = FALSE)
  
  # Eliminar valores repetidos en los datos
  data_unique <- unique(data)
  
  # Longitud de los datos sin repetidos
  n <- length(data_unique)
  
  # Calcular D005 según el tamaño de la muestra
  D005 <- if (n < 35) {
    0.000003 * n^4 - 0.0003 * n^3 + 0.0094 * n^2 - 0.1593 * n + 1.3345
  } else {
    1.36 / sqrt(n)
  }
  
  # Probar cada distribución
  for (dist_name in names(distributions)) {
    dist <- distributions[[dist_name]]
    
    # Calcular los parámetros de la distribución
    dist_params <- tryCatch(do.call(dist$par, list(lmoms(data_unique))), error = function(e) NULL)
    
    if (!is.null(dist_params)) {
      # Realizar la prueba de Kolmogorov-Smirnov
      delta <- tryCatch(ks.test(data_unique, dist$cdf, dist_params)$statistic, error = function(e) NA)
      
      # Determinar si el ajuste es bueno
      ajuste <- if (!is.na(delta) && D005 > delta) "Fit" else if (is.na(delta)) "NA in delta" else "No Fit"
      
      # Guardar los resultados
      results <- rbind(results, data.frame(Distribucion = dist_name, Delta = delta, D005 = D005, Ajuste = ajuste))
    }
  }
  
  # Devolver los resultados
  
  return(results)
}


goodness_of_fit <- function(data, dlf) {
  # Lista de distribuciones a probar
  distributions <- list(
    GPA = list(cdf = "cdfgpa", par = "pargpa", qf = "qfpa"),
    GEV = list(cdf = "cdfgev", par = "pargev", qf = "qfgev"),
    GLO = list(cdf = "cdfglo", par = "parglo", qf = "qfglo"),
    GNO = list(cdf = "cdfgno", par = "pargno", qf = "qfgno"),
    WAK = list(cdf = "cdfwak", par = "parwak", qf = "qfwak")
  )
  
  # Inicializar un dataframe para almacenar los resultados
  results <- data.frame(Distribucion = character(), 
                        Delta = numeric(), 
                        D005 = numeric(), 
                        Ajuste = character(),
                        RMSE = numeric(),
                        stringsAsFactors = FALSE)
  
  # Eliminar valores repetidos en los datos
  data_unique <- unique(data)
  
  # Longitud de los datos sin repetidos
  n <- length(data_unique)
  
  # Calcular D005 según el tamaño de la muestra
  D005 <- if (n < 35) {
    0.000003 * n^4 - 0.0003 * n^3 + 0.0094 * n^2 - 0.1593 * n + 1.3345
  } else {
    1.36 / sqrt(n)
  }
  
  # Calcular probabilidades empíricas
  sorted_data <- sort(data_unique)
  emp_probs <- ppoints(n)
  
  # Probar cada distribución
  for (dist_name in names(distributions)) {
    dist <- distributions[[dist_name]]
    
    # Calcular los parámetros de la distribución
    dist_params <- tryCatch(do.call(dist$par, list(lmoms(data_unique))), error = function(e) NULL)
    
    if (!is.null(dist_params)) {
      # Realizar la prueba de Kolmogorov-Smirnov
      delta <- tryCatch(ks.test(data_unique, dist$cdf, dist_params)$statistic, error = function(e) NA)
      
      # Calcular RMSE
      if (!is.null(dist$qf)) {
        tryCatch({
          theoretical_quantiles <- do.call(dist$qf, list(emp_probs, dist_params))
          rmse_value <- sqrt(mean((sorted_data - theoretical_quantiles)^2))
        }, error = function(e) {
          rmse_value <- NA
        })
      } else {
        rmse_value <- NA
      }
      
      # Determinar si el ajuste es bueno
      ajuste <- if (!is.na(delta) && D005 > delta) "Fit" else if (is.na(delta)) "NA in delta" else "No Fit"
      
      # Guardar los resultados
      results <- rbind(results, data.frame(Distribucion = dist_name, 
                                           Delta = delta, 
                                           D005 = D005, 
                                           Ajuste = ajuste,
                                           RMSE = rmse_value))
    }
  }
  
  # Devolver los resultados
  return(results)
}

goodness_of_fit <- function(data, dlf) {
  # Lista de distribuciones a probar
  distributions <- list(
    GPA = list(cdf = "cdfgpa", par = "pargpa", qf = "qfpa"),
    GEV = list(cdf = "cdfgev", par = "pargev", qf = "qfgev"),
    GLO = list(cdf = "cdfglo", par = "parglo", qf = "qfglo"),
    GNO = list(cdf = "cdfgno", par = "pargno", qf = "qfgno"),
    WAK = list(cdf = "cdfwak", par = "parwak", qf = "qfwak")
  )
  
  # Inicializar un dataframe para almacenar los resultados
  results <- data.frame(Distribucion = character(), 
                        Delta = numeric(), 
                        D005 = numeric(), 
                        Ajuste = character(),
                        RMSE = numeric(),
                        stringsAsFactors = FALSE)
  
  # Eliminar valores repetidos en los datos
  data_unique <- unique(data)
  
  # Longitud de los datos sin repetidos
  n <- length(data_unique)
  
  # Calcular D005 según el tamaño de la muestra
  D005 <- if (n < 35) {
    0.000003 * n^4 - 0.0003 * n^3 + 0.0094 * n^2 - 0.1593 * n + 1.3345
  } else {
    1.36 / sqrt(n)
  }
  
  # Calcular probabilidades empíricas
  sorted_data <- sort(data_unique)
  emp_probs <- ppoints(n)
  
  # Probar cada distribución
  for (dist_name in names(distributions)) {
    dist <- distributions[[dist_name]]
    delta <- NA
    rmse_value <- NA
    
    tryCatch({
      # Calcular los parámetros de la distribución
      dist_params <- do.call(dist$par, list(lmoms(data_unique)))
      
      # Realizar la prueba de Kolmogorov-Smirnov
      delta <- ks.test(data_unique, dist$cdf, dist_params)$statistic
      
      # Calcular RMSE
      if (!is.null(dist$qf)) {
        theoretical_quantiles <- do.call(dist$qf, list(emp_probs, dist_params))
        rmse_value <- sqrt(mean((sorted_data - theoretical_quantiles)^2))
      }
    }, error = function(e) {
      # Si hay error, se mantienen los valores NA
    })
    
    # Determinar si el ajuste es bueno
    ajuste <- if (!is.na(delta) && D005 > delta) "Fit" else if (is.na(delta)) "NA in delta" else "No Fit"
    
    # Guardar los resultados
    results <- rbind(results, data.frame(Distribucion = dist_name, 
                                         Delta = delta, 
                                         D005 = D005, 
                                         Ajuste = ajuste,
                                         RMSE = rmse_value))
  }
  
  # Devolver los resultados
  return(results)
}

goodness_of_fit <- function(data, dlf) {
  # Lista de distribuciones a probar con sus respectivas funciones
  distributions <- list(
    GPA = list(cdf = "cdfgpa", par = "pargpa", qf = "quagpa"),
    GEV = list(cdf = "cdfgev", par = "pargev", qf = "quagev"),
    GLO = list(cdf = "cdfglo", par = "parglo", qf = "quaglo"),
    GNO = list(cdf = "cdfgno", par = "pargno", qf = "quagno"),
    WAK = list(cdf = "cdfwak", par = "parwak", qf = "quawak")
  )
  
  # Inicializar dataframe para resultados
  results <- data.frame(Distribucion = character(), 
                        Delta = numeric(), 
                        D005 = numeric(), 
                        Ajuste = character(),
                        RMSE = numeric(),
                        stringsAsFactors = FALSE)
  
  # Eliminar valores repetidos
  data_unique <- unique(data)
  n <- length(data_unique)
  sorted_data <- sort(data_unique)
  emp_probs <- ppoints(n)
  
  # Calcular D005
  D005 <- if (n < 35) {
    0.000003 * n^4 - 0.0003 * n^3 + 0.0094 * n^2 - 0.1593 * n + 1.3345
  } else {
    1.36 / sqrt(n)
  }
  
  # Probar cada distribución
  for (dist_name in names(distributions)) {
    dist <- distributions[[dist_name]]
    delta <- NA
    rmse_value <- NA
    
    try({
      # Obtener parámetros de la distribución
      params <- do.call(dist$par, list(lmoms(data_unique)))
      
      # Prueba Kolmogorov-Smirnov
      delta <- ks.test(data_unique, dist$cdf, params)$statistic
      
      # Calcular RMSE
      if (!is.null(dist$qf)) {
        # Calcular cuantiles teóricos
        theoretical <- do.call(dist$qf, list(emp_probs, params))
        
        # Verificar que no hay NAs y que las longitudes coincidan
        if (length(theoretical) == length(sorted_data) && all(!is.na(theoretical))) {
          rmse_value <- sqrt(mean((sorted_data - theoretical)^2))
        }
      }
    }, silent = TRUE)
    
    # Determinar bondad de ajuste
    ajuste <- if (!is.na(delta) && 0.207 > delta) "Fit" else "No Fit"
    
    # Guardar resultados
    results <- rbind(results, data.frame(
      Distribucion = dist_name,
      Delta = ifelse(is.na(delta), NA, delta),
      D005 = D005,
      Ajuste = ajuste,
      RMSE = ifelse(is.na(rmse_value), NA, rmse_value),
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}


station_name1 <- "Chusis"  # Cambia esto por el nombre de la estación
station_name2 <- "Bernal"
station_name3 <- "Miraflores"
station_name4 <- "SanMiguel"


#Ejemplo
cat("Procesando", Chusis_insitu$Max_Precip, "\n")
Chusis1 <- fit_and_plot(Chusis_insitu$Max_Precip, station_name1)
Chusis
Chusisi1results <- goodness_of_fit(Chusis_insitu$Max_Precip, Chusisinsitu$dlf)
print(Chusisi1results)


library(extremeStat)
library(lmomco)



# Aplicar las funciones a cada estación
cat("Procesando Bernal\n")
Bernal_fit <- fit_and_plot(Bernal_PMDA, "Bernal")
Bernal_fit$dlf$parameter$gev
Bernal_fit$dlf$parameter$glo
Bernal_fit$dlf$parameter$gno
Bernal_fit$dlf$parameter$gpa
Bernal_fit$dlf$parameter$wak
Bernal_fit$dle$returnlev
Bernal_gof <- goodness_of_fit(Bernal_PMDA, Bernal_fit)
print(Bernal_gof)

cat("Procesando Chusis\n")
Chusis_fit <- fit_and_plot(Chusis_PMDA, "Chusis")
Chusis_fit$dlf$parameter$gev
Chusis_fit$dlf$parameter$glo
Chusis_fit$dlf$parameter$gno
Chusis_fit$dlf$parameter$gpa
Chusis_fit$dlf$parameter$wak
Chusis_fit$dle$returnlev
Chusis_gof <- goodness_of_fit(Chusis_PMDA, Chusis_fit)
print(Chusis_gof)

cat("Procesando Miraflores\n")
Miraflores_fit <- fit_and_plot(Miraflores_PMDA, "Miraflores")
Miraflores_fit$dlf$parameter$gev
Miraflores_fit$dlf$parameter$glo
Miraflores_fit$dlf$parameter$gno
Miraflores_fit$dlf$parameter$gpa
Miraflores_fit$dlf$parameter$wak
Miraflores_fit$dle$returnlev
Miraflores_gof <- goodness_of_fit(Miraflores_PMDA, Miraflores_fit)
print(Miraflores_gof)

cat("Procesando San Miguel\n")
SanMiguel_fit <- fit_and_plot(SanMiguel_PMDA, "San Miguel")
SanMiguel_fit$dlf$parameter$gev
SanMiguel_fit$dlf$parameter$glo
SanMiguel_fit$dlf$parameter$gno
SanMiguel_fit$dlf$parameter$gpa
SanMiguel_fit$dlf$parameter$wak
SanMiguel_fit$dle$returnlev
SanMiguel_gof <- goodness_of_fit(SanMiguel_PMDA, SanMiguel_fit)
print(SanMiguel_gof)

# Procesando Bernal
cat("Procesando", data$Bernal_Lleno, "\n")
Bernal1 <- fit_and_plot(data$Bernal_Lleno, "Bernal")
Bernal1$dlf$parameter$gev
Bernal1$dlf$parameter$glo
Bernal1$dlf$parameter$gno
Bernal1$dlf$parameter$gpa
Bernal1$dlf$parameter$wak
Bernal1$dle$returnlev
Bernal1_results <- goodness_of_fit(data$Bernal_Lleno, Bernal1$dlf)
print(Bernal1_results)

# Procesando Chusis
cat("Procesando", data$Chusis_Lleno, "\n")
Chusis1 <- fit_and_plot(data$Chusis_Lleno, "Chusis")
Chusis1$dlf$parameter$gev
Chusis1$dlf$parameter$glo
Chusis1$dlf$parameter$gno
Chusis1$dlf$parameter$gpa
Chusis1$dlf$parameter$wak
Chusis1$dle$returnlev
Chusis1_results <- goodness_of_fit(data$Chusis_Lleno, Chusis1$dlf)
print(Chusis1_results)

# Procesando Miraflores
cat("Procesando", data$Miraflores_Lleno, "\n")
Miraflores1 <- fit_and_plot(data$Miraflores_Lleno, "Miraflores")
Miraflores1$dlf$parameter$gev
Miraflores1$dlf$parameter$glo
Miraflores1$dlf$parameter$gno
Miraflores1$dlf$parameter$gpa
Miraflores1$dlf$parameter$wak
Miraflores1$dle$returnlev
Miraflores1_results <- goodness_of_fit(data$Miraflores_Lleno, Miraflores1$dlf)
print(Miraflores1_results)

# Procesando San Miguel
cat("Procesando", data$San.Miguel_Lleno, "\n")
SanMiguel1 <- fit_and_plot(data$San.Miguel_Lleno, "San Miguel")
SanMiguel1$dlf$parameter$gev
SanMiguel1$dlf$parameter$glo
SanMiguel1$dlf$parameter$gno
SanMiguel1$dlf$parameter$gpa
SanMiguel1$dlf$parameter$wak
SanMiguel1$dle$returnlev
SanMiguel1_results <- goodness_of_fit(data$San.Miguel_Lleno, SanMiguel1$dlf)
print(SanMiguel1_results)





#---------------------------------------




station_name_Alamor <- "Alamor"
cat("Procesando", station_name_Alamor, "\n")
Alamor_fit <- fit_and_plot(Alamor_PMDA, station_name_Alamor)
Alamor_fit$dlf$parameter$gev
Alamor_fit$dlf$parameter$glo
Alamor_fit$dlf$parameter$gno
Alamor_fit$dlf$parameter$gpa
Alamor_fit$dlf$parameter$wak
Alamor_fit$dle$returnlev
Alamor_gof <- goodness_of_fit(Alamor_PMDA, Alamor_fit)
print(Alamor_gof)


station_name_Lancones <- "Lancones"
cat("Procesando", station_name_Lancones, "\n")
Lancones_fit <- fit_and_plot(Lancones_PMDA, station_name_Lancones)
Lancones_fit$dlf$parameter$gev
Lancones_fit$dlf$parameter$glo
Lancones_fit$dlf$parameter$gno
Lancones_fit$dlf$parameter$gpa
Lancones_fit$dlf$parameter$wak
Lancones_fit$dle$returnlev
Lancones_gof <- goodness_of_fit(Lancones_PMDA, Lancones_fit)
print(Lancones_gof)


station_name_Mallares <- "Mallares"
cat("Procesando", station_name_Mallares, "\n")
Mallares_fit <- fit_and_plot(Mallares_PMDA, station_name_Mallares)
Mallares_fit$dlf$parameter$gev
Mallares_fit$dlf$parameter$glo
Mallares_fit$dlf$parameter$gno
Mallares_fit$dlf$parameter$gpa
Mallares_fit$dlf$parameter$wak
Mallares_fit$dle$returnlev
Mallares_gof <- goodness_of_fit(Mallares_PMDA, Mallares_fit)
print(Mallares_gof)


station_name_Panasca <- "Panasca"
cat("Procesando", station_name_Panasca, "\n")
Panasca_fit <- fit_and_plot(Panasca_PMDA, station_name_Panasca)
Panasca_fit$dlf$parameter$gev
Panasca_fit$dlf$parameter$glo
Panasca_fit$dlf$parameter$gno
Panasca_fit$dlf$parameter$gpa
Panasca_fit$dlf$parameter$wak
Panasca_fit$dle$returnlev
Panasca_gof <- goodness_of_fit(Panasca_PMDA, Panasca_fit)
print(Panasca_gof)


station_name_PasajeSur <- "PasajeSur"
cat("Procesando", station_name_PasajeSur, "\n")
PasajeSur_fit <- fit_and_plot(PasajeSur_PMDA, station_name_PasajeSur)
PasajeSur_fit$dlf$parameter$gev
PasajeSur_fit$dlf$parameter$glo
PasajeSur_fit$dlf$parameter$gno
PasajeSur_fit$dlf$parameter$gpa
PasajeSur_fit$dlf$parameter$wak
PasajeSur_fit$dle$returnlev
PasajeSur_gof <- goodness_of_fit(PasajeSur_PMDA, PasajeSur_fit)
print(PasajeSur_gof)


station_name_CerroArena <- "CerroArena"
cat("Procesando", station_name_CerroArena, "\n")
CerroArena_fit <- fit_and_plot(CerroArena_PMDA, station_name_CerroArena)
CerroArena_fit$dlf$parameter$gev
CerroArena_fit$dlf$parameter$glo
CerroArena_fit$dlf$parameter$gno
CerroArena_fit$dlf$parameter$gpa
CerroArena_fit$dlf$parameter$wak
CerroArena_fit$dle$returnlev
CerroArena_gof <- goodness_of_fit(CerroArena_PMDA, CerroArena_fit)
print(CerroArena_gof)











# --- Ajuste de distribuciones ---
Jayanca_PMDA_fits  <- fit_distributions(Jayanca_PMDA)
Jayanca_PMDA_fits
Jayanca_PMDA_params <- extract_parameters(Jayanca_PMDA_fits)
Jayanca_PMDAfit_ln3 <- fit_lognormal3p_lmom(Jayanca_PMDA)
Jayanca_PMDAfit_ln3
Jayanca_PMDAfit_pe3 <- fit_gamma3p_lmom(Jayanca_PMDA)
Jayanca_PMDAfit_pe3
Jayanca_PMDAparams3p <- extract_lmom3_parameters(Jayanca_PMDAfit_ln3, Jayanca_PMDAfit_pe3)
Jayanca_PMDAfit_lp3 <- fit_logpearson3p(Jayanca_PMDA)
Jayanca_PMDAfit_lp3
Jayanca_PMDAbase_fitss  <- fit_log_distributions(Jayanca_PMDA)
Jayanca_PMDAbase_fitss
Jayanca_PMDAbase_params1 <- extract_parameters(Jayanca_PMDAbase_fitss)

# --- Pruebas KS ---
ks_norm(Jayanca_PMDA)
ks_gamma(Jayanca_PMDA)
ks_gumbel(Jayanca_PMDA)
ks_lognormal(Jayanca_PMDA)
ks_loggumbel(Jayanca_PMDA)
ks_lognormal3p(Jayanca_PMDA)
ks_gamma3p(Jayanca_PMDA)
ks_logpearson3p(Jayanca_PMDA)

# --- Niveles de retorno ---
rl_norm(Jayanca_PMDA, p)
rl_gamma(Jayanca_PMDA, p)
rl_gumbel(Jayanca_PMDA, p)
rl_lognormal(Jayanca_PMDA, p)
rl_loggumbel(Jayanca_PMDA, p)
rl_lognormal3p(Jayanca_PMDAfit_ln3, p)
rl_gamma3p(Jayanca_PMDAfit_pe3, p)
rl_lp3(Jayanca_PMDA, p)

# --- Ajuste con fit_and_plot ---
station_name_Jayanca <- "Jayanca"
cat("Procesando", station_name_Jayanca, "\n")
Jayanca_fit <- fit_and_plot(Jayanca_PMDA, station_name_Jayanca)
Jayanca_fit$dlf$parameter$gev
Jayanca_fit$dlf$parameter$glo
Jayanca_fit$dlf$parameter$gno
Jayanca_fit$dlf$parameter$gpa
Jayanca_fit$dlf$parameter$wak
Jayanca_fit$dle$returnlev
Jayanca_gof <- goodness_of_fit(Jayanca_PMDA, Jayanca_fit)
print(Jayanca_gof)







# --- Ajuste de distribuciones ---
Puchaca_PMDA_fits  <- fit_distributions(Puchaca_PMDA)
Puchaca_PMDA_fits
Puchaca_PMDA_params <- extract_parameters(Puchaca_PMDA_fits)
Puchaca_PMDAfit_ln3 <- fit_lognormal3p_lmom(Puchaca_PMDA)
Puchaca_PMDAfit_ln3
Puchaca_PMDAfit_pe3 <- fit_gamma3p_lmom(Puchaca_PMDA)
Puchaca_PMDAfit_pe3
Puchaca_PMDAparams3p <- extract_lmom3_parameters(Puchaca_PMDAfit_ln3, Puchaca_PMDAfit_pe3)
Puchaca_PMDAfit_lp3 <- fit_logpearson3p(Puchaca_PMDA)
Puchaca_PMDAfit_lp3
Puchaca_PMDAbase_fitss  <- fit_log_distributions(Puchaca_PMDA)
Puchaca_PMDAbase_fitss
Puchaca_PMDAbase_params1 <- extract_parameters(Puchaca_PMDAbase_fitss)

# --- Pruebas KS ---
ks_norm(Puchaca_PMDA)
ks_gamma(Puchaca_PMDA)
ks_gumbel(Puchaca_PMDA)
ks_lognormal(Puchaca_PMDA)
ks_loggumbel(Puchaca_PMDA)
ks_lognormal3p(Puchaca_PMDA)
ks_gamma3p(Puchaca_PMDA)
ks_logpearson3p(Puchaca_PMDA)

# --- Niveles de retorno ---
rl_norm(Puchaca_PMDA, p)
rl_gamma(Puchaca_PMDA, p)
rl_gumbel(Puchaca_PMDA, p)
rl_lognormal(Puchaca_PMDA, p)
rl_loggumbel(Puchaca_PMDA, p)
rl_lognormal3p(Puchaca_PMDAfit_ln3, p)
rl_gamma3p(Puchaca_PMDAfit_pe3, p)
rl_lp3(Puchaca_PMDA, p)

# --- Ajuste con fit_and_plot ---
station_name_Puchaca <- "Puchaca"
cat("Procesando", station_name_Puchaca, "\n")
Puchaca_fit <- fit_and_plot(Puchaca_PMDA, station_name_Puchaca)
Puchaca_fit$dlf$parameter$gev
Puchaca_fit$dlf$parameter$glo
Puchaca_fit$dlf$parameter$gno
Puchaca_fit$dlf$parameter$gpa
Puchaca_fit$dlf$parameter$wak
Puchaca_fit$dle$returnlev
Puchaca_gof <- goodness_of_fit(Puchaca_PMDA, Puchaca_fit)
print(Puchaca_gof)








#----------------------------------------

# Cargar datos
df <- read.csv("PMDA.csv")

# Seleccionar columnas numéricas
datos_numericos <- df[sapply(df, is.numeric)]

# Aplicar el filtro IQR para detectar outliers superiores
outliers <- apply(datos_numericos, 2, function(columna) {
  Q1 <- quantile(columna, 0.25, na.rm = TRUE)
  Q3 <- quantile(columna, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  umbral_superior <- Q3 + 1.5 * IQR_val
  columna > umbral_superior
})

# Extraer las filas que contienen al menos un outlier superior
df[outliers %>% as.data.frame() %>% apply(1, any), ]







