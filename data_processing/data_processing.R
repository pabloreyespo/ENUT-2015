rm(list = ls())  # .rs.restartR()
pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats", "lubridate", "haven", "reshape2")
invisible(lapply(pkgs, library, character.only=TRUE))
options(dplyr.summarise.inform = FALSE)

source("data_processing/processing_functions.R")

## PREPROCESAMIENTO DE DATOS
data <- haven::read_dta("data/raw/BASE_USUARIO_corregida.dta")
data <- new_variables_prefilter(data)
data <- na_completion(data)
data <- new_variables_postfilter(data)

grupos <- data %>%
  group_by(quintil, trabaja, tramo_edad,sexo) %>%
  summarise(cuantos = n())

## CLASIFICACIÓN DE ACTIVIDADES
data <- get_25activities(data)

# library(fitdistrplus)
# descdist(data$ttotal_ds_original, boot = 1000)
# fit_gamma  <- fitdist(data$ttotal_ds_original, "gamma")
# fit_normal <- fitdist(data$ttotal_ds_original, "norm")
# fit_lognormal <- fitdist(data$ttotal_ds_original, "lnorm")
#
# par(mfrow = c(2, 2))
# plot.legend <- c("Gamma","Normal","Lognormal")
# denscomp(list(fit_gamma,fit_normal,fit_lognormal), legendtext = plot.legend)
# qqcomp(list(fit_gamma,fit_normal,fit_lognormal), legendtext = plot.legend)
# cdfcomp(list(fit_gamma,fit_normal,fit_lognormal), legendtext = plot.legend)
# ppcomp(list(fit_gamma,fit_normal,fit_lognormal), legendtext = plot.legend)
# par(mfrow=c(1,1))

ggplot(data, aes(x = t_total_ds)) + geom_histogram(binwidth = 1)
ggplot(data, aes(x = t_total_fds)) + geom_histogram(binwidth = 1)

data_outliers <- outlier_detection_Vallejo(data)
#data_outliers <- outlier_detection_mahalanobis(data)
#data_outliers <- outlier_detection_quantiles(data)

ggplot(data_outliers, aes(x = t_total_ds)) + geom_histogram(binwidth = 1)
ggplot(data_outliers, aes(x = t_total_fds)) + geom_histogram(binwidth = 1)

data_outliers <- data_outliers %>% dplyr::select(
  all_of(identificadores),
  dia_semana,
  dia_fin_semana,
  k31_1_1, k31_1_2, k31_1_3,
  #c5:c14,
  all_of(composicion_hogar),
  all_of(proveedores_externos),
  all_of(sociodemograficas),
  all_of(laborales),
  all_of(ingresos),
  all_of(paste0(acts_corregidas, "_ds")),
  all_of(paste0(acts_corregidas, "_fds")),
  t11_1_1:t15_1_1)

# ------------------------------------------------------------------------------
haven::write_dta(data_outliers %>% arrange(id_persona), "data/raw/ENUT_PRE_WEEKEND_IMPUTATION.dta")
write_csv(data_outliers %>% arrange(id_persona), "data/raw/ENUT_PRE_WEEKEND_IMPUTATION.csv")
data <- haven::read_dta("data/raw/ENUT_PRE_WEEKEND_IMPUTATION.dta") %>% arrange(id_persona)
# ------------------------------------------------------------------------------

twin_matrix <- read_csv("data/raw/matriz_gemelos.csv.gzip", col_names = F)
# twin_matrix <- t(as.matrix(twin_matrix))
data_post <- impute_weekend(data, twin_matrix) # TODO arreglar esta función para la nueva implementación de datos
rm(twin_matrix)
data_post <- diagnostico_trabajo(data_post, F, T)
data_post <- adjust_working_hours(data_post)
data_post <- data_to168hours(data_post) # TODO probar distintos posicionamientos

### Continuar con la imputación de gastos, está la opción de usar fractional logit para imputar los gastos
#aux <- data_post %>% filter(ing_trab > 0, tt > 0, trabaja == 1) %>%
#  mutate(w = ing_trab / tt) %>%
#  dplyr::select(ing_personal, ing_trab, w, tt, edad_años, trabaja)

data_descargable <- agregar_actividades(data_post)
data22 <- data_descargable[["data22"]] %>% mutate(w = ing_trab / t_to)
data11 <- data_descargable[["data11"]] %>% mutate(w = ing_trab / t_paid_work)
haven::write_dta(data22, "data/enut-i-22.dta")
haven::write_dta(data11, "data/enut-i-11.dta")
write_csv(data22, "data/enut-i-22.csv")
write_csv(data11, "data/enut-i-11.csv")


source("data_processing/processing_functions.R")

data22 = haven::read_dta( "data/enut-i-22.dta")
data11 = haven::read_dta( "data/enut-i-11.dta")

table(data22$es_trabajador)
table(data22$es_familia)

## ----- Análisis datos resultantes -----------------
data11G <- imputacion_gastos(data11)
data22G <- imputacion_gastos(data22)

haven::write_dta(data22G, "data/enut-i-22G.dta")
haven::write_dta(data11G, "data/enut-i-11G.dta")
write_csv(data22G, "data/enut-i-22G.csv")
write_csv(data11G, "data/enut-i-11G.csv")

ggplot(data11G, aes(x = w)) + geom_histogram(bins = 50)

# data_aux[data_aux==0] <- NA

######## PARA TABLAS DE DATOS #######

#medias <- data22 %>%
#  dplyr::select(id_persona, tt:tv) %>%
#  melt(measure.vars = 2:26, variable.name = "tipo.tiempo") %>%
#  mutate(value = as.numeric(value)) %>% mutate(participa = case_when(value > 0 ~ 1, T ~ 0)) %>%
#  group_by(tipo.tiempo) %>%
#  summarise(participacion = mean(participa),
#            media = mean(value),
#            sd = sd(value))

#sem = c(paste(c(act_trabajo_remunerado, act_busqueda_trabajo, act_preparar_comidas, act_limpieza_vivienda, act_limpieza_ropa, act_mantenimiento_hogar, act_administracion_hogar, act_compras_hogar, act_cuidado_mascotas, act_trabajo_cuidado_dependientes, act_trabajo_cuidado_bebes, act_trabajo_cuidado_niños, act_trabajo_cuidado_adultos, act_trabajo_cuidado_mayores, act_trabajo_no_rem_voluntarios, act_aprendizaje, act_ocio_social, act_ocio_eventos, act_ocio_aficiones, act_ocio_deporte, act_ocio_medioscomunicacion, act_cuidado_personal_general, act_cuidado_personal_comer, act_dormir), "1_2", sep = "_"),
#        act_traslados_ds)

#finsem = c(paste(c(act_trabajo_remunerado, act_busqueda_trabajo, act_preparar_comidas, act_limpieza_vivienda, act_limpieza_ropa, act_mantenimiento_hogar, act_administracion_hogar, act_compras_hogar, act_cuidado_mascotas, act_trabajo_cuidado_dependientes, act_trabajo_cuidado_bebes, act_trabajo_cuidado_niños, act_trabajo_cuidado_adultos, act_trabajo_cuidado_mayores, act_trabajo_no_rem_voluntarios, act_aprendizaje, act_ocio_social, act_ocio_eventos, act_ocio_aficiones, act_ocio_deporte, act_ocio_medioscomunicacion, act_cuidado_personal_general, act_cuidado_personal_comer, act_dormir), "2_2",sep = "_"),
#        act_traslados_fds)

#participacion = data; participacion[sem] = participacion[sem] * 5 + participacion[finsem] * 2
#participacion <- participacion %>%
#  dplyr::select(id_persona, sem) %>%
#  melt(measure.vars = 2:106, variable.name = "tipo.tiempo") %>%
#  mutate(value = as.numeric(value)) %>% mutate(participa = case_when(value > 0 ~ 1, T ~ 0)) %>%
#  group_by(tipo.tiempo) %>%
#  summarise(participacion = mean(participa),
#            media = mean(value),
#            sd = sd(value))

##### Luego de terminar esta versión del procesamiento, hacer una en que los gemelos se escalan antes y otra en que se hace después


### PARA TABLAS RECURRIR A LAS TABLAS DE SCHMID
### GRAFICAR VALORES DEL TIEMPO EN TERMINOS DE BOXPLOT