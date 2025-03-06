pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr",
          "forcats", "lubridate", "haven", "reshape2","apollo")
invisible(lapply(pkgs, library, character.only=TRUE))

acts_corregidas <- c(
    # confiables:
    't_to',
    't_to_js', #job search
    "t_tcnr_ce", # cuidados escenciales
    "t_tcnr_re", # cuidados relativos a la eseñanza
    "t_tcnr_oac", # otros cuidados
    #"t_tcnr_0_4",
    #"t_tcnr_5_14",
    #"t_tcnr_nna",  #(niños y adolescentes)
    #"t_tcnr_15_65",
    #"t_tcnr_66",
    #"t_tncr_psdf"
    "t_tdnr_psc",
    "t_tdnr_lv",
    "t_tdnr_lrc",
    "t_tdnr_mrm",
    "t_tdnr_admnhog",
    "t_tdnr_comphog",
    "t_tdnr_cmp",
    "t_tvaoh_tv", # voluntario a la comunidad
    "t_tvaoh_oh", #voluntario a otros hogares
    "t_cpaf_cp", # cuidados personales
    "t_cpag_comer",
    "t_cpag_dormir",
    "t_ed",
    "t_vsyo_csar", # convivencia social y actividades recreativas
    "t_vsyo_aa", # arte y aficiones
    "t_mcm",
    "t_tt1" # traslados enut 2015
  )

t_agregados <- c(
  "t_paid_work",
  "t_job_search",
  "t_domestic_work",
  "t_care_work",
  "t_unpaid_voluntary",
  "t_education",
  "t_leisure",
  "t_personal_care",
  "t_meals",
  "t_sleep",
  "t_commute1")


identificadores <- c("id_persona", "id_hogar")
composicion_hogar <- c("parentesco",
                       "n_menores6",
                       "n_menores12",
                       "n_men15",
                       "n_menores18",
                       "n_menores",
                       "n_mayores",
                       "n_tiempo",
                       "n_trabajadores",
                       "n_profesionales",
                       "hay_tercera",
                       "n_personas",
                       "edad_promedio",
                       "tiene_hijos",
                       "en_pareja",
                       "vive_pareja")

sociodemograficas <- c("sexo",
                       "edad_años",
                       "tramo_edad",
                       "nivel_escolaridad",
                       "estudia",
                       "trabaja",
                       "horas_trabajo",
                       "quintil",
                       "macrozona",
                       "region",
                       "prop_hogar")
laborales <- c("cae", "cise", "ciuo_agrupada")
proveedores_externos <- c("servicio_domestico", "ayuda_cercanos", "fuentes_externas")
ingresos     <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc", "ing_gpp", "ing_personal", "ingreso_hogar" ,"income_person_week")
tipo_muestra <- c("es_trabajador", "es_familia")

new_variables_prefilter<- function(data) { # OJO, DEBEN ENTRAR TODOS INDEPENDIENTE DE SI DECLARAN TIEMPO O NO
  act_s_domestico <- c("f12_1_1","f15_1_1","f15_1_2","f15_1_3","f16_1_1","f16_1_2","f16_1_3","f22_1_1","f27_1_1",
                       "f27_1_2","f27_1_3","f28_1_1","f28_1_2","f28_1_3", 'f11_1_1', 'f21_1_1')
  recibe_servicio_domestico <- c("f11_1_1","f21_1_1")

  # INFORMACIÓN DEL INDIVIDUO
  data <- data %>%
  mutate(
    menor_edad        = case_when(c14_1_1 < 18 ~ 1, TRUE ~ 0),
    menor6            = case_when(c14_1_1 < 6 ~ 1, TRUE ~ 0),
    menor12           = case_when(c14_1_1 >= 6 & c14_1_1 < 12 ~ 1, TRUE ~ 0),
    men15             = case_when(c14_1_1 < 15 ~ 1, TRUE ~ 0),
    menor18           = case_when(c14_1_1 >= 12 & c14_1_1 < 18 ~ 1, TRUE ~ 0),
    menor25           = case_when(c14_1_1 <= 25 ~ 1, TRUE ~ 0),
    mayor_edad        = case_when(c14_1_1 >= 18 ~ 1, TRUE ~ 0),
    tercera_edad      = case_when(c14_1_1 >= 60 ~ 1, TRUE ~ 0),
    nivel_escolaridad = case_when(
      educ <= 1 ~ 1, # nada
      educ <= 4 ~ 2, # Básica
      educ %in% c(5,6,7,9) ~ 3, # Media
      educ <= 8 ~ 4, # Técnica
      T  ~ 5), # Profesional),
    tiene_hijos = case_when(t11_1_3 < 6 ~ 1, TRUE ~ 0),
    trabaja     = case_when(k11_1_1 == 1 ~ 1, TRUE ~ 0),
    en_pareja   = case_when(c15_1_1 <= 2 ~ 1, TRUE ~0),
    vive_pareja = case_when(c16_1_1 == 1 ~ 1, TRUE ~ 0),
    dia_semana = dia_semana,
    dia_fin_semana = dia_fin_semana + 5
  )

  # COMPOSICIÓN DEL HOGAR
  data <- data %>%
    group_by(id_hogar) %>%
    mutate(n_menores = sum(menor_edad,na.rm=TRUE),
           n_menores6 = sum(menor6,na.rm=TRUE),
           n_menores12 = sum(menor12,na.rm=TRUE),
           n_men15 = sum(men15,na.rm=TRUE),
           n_menores18 = sum(menor18,na.rm=TRUE),
           n_menores25 = sum(menor25, na.rm=TRUE),
           n_mayores = sum(mayor_edad,na.rm=TRUE),
           n_tercera = sum(tercera_edad,na.rm=TRUE),
           n_tiempo = sum(tiempo),
           n_trabajadores = sum(trabaja),
           n_profesionales = sum(nivel_escolaridad >=4 ),
           edad_promedio = mean(c14_1_1)) %>%
    mutate(hay_tercera = case_when(n_tercera > 1 ~ 1 & tercera_edad == 0,  T ~ 0),
           n_personas = n_menores + n_mayores) %>%
    ungroup()

  # AYUDAS QUE RECIBE EL HOGAR
  data <- data %>%
    mutate_at(act_s_domestico, ~ replace_na(.,0)) %>%
    mutate_at(act_s_domestico, ~ifelse(. == 96, 0, .)) %>%
    group_by(id_hogar) %>%
    mutate(
      servicio_domestico = abs(max(f11_1_1) - 2),
      ayuda_cercanos = abs(max(f21_1_1) - 2)) %>%
    mutate(fuentes_externas = case_when(
      servicio_domestico  == 1 | ayuda_cercanos == 1 ~ 1, T ~ 0)) %>%
    ungroup()

  # PREPROCESAMIENTO DE INGRESOS
  data <- data %>%
    # filter(tiempo == 1) %>% # será relevante desaserme de las personas que no declaran tiempo antes?
    mutate_at(c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc"),
              ~replace_na(.,0)) %>%
    mutate(ingresos_propios =  ing_trab + ing_jub_aps) %>%
    group_by(id_hogar) %>%
    mutate(prop_ingresos = ingresos_propios / sum(ingresos_propios)) %>%
    mutate(ingreso_hogar = sum(ingresos_propios) + sum(ing_g)) %>%
    mutate(ing_gpp = sum(ing_g) * prop_ingresos) %>%
    mutate(income_person_week = (sum(ingresos_propios) + ing_gpp) / n()) %>% ungroup() %>%
    mutate(ingreso_personal = ing_trab + ing_jub_aps + ing_gpp) %>%
    group_by(id_hogar) %>% mutate(prop_hogar = ingreso_personal / sum(ingreso_personal))%>%
    ungroup() %>%
    mutate_at(c("prop_hogar", "ingreso_hogar", "ing_gpp", "income_person_week", "ingreso_personal"),
              ~replace_na(.,0))

  return(data)
}

na_completion <- function(data) {
  act_traslados_ds <- c("m12_1_1", "m12_1_2", "q18_1_1", "q18_1_2", "r12_1_1", "r12_1_2")
  act_traslados_fds <- c("m12_2_1", "m12_2_2", "q18_2_1", "q18_2_2", "r12_2_1", "r12_2_2")
  vars_todos <- c("t11_1_1", "t11_1_6", "t11_1_7", "t11_1_8","t12_1_3","t12_1_4")

  acts <- c('m11', 'q11','m21', 'o11', 'o13', 'o14', 'o21', 'o32', 'o33', 'o34', 'o42', 'o43', 'o44', 'o51', 'o61', 'n11', 'n12', 'n13', 'n14', 'n15', 'n16', 'n17', 'n18', 'n19', 'n110', 'n111', 'n112', 'n113', 'n21', 'n22', 'n23', 'n24', 'n25', 'n26', 'n27', 'n28', 'n29', 'n210', 'n211', 'n212', 'n31', 'n32', 'n33', 'n34', 'n35', 'n36', 'n37', 'n38', 'n39', 'n310', 'n311', 'n41', 'n42', 'n43', 'n44', 'n45', 'n46', 'n51', 'n52', 'n53', 'n54', 'p11', 'p12', 'p13', 'p14', 'p15', 'p21', 'p22', 'p23', 'r11', 'r21', 'r22', 's31', 's41', 'q17', 'o12', 'o22', 'o23', 'o31', 'o41', 'o52', 'o62', 'o63', 'o71', 'o72', 's11', 's21', 's22', 's23', 's32', 's51', 's52', 's53', 's54', 'q12', 'q13', 'q14', 'q15', 'q16')

  data<- data  %>%
    filter(tiempo == 1) %>%
    mutate_at(c("c14_1_1","m14_1_1","m14_2_1","k42_1_1","k15_1_1",
                "l15_1_1","l16_1_1","l17_1_1","l117_1_1", "l118_1_1","l119_1_1"),
              ~ifelse(is.na(.), 85, .)) %>%
    #filter(if_all(n_linea_p:te_ayuda_cercanos, ~ !.x  %in% 96)) %>%
    #filter(l110_1_1 != 1 & l122_1_1 != 1) %>% # en cama enfermos
    #filter(l111_1_1 != 1 & l123_1_1 != 1) %>%# pasaron cosas fuera de lo común
    #filter(m14_1_1 != 2 & m14_2_1 != 2) %>% # huelga
    #filter(k42_1_1 != 6) %>%# no buscó trabajo por estar embarazada
    #filter(!(l15_1_1 == 2 & l16_1_1 == 2 & l17_1_1 == 2 & (k42_1_1 == 2 | k42_1_1 == 3 | k42_1_1 == 5))) %>% # los de vacaciones
    #filter(!(l117_1_1 == 2 & l118_1_1 == 2 & l119_1_1 == 2 & (k42_1_1 == 2 | k42_1_1 == 3 | k42_1_1 == 5))) %>%
    #filter(k15_1_1 != 1) %>%
    mutate_at(c(paste0(acts, "_2_2"), paste0(acts, "_1_2")), ~replace_na(.,0)) %>%
    mutate_at(c(paste0(acts, "_2_1"), paste0(acts, "_1_1")), ~replace_na(.,2)) %>%
    mutate_at(c(paste0(acts, "_2_1"), paste0(acts, "_1_1")), ~abs(. -2)) %>%
    mutate_at(c(paste0(acts, "_1_2"), paste0(acts, "_2_2")), ~ifelse(. >= 25, 0, .)) %>%
    mutate_at(c(act_traslados_ds, act_traslados_fds), ~replace_na(.,0)) %>%
    mutate_at(c(act_traslados_ds, act_traslados_fds), ~ifelse(. >= 25, 0, .)) %>%
    mutate_at(c(110: ncol(data)), ~replace_na(.,0)) %>%
    filter(if_all(all_of(vars_todos), ~ !.x  %in% 85)) %>%
    mutate(
      t_total_ds = dplyr::select(
        ., all_of(c(paste0(acts, "_1_2"), act_traslados_ds))) %>% rowSums(na.rm = TRUE),
      t_total_fds = dplyr::select(
        ., all_of(c(paste0(acts, "_2_2"), act_traslados_fds))) %>% rowSums(na.rm = TRUE)) %>%
    mutate(sexo = abs(c13_1_1 - 1))
  return(data)
}

outlier_detection_Vallejo <- function(data)  {

  data["dias_normal"] <- TRUE
  data <- data %>%
    # group_by(años_escolaridad, k11_1_1, rango_edad,sexo) %>%
    group_by(quintil, trabaja, tramo_edad, sexo) %>%
    mutate(xi_ds = mean(t_total_ds) , sd_ds = sd(t_total_ds),
           xi_fds = mean(t_total_fds), sd_fds = sd(t_total_fds),) %>%
    mutate(gamma_ds = sd_ds/xi_ds, gamma_fds = sd_fds/xi_fds) %>%
    mutate(lower_limit_ds = xi_ds - sd_ds + gamma_ds/2,
           upper_limit_ds = xi_ds + sd_ds,
           lower_limit_fds = xi_fds - sd_fds + gamma_fds/2,
           upper_limit_fds = xi_fds + sd_fds) %>%
    ungroup() %>%
    mutate(dias_normal = case_when(
      dias_normal == FALSE ~ FALSE,
      t_total_ds  < lower_limit_ds  | t_total_ds  > upper_limit_ds  ~ FALSE,
      t_total_fds < lower_limit_fds | t_total_fds > upper_limit_fds ~ FALSE,
      TRUE ~ TRUE)) %>%
    filter(dias_normal == TRUE) %>%

  return(data)
}

new_variables_postfilter <- function(data) {
  ingresos     <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc",
                    "ing_gpp", "ing_personal", "ingreso_hogar", "income_person_week")

  data <- data %>%
    mutate(horas_trabajo = case_when(k31_1_3 > 0 ~ k31_1_3, T ~ 0),
           macrozona = case_when(region <= 5 | region == 15 ~ "norte",
                                 region ==13 ~ "metropolitana",
                                 region <= 8 & region > 5 ~ "centro",
                                 TRUE ~ "sur"),
           edad_años = c14_1_1,
           tramo_edad = case_when(edad_años <=24 ~ "12-24",
                                  edad_años <=44 ~ "25-44",
                                  edad_años <=65 ~ "45-65",
                                  T            ~ "66+"),
           estudia = abs(abs(d14_1_1-1)-1),
           n_menores  = case_when(n_menores  <= 3 ~ n_menores , T ~ 4),
           n_mayores = case_when(n_mayores <= 5 ~ n_mayores, T ~ 6),
           nivel_escolaridad = case_when( # todos los niveles son completos, excepto el inicial
             nivel_escolaridad == 1 ~ "ninguna",
             nivel_escolaridad == 2 ~ "primaria",
             nivel_escolaridad == 3 ~ "secundaria",
             nivel_escolaridad == 4 ~ "técnica",
             T                      ~ "universitaria"),
           parentesco = c12_1_1,
           ing_personal = ing_trab + ing_jub_aps + ing_gpp) %>%
    mutate_at(ingresos, ~. / 1000 / 4) %>% #/654
    mutate(ing_personal = round(ing_personal, 2))

  data <- data %>% mutate(
        cae = cae,
        cise = cise_5,
        ciuo_agrupada = case_when(
          ciuo88_1d %in% c(1,2)   ~ 1,
          ciuo88_1d == 3          ~ 2,
          ciuo88_1d %in% c(4,5,9) ~ 3,
          ciuo88_1d == 6          ~ 4,
          ciuo88_1d == 7          ~ 5,
          ciuo88_1d == 8          ~ 6,
          ciuo88_1d %in% c(0,99)  ~ 7,
        )
    )

  cae_id <- c("Menor de 15 años"=1, "Ocupada(o)"=2, "Desocupada(o)"=3, "Inactiva(o)"=4, "Sin clasificar"=96)
  data$cae <- names(cae_id)[match(data$cae, cae_id)]

  return(data)
}

get_25activities <- function(data) {
  # trabajo (libre)
  act_t_to          <- c("m11")
  act_t_to_js       <- c("m21")

  # trabajo de cuidados
  act_t_tcnr_ce <- c("n13", "n23",
                     "n14", "n24", "n32",
                     "n15", "n25", "n33",
                     "n11", "n21", "n31",
                     "n111", "n112", "n211", "n212", "n310", "n311",
                     "n12", "n22")

  act_t_tcnr_re <- c(
    "n19", "n29", "n37", "n44",
    "n110", "n210", "n38", "n45",
    "n39"
  )

  act_t_tcnr_oac <- c(
    "n16", "n17", "n18", "n113",
    "n26", "n27", "n28",
    "n34", "n35", "n36",
    "n41", "n42", "n43", "n46",
    "n51", "n52", "n53", "n54"
  )

  # trabajo doméstico (podría dividir en las relacionadas al hogar y relacionadas a los miembros del hogar)
  act_t_tdnr_psc <- c("o11", "o12", "o13", "o14")
  act_t_tdnr_lv  <- c("o21", "o22", "o23")
  act_t_tdnr_lrc <- c("o31", "o32", "o33", "o34")
  act_t_tdnr_mrm <- c("o41", "o42", "o43", "o44")
  act_t_tdnr_admnhog <- c("o51", "o52") # puse aqui buscar trabajo
  act_t_tdnr_comphog <- c("o61", "o62", "o63")
  act_t_tdnr_cmp <- c("o71", "o72")

  # trabajo no remunerado
  act_t_tvaoh_tv <- c("p21", "p22", "p23")
  act_t_tvaoh_oh <- c("p11", "p12", "p13", "p14", "p15")
  # aprendizaje
  act_t_ed <- c("r11", "r21", "r22")

  # ocio
  act_t_vsyo_csar <- act_ocio_social <- c("s11", "s21", "s22", "s23") # social y eventos pueden ser mezclables
  act_t_vsyo_aa <- c("s31", "s32", "s41")
  act_t_mcm <- c("s51", "s52", "s53", "s54")

  # cuidados personales
  act_t_cpaf_cp <- c("q12", "q17")
  act_t_cpag_comer <- c("q13", "q14", "q15", "q16")
  act_t_cpag_dormir <- c("q11")

  # traslados (committed)

  act_t_tt_ds <- c("m12_1_1", "m12_1_2", "q18_1_1", "q18_1_2", "r12_1_1", "r12_1_2")
  act_t_tt_fds <- c("m12_2_1", "m12_2_2", "q18_2_1", "q18_2_2", "r12_2_1", "r12_2_2")

  data <- data %>%
    mutate(
      t_to_ds = dplyr::select(., paste0(act_t_to, "_1_2")) %>% rowSums(na.rm = T),
      t_to_js_ds = dplyr::select(., paste0(act_t_to_js, "_1_2")) %>% rowSums(na.rm = T),
      t_tcnr_ce_ds = dplyr::select(., paste0(act_t_tcnr_ce, "_1_2")) %>% rowSums(na.rm = T),
      t_tcnr_re_ds = dplyr::select(., paste0(act_t_tcnr_re, "_1_2")) %>% rowSums(na.rm = T),
      t_tcnr_oac_ds = dplyr::select(., paste0(act_t_tcnr_oac, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_psc_ds = dplyr::select(., paste0(act_t_tdnr_psc, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_lv_ds = dplyr::select(., paste0(act_t_tdnr_lv, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_lrc_ds = dplyr::select(., paste0(act_t_tdnr_lrc, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_mrm_ds = dplyr::select(., paste0(act_t_tdnr_mrm, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_admnhog_ds = dplyr::select(., paste0(act_t_tdnr_admnhog, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_comphog_ds = dplyr::select(., paste0(act_t_tdnr_comphog, "_1_2")) %>% rowSums(na.rm = T),
      t_tdnr_cmp_ds = dplyr::select(., paste0(act_t_tdnr_cmp, "_1_2")) %>% rowSums(na.rm = T),
      t_tvaoh_tv_ds = dplyr::select(., paste0(act_t_tvaoh_tv, "_1_2")) %>% rowSums(na.rm = T),
      t_tvaoh_oh_ds = dplyr::select(., paste0(act_t_tvaoh_oh, "_1_2")) %>% rowSums(na.rm = T),
      t_ed_ds = dplyr::select(., paste0(act_t_ed, "_1_2")) %>% rowSums(na.rm = T),
      t_vsyo_csar_ds = dplyr::select(., paste0(act_t_vsyo_csar, "_1_2")) %>% rowSums(na.rm = T),
      t_vsyo_aa_ds = dplyr::select(., paste0(act_t_vsyo_aa, "_1_2")) %>% rowSums(na.rm = T),
      t_mcm_ds = dplyr::select(., paste0(act_t_mcm, "_1_2")) %>% rowSums(na.rm = T),
      t_cpaf_cp_ds = dplyr::select(., paste0(act_t_cpaf_cp, "_1_2")) %>% rowSums(na.rm = T),
      t_cpag_comer_ds = dplyr::select(., paste0(act_t_cpag_comer, "_1_2")) %>% rowSums(na.rm = T),
      t_cpag_dormir_ds = dplyr::select(., paste0(act_t_cpag_dormir, "_1_2")) %>% rowSums(na.rm = T),
      t_tt1_ds = dplyr::select(., all_of(act_t_tt_ds)) %>% rowSums(na.rm = TRUE)) %>%
    mutate(
      t_to_fds = dplyr::select(., paste0(act_t_to, "_2_2")) %>% rowSums(na.rm = T),
      t_to_js_fds = dplyr::select(., paste0(act_t_to_js, "_2_2")) %>% rowSums(na.rm = T),
      t_tcnr_ce_fds = dplyr::select(., paste0(act_t_tcnr_ce, "_2_2")) %>% rowSums(na.rm = T),
      t_tcnr_re_fds = dplyr::select(., paste0(act_t_tcnr_re, "_2_2")) %>% rowSums(na.rm = T),
      t_tcnr_oac_fds = dplyr::select(., paste0(act_t_tcnr_oac, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_psc_fds = dplyr::select(., paste0(act_t_tdnr_psc, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_lv_fds = dplyr::select(., paste0(act_t_tdnr_lv, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_lrc_fds = dplyr::select(., paste0(act_t_tdnr_lrc, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_mrm_fds = dplyr::select(., paste0(act_t_tdnr_mrm, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_admnhog_fds = dplyr::select(., paste0(act_t_tdnr_admnhog, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_comphog_fds = dplyr::select(., paste0(act_t_tdnr_comphog, "_2_2")) %>% rowSums(na.rm = T),
      t_tdnr_cmp_fds = dplyr::select(., paste0(act_t_tdnr_cmp, "_2_2")) %>% rowSums(na.rm = T),
      t_tvaoh_tv_fds = dplyr::select(., paste0(act_t_tvaoh_tv, "_2_2")) %>% rowSums(na.rm = T),
      t_tvaoh_oh_fds = dplyr::select(., paste0(act_t_tvaoh_oh, "_2_2")) %>% rowSums(na.rm = T),
      t_ed_fds = dplyr::select(., paste0(act_t_ed, "_2_2")) %>% rowSums(na.rm = T),
      t_vsyo_csar_fds = dplyr::select(., paste0(act_t_vsyo_csar, "_2_2")) %>% rowSums(na.rm = T),
      t_vsyo_aa_fds = dplyr::select(., paste0(act_t_vsyo_aa, "_2_2")) %>% rowSums(na.rm = T),
      t_mcm_fds = dplyr::select(., paste0(act_t_mcm, "_2_2")) %>% rowSums(na.rm = T),
      t_cpaf_cp_fds = dplyr::select(., paste0(act_t_cpaf_cp, "_2_2")) %>% rowSums(na.rm = T),
      t_cpag_comer_fds = dplyr::select(., paste0(act_t_cpag_comer, "_2_2")) %>% rowSums(na.rm = T),
      t_cpag_dormir_fds = dplyr::select(., paste0(act_t_cpag_dormir, "_2_2")) %>% rowSums(na.rm = T),
      t_tt1_fds = dplyr::select(., all_of(act_t_tt_fds)) %>% rowSums(na.rm = TRUE)) %>%
  mutate(
    t_total_ds = dplyr::select(., paste0(acts_corregidas, "_ds")) %>% rowSums(na.rm = TRUE),
    t_total_fds = dplyr::select(., paste0(acts_corregidas, "_fds")) %>% rowSums(na.rm = TRUE))
}

data_to168hours <- function(data) {
  act_confiable    <- c("t_to", "t_cpag_dormir")
  act_no_confiable <- acts_corregidas[!acts_corregidas %in% act_confiable]

  data <- data %>%
    #mutate_at(c(act_confiable, act_no_confiable), ~ifelse(. <= 1/4,0 ,.)) %>%
    mutate(t_confiable =  dplyr::select(.,all_of(act_confiable)) %>% rowSums(na.rm = TRUE),
           t_no_confiable =  dplyr::select(.,all_of(act_no_confiable)) %>% rowSums(na.rm = TRUE)) %>%
    mutate(t_total = t_confiable + t_no_confiable)  %>%
    filter(t_confiable <= 168) %>%
    mutate(mult_no_confiable = (168- t_confiable)/(t_total - t_confiable)) %>%
    mutate_at(act_no_confiable, ~ .*mult_no_confiable) %>%
    mutate(
      tt_otal = dplyr::select(., all_of(c(act_confiable, act_no_confiable))) %>%
        rowSums(na.rm = TRUE))

  return(data)
}


impute_weekend <- function(data, twin_matrix) {

  acts <- list(
    "6" = paste0(acts_corregidas, "_sab"),
    "7" = paste0(acts_corregidas, "_dom"))

  finsemana <- paste0(acts_corregidas, "_fds")
  dias_semana <- paste0(acts_corregidas, "_ds")

  semana_completa <- acts_corregidas
  for (i in 6:7) {
    mask <- data$dia_fin_semana == i
    finde <- unlist(acts[as.character(i)])

    data[mask, finde]  <- data[mask, finsemana]
    suma <- rowSums(twin_matrix[!mask, mask])
    data[!mask, finde] <- as.matrix(twin_matrix[!mask, mask] ) %*% as.matrix(data[mask, finsemana])
    data[!mask, finde] <-  sweep(data[!mask, finde],1,suma,"/")
  }

  sabados <- unlist(acts["6"])
  domingos <- unlist(acts["7"])
  data_post <- data %>%
    #mutate_at(sabados , ~ifelse(. <= 1/12,0 ,.)) %>% # todo tiempo al que se le dedique menos de 5 minutos
    #mutate_at(domingos , ~ifelse(. <= 1/12,0 ,.)) %>%  # todo tiempo al que se le dedique menos de 5 minutos
    mutate(sum_sabados  = dplyr::select(., all_of(sabados)) %>% rowSums(na.rm = TRUE),
           sum_domingos = dplyr::select(., all_of(domingos)) %>% rowSums(na.rm = TRUE))%>%
    mutate_at(sabados , ~ . * 24 / sum_sabados)  %>%
    mutate_at(domingos, ~ . * 24 / sum_domingos) %>%
    mutate(sum_sabados = dplyr::select(., all_of(sabados)) %>% rowSums(na.rm = TRUE),
           sum_domingos = dplyr::select(., all_of(domingos)) %>% rowSums(na.rm = TRUE)) %>%
    mutate_at(dias_semana, ~case_when((k31_1_2 < 5) & (k31_1_2 > 0) ~ . *k31_1_2, T ~ .* 5 ))

  temp <- data_post[,"t_to_ds"]
  data_post[,"t_to_ds"] <- 0
  data_post[,dias_semana] <- sweep(data_post[,dias_semana] , 1, rowSums(data_post[,dias_semana]), "/")
  data_post[,dias_semana] <- sweep(data_post[,dias_semana] , 1, as.numeric(unlist(24*5 - temp)), "*")
  data_post[,"t_to_ds"] <- temp

  data_post[,semana_completa] <- data_post[,dias_semana] + data_post[,sabados] + data_post[,domingos]
  data_post[,"t_total"] <-rowSums(data_post[,semana_completa])

  return(data_post)
}

diagnostico_trabajo <- function(data, plots, intercuartil) {
  diagnostico <- data %>%
    mutate(diferencia = t_to - k31_1_3)

  cat("Número inicial de filas",nrow(diagnostico), end = "\n")

  if (plots) {
    ggplot(diagnostico, aes(x = k31_1_1)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = k31_1_2)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = k31_1_3)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = t_to)) + geom_histogram(binwidth = 1)
    ggplot(diagnostico, aes(x = diferencia)) + geom_histogram(binwidth = 1)
  }

  if (intercuartil) {
    cuantiles <- quantile(diagnostico[(diagnostico$t_to > 0) | (diagnostico$k31_1_3 > 0),]$diferencia, c(0.25, 0.75))
    intercuantil <- cuantiles[2] - cuantiles[1]
    diagnostico <- diagnostico %>% filter(diferencia < cuantiles[2] + intercuantil, diferencia > cuantiles[1] - intercuantil)
    cat("Número final de filas",nrow(diagnostico))
    return(diagnostico)
  } else {
    diagnostico <- diagnostico %>% filter(diferencia < 10, diferencia > -10)
    cat("Número final de filas",nrow(diagnostico))
    return(diagnostico)
  }
}

adjust_working_hours <- function(data, normalize = TRUE) {
  act_ajustar <- acts_corregidas[!acts_corregidas %in% c("t_to")]
  data <- data %>% mutate(t_to = horas_trabajo)

  # if (normalize) {
  #   data <- data %>%
  #     mutate(t_ajustable = dplyr::select(.,act_ajustar) %>% rowSums(na.rm = TRUE)) %>%
  #     mutate(mult_ajuste= (168- tt)/(t_ajustable)) %>%
  #     mutate_at(act_ajustar, ~ .*mult_ajuste) %>%
  #     mutate(ttotal_ajuste = dplyr::select(., c("tt", act_ajustar))  %>% rowSums(na.rm = TRUE))
  # }

  return(data)
}

agregar_actividades <- function(data_post) {

  data_post <- data_post %>%
    mutate(
      t_paid_work = t_to, # free
      t_job_search = t_to_js, # free
      t_domestic_work = dplyr::select(., c("t_tdnr_psc", "t_tdnr_lv", "t_tdnr_lrc", "t_tdnr_mrm",
                                           "t_tdnr_admnhog", "t_tdnr_comphog", "t_tdnr_cmp",)) %>%
        rowSums(na.rm = TRUE), # commited/free
      t_care_work = dplyr::select(., c("t_tcnr_ce", "t_tcnr_re", "t_tcnr_oac",)) %>%
        rowSums(na.rm = TRUE),  # commited/free
      t_unpaid_voluntary = t_tvaoh_tv + t_tvaoh_oh, # free
      t_education = t_ed, # committed / free
      t_leisure = t_vsyo_csar + t_vsyo_aa + t_mcm, # free
      t_personal_care = t_cpaf_cp, #committed/free
      t_meals         = t_cpag_comer, #committed/free
      t_sleep = t_cpag_dormir, #committed/free
      t_commute1 = t_tt1)  # committed

  data_post <- data_post %>%
    mutate(
      es_trabajador = case_when(trabaja == 1 & cae == "Ocupada(o)" & t_to > 0 ~ 1, T ~ 0),
      es_familia = case_when(trabaja == 1 & cae == "Ocupada(o)" & t_to > 0 & vive_pareja == 1 & tiene_hijos ==1  ~ 1, T ~0))


  data22 <- data_post %>% dplyr::select(
    all_of(identificadores),
    all_of(tipo_muestra),
    dia_semana, dia_fin_semana,
    all_of(composicion_hogar),
    all_of(proveedores_externos),
    all_of(sociodemograficas),
    all_of(laborales),
    t11_1_1:t15_1_1,
    all_of(ingresos),
    all_of(acts_corregidas)) %>%
    mutate(t_total = dplyr::select(., all_of(acts_corregidas)) %>% rowSums(na.rm = TRUE))

  data22[,acts_corregidas]  = round(data22[,acts_corregidas],2)
  data22[,"temp"] = rowSums(data22[,acts_corregidas])
  data22[,"t_cpag_dormir"]       = data22[,"t_cpag_dormir"] - (data22[,"temp"] - 168)
  data22[,"temp"] = rowSums(data22[,acts_corregidas])
  data22 <- data22 %>% dplyr::select(-c("temp"))

  data11 <- data_post %>%
    dplyr::select(
      all_of(identificadores),
      all_of(tipo_muestra),
      dia_semana, dia_fin_semana,
      all_of(composicion_hogar),
      all_of(proveedores_externos),
      all_of(sociodemograficas),
      all_of(laborales),
      t11_1_1:t15_1_1,
      all_of(ingresos),
      all_of(t_agregados)) %>%
  mutate(t_total = dplyr::select(., all_of(t_agregados)) %>% rowSums(na.rm = TRUE))

  data11[,t_agregados]  = round(data11[,t_agregados],2)
  data11[,"temp"] = rowSums(data11[,t_agregados])
  data11[,"t_sleep"]  = data11[,"t_sleep"] - (data11[,"temp"] - 168)
  data11[,"temp"] = rowSums(data11[,t_agregados])

  return(list(data22 = data22, data11= data11))
}
###### MANTENER POR AHI ########
# grupos <- data %>%
#   group_by(dia_semana, años_escolaridad, trabaja, rango_edad,sexo) %>%
#   summarise(cuantos = n())
#######

imputacion_gastos <- function(data) {
  library("minpack.lm")
  data_hogar <- data %>%
    dplyr::select(id_hogar, n_personas, n_menores6, n_menores12, n_menores18, n_menores, n_trabajadores, n_personas,
                  edad_promedio, quintil, macrozona, ingreso_hogar, income_person_week, n_profesionales) %>%
    distinct(id_hogar, .keep_all = T) %>%
    mutate(n_personas_cut = case_when(n_personas >= 7 ~ 7, T ~ n_personas),
           n_menores6_cut = case_when(n_menores6 >= 3 ~ 3, T ~ n_menores12),
           n_menores12_cut = case_when(n_menores12 >= 2 ~ 3, T ~ n_menores12),
           n_menores18_cut = case_when(n_menores18 >= 2 ~ 3, T ~ n_menores18),
           n_trabajadores_cut = case_when(n_trabajadores >= 3 ~ 3, T ~ n_trabajadores),
           n_profesionales_cut = case_when(n_profesionales >= 2 ~ 3, T ~ n_profesionales))
  #hist(gastos$savings/gastos$ingresos, breaks =100)
  gastos  <<- read.csv("data/gastos.csv") %>% filter(ingreso_hogar > 0, savings/ingreso_hogar > -1)

  model   <<- apollo_loadModel("output/FMNL-epf-viii")
  lin_reg <<- lm(savings ~
                     ingreso_hogar +
                     (quintil==2) + (quintil==3) + (quintil==4) + (quintil==5) +
                     n_menores6_cut + n_menores12_cut + n_menores18_cut+ n_personas_cut +
                     n_trabajadores_cut  + n_profesionales_cut +
                     edad_promedio +
                     (macrozona == "metropolitana") ,
                 data = gastos)
  summary(lin_reg)
  codigos <<- c(g.alimentos = "01", g.vestimenta = "03",g.cuentas = "04", g.hogar = "05", g.salud = "06",
             g.transporte = "07", g.comunicaciones = "08", g.recreacion = "09", g.educacion = "10", g.restaurantes = "11")
  alts <<- c("alimentos", "vestimenta", "cuentas", "hogar", "salud", "transporte",
             "comunicaciones", "recreacion", "educacion", "restaurantes")

  mask <- data_hogar[, "ingreso_hogar"] >= 0
  data_hogar[mask,"savings"] <- predict(lin_reg, data_hogar[mask,])

  sum(mask)
  sum((data_hogar[mask,"savings"] < 0), na.rm = T )
  sum(( data_hogar[mask,"savings"] > data_hogar[mask,"ingreso_hogar"]), na.rm = T)
  data_hogar %>%
    distinct(id_hogar, .keep_all = T) %>%
    group_by(quintil) %>%
    summarise(
      mean_savings = mean(savings, na.rm = TRUE),
      median_savings = median(savings, na.rm = TRUE),
      median_income = median(ingreso_hogar),
      min_hogar = min(ingreso_hogar))
  gastos %>%
    group_by(quintil) %>%
    summarise(
      mean_savings = mean(savings, na.rm = TRUE),
      median_savings = median(savings, na.rm = TRUE),
      median_income = median(ingreso_hogar),
      min_hogar = min(ingreso_hogar))

  data_hogar <-  data_hogar %>%
    mutate(savings = case_when(
      #savings < 0 ~ 0,
      savings > ingreso_hogar ~ ingreso_hogar, T ~ savings ))
  data_hogar$id_hogar <- as.numeric(data_hogar$id_hogar)

  apollo_initialise()
  apollo_control <<- list(
    modelName       = "FMNL"   ,
    modelDescr      = "Fractional MNL model on time use data",
    indivID         = "id_hogar",
    outputDirectory = "output")
  apollo_probabilities <<- model$apollo_probabilities
  database <<- data_hogar
  database[,names(codigos)] <<- 0
  apollo_beta <<- model$apollo_beta
  apollo_fixed <<- model$apollo_fixed
  apollo_inputs <<- apollo_validateInputs()
  predictions_base <<- apollo_prediction(model,apollo_probabilities,apollo_inputs)
  data_hogar[,alts] = predictions_base[,alts]
  data_hogar[,alts] = sweep(data_hogar[,alts], 1, unlist(data_hogar[,"ingreso_hogar"] - data_hogar[,"savings"]), "*")

  data <- merge(data, data_hogar[,c("id_hogar", "savings", alts)], by = "id_hogar", all.x = TRUE)
  data[,c("savings", alts)] = sweep(data[,c("savings", alts)], 1, unlist(data[,"prop_hogar"]), "*")
  data[,"total_expenses"] = data[,"ing_personal"] - data["savings"]

  data[,alts] = round(data[,alts], 2)

  #ingresos     <- c("ing_ocuppal", "ing_trab", "ing_jub_aps", "ing_g", "ing_mon", "ing_mon_pc", "ing_gpp", "ing_personal", "income_person_week")
  # data[, ingresos] = data[, ingresos] * 1000
  # data[, alts] = data[, alts] * 1000
  # data[, "savings"] = data[, "savings"] * 1000
  # data[, "w"] = data[, "w"] * 1000

  return(data)
}
