#' enut-i-raw
#'
#' Processed dataset from the first National Time-Use Survey (ENUT I), applied by the
#' Instituto Nacional de Estadísticas de Chile in 2015. Contains 25 detailed time-use
#' activity categories and household expenditures imputed from the VIII Encuesta de
#' Presupuestos Familiares (EPF). Used as the primary input for structural time-use
#' models via \code{get_data()}.
#'
#' All income and expenditure variables are expressed in weekly thousands of Chilean pesos.
#' Time variables are expressed in weekly hours, normalized to sum to 168.
#'
#' \describe{
#'   \item{id_persona}{Individual identifier}
#'   \item{id_hogar}{Household identifier}
#'
#'   \item{es_trabajador}{1 if employed, CAE = "Ocupada(o)", and positive paid work time}
#'   \item{es_familia}{1 if es_trabajador, lives with partner, and has children in household}
#'
#'   \item{dia_semana}{Weekday of diary (1 = Monday to 5 = Friday)}
#'   \item{dia_fin_semana}{Weekend day of diary (6 = Saturday, 7 = Sunday)}
#'
#'   \item{parentesco}{Relationship to household head}
#'   \item{n_menores_0_5}{Number of household members under age 6 (ages 0-5)}
#'   \item{n_menores_6_11}{Number of household members aged 6-11}
#'   \item{n_menores_0_14}{Number of household members under age 15 (ages 0-14)}
#'   \item{n_menores_12_17}{Number of household members aged 12-17}
#'   \item{n_menores}{Number of household members under 18}
#'   \item{n_mayores}{Number of adult household members (18+)}
#'   \item{n_tiempo}{Number of household members who reported time use}
#'   \item{n_trabajadores}{Number of employed workers in household}
#'   \item{n_profesionales}{Number of household members with completed university or higher}
#'   \item{n_tercera_edad}{Number of household members aged 60+}
#'   \item{hay_tercera_edad}{1 if household contains at least one elderly member aged 60+ who is not the respondent}
#'   \item{n_personas}{Total household size}
#'   \item{edad_promedio}{Mean age of household members}
#'   \item{tiene_hijos}{1 if respondent has children living in the household}
#'   \item{en_pareja}{1 if respondent is in a couple relationship (married or cohabiting)}
#'   \item{vive_pareja}{1 if respondent lives with their partner}
#'
#'   \item{servicio_domestico}{1 if household receives paid domestic service}
#'   \item{ayuda_cercanos}{1 if household receives unpaid help from relatives or neighbours}
#'   \item{fuentes_externas}{1 if household receives any external care support (servicio_domestico or ayuda_cercanos)}
#'
#'   \item{sexo}{Female indicator (1 = female, 0 = male; recoded from raw variable)}
#'   \item{edad_anios}{Age in years}
#'   \item{tramo_edad}{Age bracket: "12-24", "25-44", "45-65", or "66+"}
#'   \item{nivel_escolaridad}{Highest completed education level: 1 = ninguna, 2 = básica,
#'     3 = media, 4 = técnica, 5 = universitaria o más}
#'   \item{estudia}{1 if currently enrolled in education}
#'   \item{trabaja}{1 if currently employed (from k11_1_1)}
#'   \item{horas_trabajo_contratadas}{Horas semanales contratadas segun contrato de trabajo (from k31_1_3)}
#'   \item{horas_trabajo_habituales}{Horas habituales de trabajo en la ocupacion principal (from k31_1_1);
#'     compare with \code{t_to} (diary-measured) and \code{horas_trabajo_contratadas} (contracted)}
#'   \item{dias_trabajo_semana}{Dias trabajados por semana en la ocupacion principal (from k31_1_2);
#'     used internally to scale weekday diary hours during weekend imputation}
#'   \item{quintil}{Household income quintile (1-5)}
#'   \item{macrozona}{Geographic macro-zone: "norte", "metropolitana", "centro", or "sur"}
#'   \item{region}{Region code (numeric)}
#'   \item{prop_ing_hogar}{Respondent's share of total personal income in the household; used to allocate household-level expenditures to individuals}
#'
#'   \item{cae}{Economic activity category: "Ocupada(o)", "Desocupada(o)", "Inactiva(o)", "Menor de 15 años", or "Sin clasificar"}
#'   \item{cise}{Employment status per CISE classification}
#'   \item{ciuo_agrupada}{Occupation group per CIUO-08 grouped classification}
#'
#'   \item{ing_ocuppal}{Income from main occupation (weekly, thousands CLP)}
#'   \item{ing_trab}{Total labor income (weekly, thousands CLP)}
#'   \item{ing_jub_aps}{Pension and AFP income (weekly, thousands CLP)}
#'   \item{ing_g}{Imputed group income component from household total (weekly, thousands CLP)}
#'   \item{ing_mon}{Monetary transfers and other non-labor income (weekly, thousands CLP)}
#'   \item{ing_mon_pc}{Per capita monetary income (weekly, thousands CLP)}
#'   \item{ing_gpp}{Individual share of group income (weekly, thousands CLP)}
#'   \item{ing_personal}{Personal income: ing_trab + ing_jub_aps + ing_gpp (weekly, thousands CLP)}
#'   \item{ingreso_hogar}{Total household disposable income (weekly, thousands CLP)}
#'   \item{income_person_week}{Household income divided by number of members (weekly, thousands CLP)}
#'
#'   \item{t_to}{Trabajo remunerado (paid work), horas semanales}
#'   \item{t_to_js}{Busqueda de trabajo (job search), horas semanales}
#'   \item{t_tcnr_ce}{Trabajo de cuidado no remunerado - cuidados esenciales: cuidado fisico personal
#'     de miembros del hogar (aseo, alimentacion, vestido), horas semanales}
#'   \item{t_tcnr_re}{Trabajo de cuidado no remunerado - cuidados relativos a la ensenanza:
#'     acompanamiento al establecimiento educacional, apoyo en tareas y lectura, horas semanales}
#'   \item{t_tcnr_oac}{Trabajo de cuidado no remunerado - otros cuidados: acompanamiento medico,
#'     traslado al trabajo u otras actividades, otros cuidados no clasificados, horas semanales}
#'   \item{t_tdnr_psc}{Trabajo domestico no remunerado - preparacion y servicio de comida, horas semanales}
#'   \item{t_tdnr_lv}{Trabajo domestico no remunerado - limpieza de vivienda, horas semanales}
#'   \item{t_tdnr_lrc}{Trabajo domestico no remunerado - limpieza y reparacion de ropa, horas semanales}
#'   \item{t_tdnr_mrm}{Trabajo domestico no remunerado - mantenimiento y reparaciones menores del hogar,
#'     horas semanales}
#'   \item{t_tdnr_admnhog}{Trabajo domestico no remunerado - administracion del hogar, horas semanales}
#'   \item{t_tdnr_comphog}{Trabajo domestico no remunerado - compras del hogar, horas semanales}
#'   \item{t_tdnr_cmp}{Trabajo domestico no remunerado - cuidados de mascotas y plantas, horas semanales}
#'   \item{t_tvaoh_tv}{Trabajo voluntario y ayuda a otros hogares - voluntariado en la comunidad,
#'     horas semanales}
#'   \item{t_tvaoh_oh}{Trabajo voluntario y ayuda a otros hogares - ayuda directa a otros hogares,
#'     horas semanales}
#'   \item{t_cpaf_cp}{Cuidados personales - actividades fisiologicas (higiene, visitas medicas, ejercicio),
#'     excluye sueno y comidas, horas semanales}
#'   \item{t_cpag_comer}{Cuidados personales - comer y beber, horas semanales}
#'   \item{t_cpag_dormir}{Cuidados personales - dormir; ajustado para que la suma de actividades sea
#'     168 horas, horas semanales}
#'   \item{t_ed}{Educacion y formacion (asistencia a clases, estudio, capacitacion), horas semanales}
#'   \item{t_vsyo_csar}{Vida social y ocio - convivencia social y actividades recreativas (reuniones,
#'     fiestas, deportes como espectador), horas semanales}
#'   \item{t_vsyo_aa}{Vida social y ocio - artes y aficiones (artes plasticas, artesania, juegos),
#'     horas semanales}
#'   \item{t_mcm_leer}{Medios de comunicacion y masivos - lectura (libros, prensa, digital), horas semanales}
#'   \item{t_mcm_audio}{Medios de comunicacion y masivos - consumo de audio (radio, musica, podcasts),
#'     horas semanales}
#'   \item{t_mcm_video}{Medios de comunicacion y masivos - consumo de video y television, horas semanales}
#'   \item{t_mcm_computador}{Medios de comunicacion y masivos - uso recreativo de computador e internet,
#'     horas semanales}
#'   \item{t_tt1}{Traslados - traslados asociados a trabajo remunerado, educacion y salud, horas semanales}
#'
#'   \item{t_total}{Total weekly hours across all activities (should equal 168)}
#'   \item{w}{Hourly wage rate: ing_trab / t_to (thousands CLP per hour)}
#'
#'   \item{alimentos}{Imputed food expenditure (individual share, weekly thousands CLP)}
#'   \item{vestimenta}{Imputed clothing expenditure (individual share, weekly thousands CLP)}
#'   \item{cuentas}{Imputed utility and fixed household expenditure (individual share, weekly thousands CLP)}
#'   \item{hogar}{Imputed household goods and services expenditure (individual share, weekly thousands CLP)}
#'   \item{salud}{Imputed health expenditure (individual share, weekly thousands CLP)}
#'   \item{transporte}{Imputed transportation expenditure (individual share, weekly thousands CLP)}
#'   \item{comunicaciones}{Imputed communications expenditure (individual share, weekly thousands CLP)}
#'   \item{recreacion}{Imputed recreation and culture expenditure (individual share, weekly thousands CLP)}
#'   \item{educacion}{Imputed education expenditure (individual share, weekly thousands CLP)}
#'   \item{restaurantes}{Imputed restaurants and food away from home expenditure (individual share, weekly thousands CLP)}
#'   \item{savings}{Imputed household savings, allocated by income share (weekly thousands CLP)}
#'   \item{total_expenses}{Total imputed expenditure: ing_personal - savings (weekly thousands CLP)}
#' }
#'
#' @details
#' The dataset is produced by running \code{data_processing/data_processing.R} followed by
#' \code{imputacion_gastos()}. Time variables are normalized to a 168-hour week using a
#' two-step procedure: paid work (\code{t_to}) and sleep (\code{t_cpag_dormir}) are treated
#' as reliable anchors; all other activities are scaled proportionally. Weekend time use is
#' imputed via a twin-matching matrix when the respondent's diary day was a weekday.
#' Outliers are removed using Vallejo's method by groups of quintile, employment status,
#' age bracket, and sex. Expenditures are imputed from EPF VIII using a fractional MNL
#' model for budget shares and a linear regression for the savings rate, then allocated to
#' individuals by \code{prop_ing_hogar}.
#'
#' @source <https://www.ine.gob.cl/enut>
#' @source <https://www.ine.gob.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares>
#'
#' @docType data
#' @keywords datasets
#' @name enut_i_raw
#' @usage data(enut_i_raw)
#' @format A data frame with approximately 2,000-3,000 rows and 95 variables
NULL
