options(scipen = "999")
library(tidyverse)
library(feather)
library(readxl)
library(kableExtra)
library(susor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(rio)
library(tidyverse)
library(plotly)

source("tema_elegante_ggplot.R")

actualizar <- F

# Descargar los datos desde el servidor
if(actualizar == T){
  source("descarga_datos_suso.R")
}


# Cargar las tablas necesarias #####
ruta_datos <- "/home/ricardo/Documents/INE/2. servicios_compartidos/COVID/shiny_tablero_Covid/tablero_covid/C:/Users/klehm/OneDrive - Instituto Nacional de Estadisticas/covid"

# Archivos versión inicial del cuestionario
    llamados <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_2/llamados.tab"))
asignaciones <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_2/assignment__actions.tab"))
   principal <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_2/covid19_r3_v3.tab"))
      action <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_2/interview__actions.tab"))

# Archivos de la última versión del cuestionario
    llamados2 <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_3/llamados.tab"))
asignaciones2 <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_3/assignment__actions.tab"))
   principal2 <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_3/covid19_r3_v3.tab"))
      action2 <- read_tsv(paste0(ruta_datos,"/data/covid19_r3_v2_3/interview__actions.tab"))

muestra_seguimiento <- read_excel(paste0(ruta_datos,"/data/selecciones_muestrales/10062021_Muestra_Covid_3_4.xlsx"), sheet = "Log_Cov1_o_Cov2")
muestra_casen <- read_excel(paste0(ruta_datos,"/data/selecciones_muestrales/10062021_Muestra_Covid_3_4.xlsx"), sheet = "Complemento CASEN")
comunas <- read_excel(paste0(ruta_datos,"/data/Comunas.xlsx"))


# Cargar cuentas de usuarios, para pegarle los nombres 
primeros_usuarios <- read_excel(paste0(ruta_datos,"/administrativo/personal_covid_actualizado.xlsx"))
usuarios_nuevos <- read_tsv(paste0(ruta_datos,"/administrativo/cuentas_nuevo_personal_20210615.tab"))

# Unir todos los usuarios y sacar supervisores
usuarios <- primeros_usuarios %>% 
  bind_rows(usuarios_nuevos) %>%  
  filter(Role == "interviewer")

# Unir las dos versiones del cuestionario 
llamados <- llamados %>% 
  bind_rows(llamados2)

asignaciones <- asignaciones %>% 
  bind_rows(asignaciones2)

principal <- principal %>%
  mutate(version = 1) %>% 
  bind_rows(principal2 %>% mutate(version = 2,
                                  r02_2 = as.character(r02_2)))

actions <- action %>% 
  bind_rows(action2)

# Función para contar días de fin de semana
contar_dias_finde <- function(fecha_inicial, fecha_final) {
  secuencia_dias <- as_date(fecha_inicial:fecha_final)
  dia_semana <-  wday(secuencia_dias, label = TRUE) %>%
    as.character()
  length(dia_semana[dia_semana == "sáb\\." | dia_semana == "dom\\."])
}


# Se selecciona una fila por cada entrevista 
llamados_hogar <- llamados %>% 
  group_by(interview__id, interview__key) %>% 
  slice(1) %>% 
  ungroup()

# Dejamos un hogar por cada vivienda, para reportar a nivel de vivienda 
# Usamos folio, ya que esta variable contiene la información de vivienda
principal_viv <- principal %>% 
  right_join(llamados_hogar, by = c("interview__id", "interview__key")) %>% # Dejar solo lo que tiene registros en la hoja de ruta
  group_by(folio) %>% 
  mutate(interview__status = max(interview__status)) %>% 
  slice(1) %>% 
  ungroup()


# Todas las que aparecen en la tabla principal
unidades_iniciadas <- nrow(principal_viv)

# Número de iniciadas por día 
iniciadas_dia <- actions %>%
  filter(action == 2) %>% 
  right_join(llamados_hogar %>% 
               select(interview__id, interview__key),
             by = c("interview__id", "interview__key")) %>% # Dejar solo lo que tiene registros en la hoja de ruta
  left_join(principal %>% 
              select(interview__id, interview__key, folio),
            by = c("interview__id", "interview__key")) %>% # Sacar el folio de acá
  group_by(folio) %>% 
  mutate(fecha = as_date(date)) %>% 
  arrange(fecha) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(!is.na(fecha)) %>% 
  group_by(fecha) %>% 
  summarise(iniciadas = n())

completadas_dia <- actions %>% 
  filter(action %in% c(3)) %>% 
  group_by(interview__id, interview__key) %>% 
  slice(1) %>% 
  ungroup() %>% 
  right_join(llamados_hogar %>% 
               select(interview__id, interview__key),
             by = c("interview__id", "interview__key")) %>% # Dejar solo lo que tiene registros en la hoja de ruta
  left_join(principal %>% 
              select(interview__id, interview__key, folio, interview__status),
            by = c("interview__id", "interview__key")) %>% # Sacar el folio de acá
  group_by(folio) %>% 
  mutate(fecha = as_date(date)) %>% 
  arrange(fecha) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(!is.na(fecha)) %>% 
  group_by(fecha) %>% 
  summarise(iniciadas = n())

# Han sido rechazadas por el encargado de grupo
rechazadas <- principal_viv %>% 
  filter(interview__status == 65) %>% 
  nrow()

# Gestión completada por el encuestador
completadas <- principal_viv %>% 
  filter(interview__status == 100 | interview__status == 120 | interview__status == 130 ) %>% 
  nrow()

# Aprobadas por el encargado de grupo
aprobadas_eg <- principal_viv %>% 
  filter(interview__status == 120 | interview__status == 130) %>% 
  nrow()

# Esto se saca directamente de la selección de marcos y muestras
tamanio_muestra <- nrow(muestra_casen) + nrow(muestra_seguimiento)

# Dejar solo las qque fueron reasginadas a algún encuestador
unidades_asignadas <- asignaciones %>% 
  filter(action == 7 & responsible__role == 1) %>% # las que han sido reasignadas a un encuestador
  group_by(assignment__id) %>% 
  arrange(desc(as_date(date)) ) %>% 
  slice(1) %>%  # dejamos solo la última asignación, ya que pueden haber reasignaciones entre encuestadores
  ungroup() %>% 
  nrow()



########################
# Datos a nivel nacional
# ######################

datos_nacional <- data.frame(muestra = tamanio_muestra, 
                             asignadas = unidades_asignadas, 
                             por_asignacion = unidades_asignadas / tamanio_muestra * 100,
                             iniciadas = unidades_iniciadas,
                             por_iniciadas = unidades_iniciadas / tamanio_muestra * 100,
                             rechazadas_eg = rechazadas,
                             completadas = completadas, 
                             pendientes_encuestador = unidades_asignadas - completadas,
                             por_completadas = completadas / tamanio_muestra * 100,
                             aprobadas = aprobadas_eg,
                             pendientes_revision = completadas - aprobadas_eg,
                             por_aprobadas = aprobadas_eg / tamanio_muestra * 100)

# Dejar las tasas al final porque son la parte más relevante del cuadro
datos_nacional <- datos_nacional %>% 
  mutate(por_pendiente_enc = pendientes_encuestador / asignadas * 100,
         por_pendiente_eg = pendientes_revision / completadas * 100) %>% 
  relocate(muestra, asignadas, iniciadas, completadas, aprobadas, rechazadas_eg, pendientes_encuestador, pendientes_revision,
           por_asignacion, por_iniciadas,por_completadas,  por_aprobadas, por_pendiente_enc, por_pendiente_eg  ) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2))

# Tamaño muestra nivel regional
muestra_region <- muestra_seguimiento %>% 
  select(region = Region_16) %>% 
  bind_rows(muestra_casen %>% select(region)) %>% 
  group_by(region) %>% 
  summarise(muestra = n())

######################  
# DAtos nivel regional
# ####################
datos_region <- principal_viv %>% 
  mutate(rechazada = if_else(interview__status == 65, 1, 0, missing = 0),
         completada =  if_else(interview__status == 100 | interview__status == 120 | interview__status == 130, 1 , 0, missing = 0),
         aprobada = if_else(interview__status == 120 | interview__status == 130, 1, 0, missing = 0)) %>% 
  group_by(region) %>% 
  summarise(iniciadas = n(),
            rechazadas_eg = sum(rechazada),
            completadas = sum(completada),
            aprobadas = sum(aprobada)) %>% 
  ungroup() %>% 
  mutate(pendientes_revision =  completadas - aprobadas) %>%  
  full_join(muestra_region, by = "region")  %>% 
  mutate(por_iniciadas = iniciadas / muestra * 100,
         por_completadas = completadas / muestra * 100,
         por_aprobadas = aprobadas / muestra * 100) %>% 
  mutate_all( ~if_else(is.na(.), 0 , as.double(.)  )) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  mutate(region = as.numeric(region)) %>% 
  arrange(region) %>% 
  relocate(region, muestra, iniciadas, completadas, aprobadas, por_iniciadas, por_completadas,  por_aprobadas)  


# Unir muestra de seguimiento y refresco y pegar nombre de etiquetas
muestra_comuna <- muestra_seguimiento %>%
  select(region = Region_16, cut = CUT_16) %>%
  mutate(muestra = "seguimiento") %>%
  bind_rows(muestra_casen %>%
              select(region, cut) %>%
              mutate(muestra = "refresco")) %>%  # hay dos casos que no hacen match
  left_join(comunas, by = c("cut" = "comuna")) %>% 
  group_by(comuna_glosa, cut) %>% 
  summarise(muestra = n())

#######################
# Datos nivel comunal #
# #####################
datos_comuna <-  principal_viv %>% 
  mutate(rechazada = if_else(interview__status == 65, 1 , 0, missing = 0),
         completada =  if_else(interview__status == 100 | interview__status == 120 | interview__status == 130, 1 , 0, missing = 0),
         aprobada = if_else(interview__status == 120 | interview__status == 130, 1, 0, missing = 0)) %>% 
  group_by(comuna) %>% 
  summarise(iniciadas = n(),
            rechazadas_eg = sum(rechazada),
            completadas = sum(completada),
            aprobadas = sum(aprobada)) %>% 
  ungroup() %>% 
  mutate(pendientes_revision = completadas - aprobadas) %>% 
  full_join(muestra_comuna, by = c("comuna" = "cut"))  %>% # pegar datos de la muestra
  mutate(por_iniciadas = iniciadas / muestra * 100,
         por_completadas = completadas / muestra * 100,
         por_aprobadas = aprobadas / muestra * 100) %>% 
  mutate_at(vars(-comuna_glosa),  ~if_else(is.na(.), 0 , as.double(.)  )) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  select(-comuna) %>% 
  relocate(comuna_glosa, muestra, iniciadas, completadas, aprobadas, por_iniciadas, por_completadas,  por_aprobadas)  

# Plot 1 gestión a nivel nacional ####

# datos_nacional %>%
#  kbl(caption = "<center><strong>Indicadores de gestión - Nivel Nacional</strong></center>") %>%
#  kable_paper("hover", full_width = F)

t1_Ind_gest_Nacional <- DT::datatable(datos_nacional)

fig_1_ind_gest_nacional <- datos_nacional %>% 
  select(starts_with("por"), -por_pendiente_enc, -por_pendiente_eg) %>% 
  pivot_longer(cols = starts_with("por"), names_to = "indicador", values_to = "porcentaje") %>%
  mutate(indicador = factor(indicador, levels = c("por_asignacion", "por_iniciadas", "por_completadas", "por_aprobadas"))) %>% 
  ggplot(aes(x = indicador, y = porcentaje, fill = indicador)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Indicadores de gestión a nivel nacional") + theme_elegante()

fig_1_ind_gest_nacional <- ggplotly(fig_1_ind_gest_nacional) 


### Plot completadas dia ####

plot_completadas_dia <- completadas_dia %>% 
  rename(completadas = iniciadas) %>% 
  ggplot(aes(x = fecha, y = completadas, group = 1, color = "coral")) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y %b %d") +
  labs(title = "Encuestas completadas por día",
       caption = "Nota: corresponde a las unidades con status 100") +
   theme_elegante() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60),
        legend.position = "none") 

plot_completadas_dia <- ggplotly(plot_completadas_dia) %>% 
  layout(margin = list(b=130,t=100), annotations = 
           list(x = 1, y = -0.5, text = "Nota: corresponde a las unidades con status 100, 120 o 130", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15,
                          fontfacet="italic"))
  )


### Plot iniciada dia ####

plot_iniciadas_dia <- iniciadas_dia %>% 
  ggplot(aes(x = fecha, y = iniciadas, group = 1, color = "coral")) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y %b %d") +
  labs(title = "Encuestas iniciadas por día",
       caption = "Nota: corresponde a las unidades en las que ha sido presionado el botón iniciar") +
  theme_elegante() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60),
        legend.position = "none")  

plot_iniciadas_dia <- ggplotly(plot_iniciadas_dia) %>% 
  layout(margin = list(b=130,t=100), annotations = 
           list(x = 1, y = -0.5, text = "Nota: corresponde a las unidades en las que ha sido presionado el botón iniciar", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15,fontfacet="italic")))

# datos a nivel regional ####
datos_region %>%
  kbl(caption = "<center><strong>Indicadores de gestión - Nivel Regional</strong></center>") %>%
  kable_paper("hover", full_width = F)

### plot datos nivel regional ####

fig <- datos_region %>% 
  select(region, starts_with("por")) %>% 
  pivot_longer(cols = starts_with("por"), names_to = "indicador", values_to = "porcentaje") %>% 
  mutate(indicador = fct_relevel(indicador, "por_iniciadas", "por_completadas", "por_aprobadas")) %>% 
  ggplot(aes(x = region, y = porcentaje, fill = indicador)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_continuous(breaks = seq(1, 16, 1)) +
  labs(title = "Indicadores de gestión a nivel regional") + theme_elegante()

fig_2_ind_gest_regional <- ggplotly(fig) 

## tabla datos a nivel comunal ####
DT::datatable(datos_comuna, 
              caption =  htmltools::tags$caption(
                style = 'text-align: center; font-size: 12px',
                htmltools::em('Indicadores de gestión - Nivel Comunal'))
)


# Se construye una tabla que solo contiene los casos que han sido completados o aprobados por el EG
aapor <- llamados %>% 
  left_join(principal %>% select(interview__id, interview__key, interview__status, region, comuna, folio),
            by = c("interview__id", "interview__key") ) %>% 
  mutate(cod_ent = if_else(cod_ent < 0, cod_ent * -1, cod_ent )) %>% # encuestadores que olvidaron poner el cdf
  filter((interview__status == 100 | interview__status == 120 | interview__status == 130) & cod_ent > 0) %>%  # dejar solo las completadas y sacar los missing en código de entrevista 
  group_by(folio) %>% # dejar solo la situación más reciente del folio
  arrange(cod_ent) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    entrevistada = if_else(cod_ent == 1100 & (interview__status == 100 | interview__status == 120 | interview__status == 130), 1, 0, missing = 0),
    desconocida = if_else(cod_ent >= 4100, 1, 0, missing = 0),
    contacto = if_else(cod_ent %in% c(1100:2000, 2310:2360), 1, 0, missing = 0),
    rechazo = if_else(cod_ent %in% c(1200, 2000), 1, 0, missing = 0 )
  ) 



##################
# NIVEL NACIONAL #
##################

# La tasa de cooperación se calcula por separado debido a que el denominador es diferente
tasa_cooperacion <- aapor %>% 
  filter(cod_ent %in% c(1100, 1200, 2000, 2310:2360) ) %>% 
  count(entrevistada) %>% 
  mutate(tasa = n / sum(n) * 100) %>% 
  filter(entrevistada == 1) %>% 
  select(cooperacion_por = tasa)

# Calcular las tasas restantes y pegar la información de la tasa de cooperación. La tabla aapor solo tiene las completadas
tasas_nacional <- aapor %>%  
  summarise(completadas = n(),
            desconocida = sum(desconocida),
            contacto = sum(contacto),
            rechazo = sum(rechazo),
            entrevistada = sum(entrevistada)) %>% 
  mutate(no_lograda = completadas - entrevistada) %>% 
  bind_cols(muestra = tamanio_muestra) %>% # agregar tamaño muestral
  bind_cols(cooperacion_por =  tasa_cooperacion$cooperacion_por) %>% # agregar tasa de cooperación
  mutate_at(vars(desconocida, contacto, rechazo, entrevistada), list(por = ~. / completadas * 100)) %>% 
  mutate(tasa_respuesta = entrevistada / muestra * 100 ) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  rename(lograda = entrevistada)



##################
# NIVEL REGIONAL #
##################

tasa_cooperacion_region <- aapor %>% 
  filter(cod_ent %in% c(1100, 1200, 2000, 2310:2360) ) %>% 
  count(entrevistada, region) %>% 
  mutate(tasa = n / sum(n) * 100) %>% 
  filter(entrevistada == 1) %>% 
  select(cooperacion_por = tasa, region)

tasas_region <- aapor %>% 
  group_by(region) %>% 
  summarise(completadas = n(),
            desconocida = sum(desconocida),
            contacto = sum(contacto),
            rechazo = sum(rechazo),
            entrevistada = sum(entrevistada)) %>% 
  ungroup() %>% 
  left_join(muestra_region, by = "region") %>% # agregar tamaño de muestra
  left_join(tasa_cooperacion_region, by = "region") %>% # agregar tasa de cooperación
  mutate(no_lograda = completadas - entrevistada) %>% 
  mutate_at(vars(desconocida, contacto, rechazo, entrevistada), list(por = ~. / completadas * 100))  %>% 
  mutate(tasa_respuesta = entrevistada / muestra * 100 ) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  rename(lograda = entrevistada) 



##################
# NIVEL comunal #
##################
tasa_cooperacion_comuna <- aapor %>% 
  filter(cod_ent %in% c(1100, 1200, 2000, 2310:2360) ) %>% 
  count(entrevistada, comuna) %>% 
  mutate(tasa = n / sum(n) * 100) %>% 
  filter(entrevistada == 1) %>% 
  select(cooperacion_por = tasa, comuna)

tasas_comuna <- aapor %>% 
  group_by(comuna) %>% 
  summarise(completadas = n(),
            desconocida = sum(desconocida),
            contacto = sum(contacto),
            rechazo = sum(rechazo),
            entrevistada = sum(entrevistada)) %>% 
  ungroup() %>% 
  left_join(muestra_comuna, by = c("comuna" = "cut")  ) %>% # agregar tamaño de muestra
  mutate(no_lograda = completadas - entrevistada) %>% 
  mutate_at(vars(desconocida, contacto, rechazo, entrevistada), list(por = ~. / completadas * 100)) %>% 
  left_join(tasa_cooperacion_comuna, by = "comuna") %>% 
  mutate(tasa_respuesta = entrevistada / muestra * 100 ) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  select(-comuna) %>% 
  relocate(comuna_glosa) %>% 
  rename(lograda = entrevistada)

# Resultados operativos a nivel nacional ####
tasas_nacional %>%
  kbl(caption = "<center><strong>Indicadores de resultado operativo - Nivel Nacional</strong></center>") %>%
  kable_paper("hover", full_width = F)

# fig_3_resultado_op_nacional ####

fig <- tasas_nacional %>% 
  select(ends_with("_por"), tasa_respuesta) %>% 
  pivot_longer(cols = c(ends_with("por"), "tasa_respuesta"), names_to = "indicador", values_to = "porcentaje") %>% 
  ggplot(aes(x = indicador, y = porcentaje, fill = indicador)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Indicadores de resultado operativo - Nivel Nacional") +
  theme_elegante() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

fig_3_resultado_op_nacional <- ggplotly(fig)

tasas_acum <- aapor %>%  
  mutate(fecha = as_date(hora_llamado)) %>% 
  group_by(fecha) %>% 
  summarise(completadas = n(),
            desconocida = sum(desconocida),
            contacto = sum(contacto),
            rechazo = sum(rechazo),
            entrevistada = sum(entrevistada)) %>% 
  mutate(no_lograda = completadas - entrevistada) %>% 
  ungroup() %>% 
  bind_cols(muestra = tamanio_muestra) %>% # agregar tamaño muestral
  bind_cols(cooperacion_por =  tasa_cooperacion$cooperacion_por) %>% # agregar tasa de cooperación
  mutate_at(vars(desconocida, contacto, rechazo, entrevistada), list(por = ~. / completadas * 100)) %>% 
  mutate(tasa_logro = round(entrevistada / muestra * 100 , 2) ) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  rename(lograda = entrevistada,
         lograda_por = entrevistada_por,
         tasa_respuesta = tasa_logro) %>%
  mutate(muestra_objetivo = 5331,
         tasa_logro = lograda / muestra_objetivo * 100) %>% 
  relocate(muestra, muestra_objetivo) %>% 
  select(fecha, tasa_respuesta, tasa_logro) %>% 
  filter(!is.na(fecha)) %>% 
  mutate(respuesta_acum = cumsum(tasa_respuesta),
         logro_acum = cumsum(tasa_logro))

# fig_4_resultado_acum_nacional ####

tasas_acum <- tasas_acum %>% 
  pivot_longer(cols = c("respuesta_acum", "logro_acum"), names_to = "tasa", values_to = "valor") %>% 
  mutate(fecha = as.Date(fecha)) %>% 
  ggplot(aes(x = fecha, y = valor, group = tasa, color = tasa)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y %b %d") +
  theme_elegante() +
  theme(axis.text.x = element_text(angle = 60))

fig_4_resultado_acum_nacional <- ggplotly(tasas_acum)

# Resultados operativos a nivel regional
tasas_region %>%
  kbl(caption = "<center><strong>Indicadores de resultado operativo - Nivel regional</strong></center>") %>%
  kable_paper("hover", full_width = F)


DT::datatable(tasas_comuna, 
              caption =  htmltools::tags$caption(
                style = 'text-align: center; font-size: 12px',
                htmltools::em('Indicadores de resultado operativo - Nivel Comunal'))
)


################
# Crear tabla 3
################

# COnstruir una tabla con la primera asignación realizada a cada encuestador
primera_asigacion <- asignaciones %>% 
  filter(responsible__role == 1) %>% 
  group_by(responsible__name) %>% 
  arrange(as_date(date)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(date, responsible__name)

# Construir una tabla auxiliar que solo contiene el código más bajo 
llamados_hogar <- llamados %>%
  filter(cod_ent > 0) %>% # sacar los missings
  select(interview__key, interview__id, rutStr, cod_ent, nomenc, hora_llamado) %>%
  group_by(interview__id, interview__key) %>% 
  arrange(cod_ent) %>%  
  slice(1) %>% # código más bajo 
  ungroup()


# Se obtiene el estatus de SUSO de la tabla principal.
# El código de disposición final se obtiene de la tabla auxiliar creda más arriba
insumo_tabla6 <- asignaciones %>%
  filter((action == 7 ) & responsible__role == 1) %>% # las que han sido reasignadas a un encuestador
  mutate(time_date = make_datetime(year(date), month(date), day(date), hour(time), minute(time), second(time))) %>% 
  group_by(assignment__id) %>% 
  arrange(desc(time_date)) %>% 
  slice(1) %>% 
  group_by(responsible__name) %>%
  mutate(total_asignaciones = n()) %>%
  ungroup() %>%
  select(responsible__name, assignment__id, total_asignaciones ) %>%
  right_join(principal %>%  # se dejan todos los que están en la tabla principal
               group_by(folio, id_hogar) %>% 
               arrange(desc(interview__status)) %>% # se deja el estatus más alto, porque hay asignaciones repetidas
               slice(1) %>% 
               ungroup() %>% 
               select(assignment__id, interview__id, interview__key, interview__status, folio),
             by = "assignment__id") %>% # obtener el id de entrevista y el estatus en suso
  left_join(llamados_hogar, by = c("interview__id", "interview__key")) %>% # obtener datos de unidades gestionadas
  left_join(actions %>% 
              filter(action == 1 & responsible__role == 1) %>% 
              group_by(interview__id) %>% 
              arrange(desc(date)) %>%
              slice(1) %>% 
              ungroup() %>% 
              select(responsible__name, interview__id, interview__key ),
            by = c("interview__id", "interview__key")) %>%  # rescatar casos que están en principal, pero no en asignaciones
  mutate(responsible__name.x = if_else(is.na(responsible__name.x), responsible__name.y, responsible__name.x),
         responsible__name = responsible__name.x) %>% 
  group_by(responsible__name) %>% 
  mutate(total_asignaciones_aux = max(total_asignaciones, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(total_asignaciones = if_else(is.na(total_asignaciones),total_asignaciones_aux, total_asignaciones)) %>% 
  mutate(completada = if_else((interview__status == 100 | interview__status == 120 | interview__status == 130), 1, 0, missing = 0), 
         lograda = if_else(cod_ent == 1100 & (interview__status == 100 | interview__status == 120 | interview__status == 130), 1, 0, missing = 0),
         fecha = as_date(hora_llamado)) %>%
  group_by(responsible__name) %>%
  mutate(completadas_total = sum(completada),
         logradas_total = sum(lograda)) %>%
  group_by(fecha, responsible__name) %>%
  mutate(completadas_diarios = sum(completada),
         logradas_diarios = sum(lograda)) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(primera_asigacion, by = "responsible__name") %>% # pegar fecha de la primera asignación
  mutate(fecha_inicial = as_date(date),
         fecha_final = as_date(Sys.Date())) %>%
  mutate(dias_corridos = fecha_final - fecha_inicial + 1,
         n_finde = map2_int(.x = .data$fecha_inicial, .y = .data$fecha_final, ~contar_dias_finde(.x, .y)),
         dias_habiles = as.integer(dias_corridos - n_finde)   ) %>% 
  select(-responsible__name.x, -responsible__name.y, -total_asignaciones_aux) # descartar cosas que no sirven

# Hacer ajuste por días feriados durante el periodo de levantamiento

if (Sys.Date() > as_date("2021-06-21")) {
  insumo_tabla6 <- insumo_tabla6 %>% 
    mutate(dias_habiles =  dias_habiles - 1)
} 


if (Sys.Date() > as_date("2021-06-28")) {
  insumo_tabla6 <- insumo_tabla6 %>% 
    mutate(dias_habiles =  dias_habiles - 1)
} 

if (Sys.Date() > as_date("2021-07-16")) {
  insumo_tabla6 <- insumo_tabla6 %>% 
    mutate(dias_habiles =  dias_habiles - 1)
}


# Crear parte general de la tabla
tabla6_general <-  insumo_tabla6 %>%
  mutate(media_logradas = logradas_total / dias_habiles ) %>%
  select(responsible__name, rutStr, dias_habiles, total_asignaciones, completadas_total, logradas_total, media_logradas ) %>%
  group_by(responsible__name) %>%
  slice(1) %>% 
  ungroup() %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  left_join(usuarios %>% select(login, fullname), by = c("responsible__name" = "login")) %>% 
  relocate(fullname)



# Sacar la tabla a nivel de días
tabla6_por_dia <- insumo_tabla6 %>%
  select(responsible__name, logradas_diarios, fecha) %>%
  pivot_wider(names_from = "fecha" , values_from = "logradas_diarios", names_prefix = "fecha_") %>% 
  mutate_at(vars(starts_with("fecha_")), ~if_else(is.na(.), 0, .) ) %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  select(-fecha_NA)

# Unir ambas partes de la tablaa
tabla6 <- tabla6_general %>%
  left_join(tabla6_por_dia, by = "responsible__name")




fig <- insumo_tabla6 %>%
  select(responsible__name, logradas_diarios, fecha) %>%
  pivot_wider(names_from = "fecha" , values_from = "logradas_diarios", names_prefix = "fecha_") %>% 
  select(-fecha_NA) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), 0, .)) %>% 
  pivot_longer(cols = starts_with("fecha"), names_to = "fecha", values_to = "logradas", names_prefix = "fecha_") %>% 
  mutate(fecha = as_date(fecha)) %>% 
  ggplot(aes(x = fecha, y = logradas, group = responsible__name, color = responsible__name)) + 
  geom_line() +
  geom_point()

ggplotly(fig)



# Esfuerzo operativo


################
# Crear tabla 4
################


# Se obtiene el estatus de SUSO de la tabla principal.
# El código de disposición final se obtiene de la tabla de llamados
insumo_tabla7 <- asignaciones %>%
  mutate(time_date = make_datetime(year(date), month(date), day(date), hour(time), minute(time), second(time))) %>% 
  filter((action == 7 | action == 8) & responsible__role == 1) %>% # las que han sido reasignadas a un encuestador
  group_by(assignment__id) %>% 
  arrange(desc(as_date(time_date)) ) %>% 
  slice(1) %>% 
  group_by(responsible__name) %>%
  mutate(total_asignaciones = n()) %>%
  ungroup() %>%
  select(responsible__name, assignment__id, total_asignaciones ) %>%
  left_join(principal %>% 
              select(assignment__id, interview__id, interview__key), 
            by = "assignment__id") %>% # sacar id de entrevista
  left_join(llamados %>% 
              select(interview__key, interview__id, rutStr, nomenc, cod_ent, hora_llamado),
            by = c("interview__id", "interview__key"))  %>%
  mutate(contacto = if_else(cod_ent %in% c(1100:2000, 2310:2360), 1, 0, missing = 0),
         no_contacto = if_else(cod_ent %in% c(2200: 2280, 2900), 1, 0, missing = 0),
         fecha = as_date(hora_llamado),
         intento = if_else(nomenc != "##N/A##" & !is.na(cod_ent), 1, 0, missing = 0 ) ) %>% # marcar cada uno de los intentos
  group_by(responsible__name) %>%
  mutate(contactos_total = sum(contacto),
         no_contactos_total = sum(no_contacto),
         intentos_total = sum(intento)) %>%
  group_by(fecha, responsible__name) %>%
  mutate(contactos_diarios = sum(contacto),
         no_contactos_diarios = sum(no_contacto),
         intentos_diarios = n()) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(primera_asigacion, by = "responsible__name") %>% 
  mutate(fecha_inicial = as_date(date),
         fecha_final = as_date(Sys.Date())) %>%
  mutate(dias_corridos = fecha_final - fecha_inicial + 1,
         n_finde = map2_int(.x = .data$fecha_inicial, .y = .data$fecha_final, ~contar_dias_finde(.x, .y)),
         dias_habiles = as.integer(dias_corridos - n_finde))

# Hacer ajuste por días feriados durante el periodo de levantamiento
if (Sys.Date() > as_date("2021-06-28")) {
  insumo_tabla6 <- insumo_tabla6 %>% 
    mutate(dias_habiles =  dias_habiles - 1)
} 

if (Sys.Date() > as_date("2021-07-16")) {
  insumo_tabla6 <- insumo_tabla6 %>% 
    mutate(dias_habiles =  dias_habiles - 1)
}

# Crear parte general de la tabla
tabla7_general <-  insumo_tabla7 %>%
  mutate(media_contacto = contactos_total / dias_habiles,
         media_no_conctato = no_contactos_total / dias_habiles,
         media_intentos = intentos_total / dias_habiles ) %>%
  select(responsible__name, total_asignaciones, contactos_total, no_contactos_total, intentos_total, dias_habiles,
         media_contacto, media_no_conctato, media_intentos ) %>%
  group_by(responsible__name) %>%
  slice(1) %>% 
  ungroup() %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  left_join(usuarios %>% select(login, fullname), by = c("responsible__name" = "login")) %>% 
  relocate(fullname)


# Sacar la tabla a nivel de días
tabla7_por_dia <- insumo_tabla7 %>%
  select( responsible__name, dias_habiles, contactos_diarios, fecha) %>%
  filter(!is.na(fecha)) %>% # sacar las asignaciones sin gestión
  pivot_wider(names_from = "fecha" , values_from = "contactos_diarios", names_prefix = "fecha_") %>% 
  mutate_if(.predicate = is.numeric, ~round(., 2)) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), 0, .))

# Unir ambas partes de la tablaa
tabla7 <- tabla7_general %>%
  left_join(tabla7_por_dia, by = "responsible__name")

# Intentos diarios por cada encuestador
tabla7_por_dia_intentos <- insumo_tabla7 %>%
  select( responsible__name, intentos_diarios, fecha) %>%
  filter(!is.na(fecha)) %>% # sacar las asignaciones sin gestión
  pivot_wider(names_from = "fecha" , values_from = "intentos_diarios", names_prefix = "fecha_") %>% 
  mutate_at(vars(starts_with("fecha_")), ~if_else(is.na(.), 0, as.double(.) ) ) 






DT::datatable(tabla7_general, 
              caption =  htmltools::tags$caption(
                style = 'text-align: center; font-size: 16px',
                htmltools::em('Esfuerzo operativo'))
)





fig <- insumo_tabla7 %>%
  select( responsible__name, dias_habiles, contactos_diarios, fecha) %>% 
  ggplot(aes(x = fecha, y = contactos_diarios, group = responsible__name, color = responsible__name)) + 
  geom_line() +
  geom_point()

ggplotly(fig)



# Resumen de esfuerzo operativo




parte1 <-  tabla7_general %>% 
  select(responsible__name, intentos_total, media_intentos, media_contacto)

parte2 <- tabla6_general %>% 
  select(responsible__name, logradas_total, media_logradas, rutStr)


tabla_resumen <- parte1 %>% 
  left_join(parte2, by = "responsible__name") %>% 
  left_join(usuarios %>% select(login, fullname), by = c("responsible__name" = "login")) %>% # pegar nombre de encuestadores
  relocate(fullname)  %>% 
  select(-intentos_total, -logradas_total)


# Principales indicadores de esfuerzo operativo
DT::datatable(tabla_resumen, 
              caption =  htmltools::tags$caption(
                style = 'text-align: center; font-size: 12px',
                htmltools::em('Esfuerzo operativo encuestadores'))
)




## Esfuerzo versus logro



tabla_resumen <- parte1 %>% 
  left_join(parte2, by = "responsible__name") %>% 
  left_join(usuarios %>% select(login, fullname), by = c("responsible__name" = "login")) %>% # pegar nombre de encuestadores
  mutate(rutStr2 = str_sub(string = rutStr, start = 1, end = nchar(rutStr) - 1),
         rutStr2 = if_else(nchar(rutStr2) == 8, str_sub(rutStr2, 1, 3), str_sub(rutStr2, 1, 2)),  
         rutStr2 = as.integer(rutStr2)) %>%
  mutate(efectividad = intentos_total / logradas_total) %>% 
  relocate(fullname)


intentos_logro <-  tabla_resumen %>% 
  ggplot(aes(x = media_intentos, y = media_logradas, label = fullname)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  labs(title = "Relación entre intentos y logro") +
  theme(plot.title = element_text(hjust = 0.5))


ggplotly(intentos_logro)


intentos_contacto <-  tabla_resumen %>% 
  ggplot(aes(x = media_intentos, y = media_contacto, label = fullname)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  labs(title = "Relación entre intentos y contacto") +
  theme(plot.title = element_text(hjust = 0.5))


ggplotly(intentos_contacto)


contacto_logro <-  tabla_resumen %>% 
  ggplot(aes(x = media_logradas, y = media_contacto, label = fullname)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  labs(title = "Relación entre contactos y logro") +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(contacto_logro)


#cor(tabla_resumen$media_logradas, tabla_resumen$media_intentos)

# Distribución temporal del esfuerzo operativo 


horas <- llamados %>% 
  mutate(hora = format(ymd_hms(hora_llamado), format = "%H:%M:%S"),
         hora = as.POSIXct(hms::parse_hm(hora))) %>% 
  filter(!is.na(hora)) %>% 
  ggplot(aes(x = hora)) +
  geom_histogram( bins = 50, alpha=0.6) +
  scale_x_datetime(date_labels = "%H:%M", breaks = "1 hour" )


ggplotly(horas)

esfuerzo_dia <- llamados %>% 
  mutate(fecha = as_date(hora_llamado),
         dia = wday(as_date(hora_llamado), label = T),
         dia = str_remove(string = dia, pattern = "\\\\."),
         dia = fct_relevel(dia, "lun", "mar", "mié", "jue", "vie", "sáb", "dom"),
         fecha_dia = paste(fecha, dia)) %>%  
  filter(!is.na(fecha)) %>% 
  group_by(fecha_dia) %>% 
  summarise(intentos = n()) %>%
  mutate(habil = if_else(str_detect(fecha_dia, "sáb|dom|2021-06-21|2021-06-28|2021-07-16"), "no hábil", "hábil")) %>% 
  ggplot(aes(x = fecha_dia, y = intentos, group = 1)) +
  geom_line() +
  geom_point(aes(color = habil)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplotly(esfuerzo_dia)





dia_semana <- llamados %>% 
  mutate( dia = wday(as_date(hora_llamado), label = T),
          dia = str_remove(string = dia, pattern = "\\\\."),
          dia = fct_relevel(dia, "lun", "mar", "mié", "jue", "vie", "sáb", "dom")) %>% 
  group_by(dia) %>% 
  summarise(intentos = n()) %>% 
  filter(!is.na(dia)) %>% 
  ggplot(aes(x = dia, y = intentos, fill = dia)) +
  geom_bar(stat = "identity")

ggplotly(dia_semana)



######## GUARDAMOS  OBJETOS #####
datos_nacional <- as.data.frame(t(datos_nacional))
datos_nacional$nombres <- str_replace_all(toupper(row.names(datos_nacional)),"_"," ") %>% stringr::str_replace("POR", "PORCENTAJE")
row.names(datos_nacional) <- NULL

datos_nacional <- datos_nacional %>% mutate(V1 = if_else(stringr::str_detect(nombres,"POR"),paste0(V1,"%"),as.character(V1)))

## TAB 1 ####
# 1
saveRDS(datos_nacional,"objetos/datos_nacional.rds")
# 2
saveRDS(fig_1_ind_gest_nacional,"objetos/fig_1_ind_gest_nacional.rds")
# 3
saveRDS(plot_completadas_dia, "objetos/plot_completadas_dia.rds")
# 4
saveRDS(plot_iniciadas_dia, "objetos/plot_iniciadas_dia.rds")
# 5
saveRDS(datos_region,"objetos/datos_region.rds")
# 6
saveRDS(fig_2_ind_gest_regional,"objetos/fig_2_ind_gest_regional.rds")
# 7
saveRDS(datos_comuna,"objetos/datos_comuna.rds") 

## TAB 2 ####

tasas_nacional  <- as.data.frame(t(tasas_nacional ))
tasas_nacional$nombres <- str_replace_all(toupper(row.names(tasas_nacional )),"_"," ") %>% stringr::str_replace("POR", "PORCENTAJE")
row.names(tasas_nacional ) <- NULL

tasas_nacional  <- tasas_nacional  %>% mutate(V1 = if_else(stringr::str_detect(nombres,"POR"),paste0(V1,"%"),as.character(V1)))

saveRDS(tasas_nacional,"objetos/tasas_nacional.rds")

saveRDS(fig_3_resultado_op_nacional,"objetos/fig_3_resultado_op_nacional.rds")

saveRDS(fig_4_resultado_acum_nacional,"objetos/fig_3_resultado_acum_nacional.rds")


# Rendimiento actualizado
#writexl::write_xlsx(tabla6_por_dia, "reportes_diarios_sdo/reportes_excel_sdo/rendimiento_diario.xlsx")
#writexl::write_xlsx(tabla7_por_dia_intentos, "reportes_diarios_sdo/reportes_excel_sdo/intentos_diarios.xlsx")












