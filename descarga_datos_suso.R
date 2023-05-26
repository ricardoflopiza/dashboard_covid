library(susor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(rio)
library(tidyverse)
library(plotly)



#Parámetros
survey_dir = "C:/Users/klehm/OneDrive - Instituto Nacional de Estadisticas/covid"
susor_qn_variable <- "covid19_r3_v2" #  "covid19_r3"
susor_qn_version <- 2
susor_quid <- "b84dc544-e710-4fea-92eb-476ba99f5eab$2"
susor_server = "https://survey.ine.gob.cl/covid2021"
susor_password <- "(INE2021p)"
susor_user = "API_COVID"

# Ingresar permisos
susor_login(susor_server = susor_server,
            susor_user = susor_user,
            susor_password = susor_password,
            susor_dir_downloads = file.path(survey_dir, "/data"),
            susor_dir_raw = file.path(survey_dir, "raw")
)


# Decirle al servidor que genere los archivos que se quieren descargar. Dentro de susor_generate_file está el POST
post_export <- susor_generate_file(
  susor_quid = 	susor_quid, #
  susor_format = "TABULAR"
)

#Seleccionar el jobid y la query, para usarla después
JobID <- post_export$jobID
apiGenerate <- post_export$apiGenerate


# Identificar el status del request. Esto se usa por si el servidor está lento
Json_GET = tempfile(fileext = ".json")
response_file_status = GET(apiGenerate, authenticate(susor_user,
                                                     susor_password),
                           encode = "json",
                           write_disk(Json_GET,
                                      overwrite = T),
                           query = list(questionnaireIdentity = susor_quid)
)

file_status_data = jsonlite::fromJSON(Json_GET)
file_status = file_status_data[file_status_data$JobId == JobID, ]$ExportStatus


# Genera mensajes al usuario, mientras el request está siendo procesado. Cuando cambia de status, el loop termina
while (file_status != "Completed") {
  Sys.sleep(1)
  message(paste("Creating", susor_qn_variable, "version:", susor_qn_version, "in Server"))
  Json_GET = tempfile(fileext = ".json")
  response_file_status = GET(apiGenerate, authenticate(susor_user,
                                                       susor_password),
                             encode = "json",
                             write_disk(Json_GET,
                                        overwrite = T),
                             query = list(questionnaireIdentity = susor_quid)
  )
  
  file_status_data = jsonlite::fromJSON(Json_GET)
  file_status = file_status_data[file_status_data$JobId == JobID, ]$ExportStatus
  
  
}

#Se genera la url para hacer la exportación. Esto necesita el jobid y la url del servidor
apiExport <- sprintf("%s/api/v2/export/%s/file", susor_server, JobID)

#Este get hace la exportación
response_export <- GET(apiExport, authenticate(susor_user, susor_password))

#Sacar el estatus de la exportación
status <- response_export$status_code

# Crear un archivo temporal comprimido, en el cual se guardará la descarga
unzip_name <- paste0(susor_qn_variable,"_", susor_qn_version)
zip_name <- paste0(unzip_name,".zip")
zipfile <- file.path(tempdir(),zip_name)

# Crear el directorio en el cual será descomprimido el archivo
outputdir <- file.path(susor_dir_downloads, unzip_name )

if (!file.exists(outputdir)){
  dir.create(outputdir)
  
}

# Abrir la conexión del archivo temporal
filecon <- file(zipfile, "wb")

#Escribir en el archivo
writeBin(response_export$content, filecon)

#Cerrar la conexión con el archivo
close(filecon)

## Descomprimir los datos en el directorio seleccionado
unzip(zipfile = zipfile, overwrite = TRUE,
      exdir = outputdir,
      unzip = "internal")



##############################################
# Descargar segunda versión del cuestionario #
##############################################


#Parámetros
survey_dir = "C:/Users/klehm/OneDrive - Instituto Nacional de Estadisticas/covid"
susor_qn_variable <- "covid19_r3_v2" #  "covid19_r3"
susor_qn_version <- 3
susor_quid <- "b84dc544-e710-4fea-92eb-476ba99f5eab$3"
susor_server = "https://survey.ine.gob.cl/covid2021"
susor_password <- "(INE2021p)"
susor_user = "API_COVID"


# Decirle al servidor que genere los archivos que se quieren descargar. Dentro de susor_generate_file está el POST
post_export <- susor_generate_file(
  susor_quid = 	susor_quid, #
  susor_format = "TABULAR"
)

#Seleccionar el jobid y la query, para usarla después
JobID <- post_export$jobID
apiGenerate <- post_export$apiGenerate


# Identificar el status del request. Esto se usa por si el servidor está lento
Json_GET = tempfile(fileext = ".json")
response_file_status = GET(apiGenerate, authenticate(susor_user,
                                                     susor_password),
                           encode = "json",
                           write_disk(Json_GET,
                                      overwrite = T),
                           query = list(questionnaireIdentity = susor_quid)
)

file_status_data = jsonlite::fromJSON(Json_GET)
file_status = file_status_data[file_status_data$JobId == JobID, ]$ExportStatus


# Genera mensajes al usuario, mientras el request está siendo procesado. Cuando cambia de status, el loop termina
while (file_status != "Completed") {
  Sys.sleep(1)
  message(paste("Creating", susor_qn_variable, "version:", susor_qn_version, "in Server"))
  Json_GET = tempfile(fileext = ".json")
  response_file_status = GET(apiGenerate, authenticate(susor_user,
                                                       susor_password),
                             encode = "json",
                             write_disk(Json_GET,
                                        overwrite = T),
                             query = list(questionnaireIdentity = susor_quid)
  )
  
  file_status_data = jsonlite::fromJSON(Json_GET)
  file_status = file_status_data[file_status_data$JobId == JobID, ]$ExportStatus
  
  
}

#Se genera la url para hacer la exportación. Esto necesita el jobid y la url del servidor
apiExport <- sprintf("%s/api/v2/export/%s/file", susor_server, JobID)

#Este get hace la exportación
response_export <- GET(apiExport, authenticate(susor_user, susor_password))

#Sacar el estatus de la exportación
status <- response_export$status_code

# Crear un archivo temporal comprimido, en el cual se guardará la descarga
unzip_name <- paste0(susor_qn_variable,"_", susor_qn_version)
zip_name <- paste0(unzip_name,".zip")
zipfile <- file.path(tempdir(),zip_name)

# Crear el directorio en el cual será descomprimido el archivo
outputdir <- file.path(susor_dir_downloads, unzip_name )

if (!file.exists(outputdir)){
  dir.create(outputdir)
}

# Abrir la conexión del archivo temporal
filecon <- file(zipfile, "wb")

#Escribir en el archivo
writeBin(response_export$content, filecon)

#Cerrar la conexión con el archivo
close(filecon)

## Descomprimir los datos en el directorio seleccionado
unzip(zipfile = zipfile, overwrite = TRUE,
      exdir = outputdir,
      unzip = "internal")

