
source("funciones.r")
source("objetos/objetos.R")

### tab 1 ####
tabItem_function1 <- function(id,...){
  ns <- NS(id)
  
 tabItem(tabName = "TAB_ind_gest_datos", h2("1. Indicadores de gestión - Resultados:"),
       box(
         title = "1 - Nivel Nacional", 
         closable = F, 
         width = 12,
         height = "500px",
         status = "warning", 
         solidHeader = FALSE, 
         collapsible = TRUE,
         column(width = 8,
         
         actionButton(inputId = ns("btn_def_ind_gest"),label = "Definición - indicadores",    style = "color: white; 
                         background-color: SteelBlue;" ),
         plotlyOutput("fig_1_ind_gest_nacional_OU",
                             width = "100%",
                             height = "900px") ),
         column(width = 4, 
         
         descript_block_function(datos_nacional), 

        )
    ),
    box(
      title = "2 - Encuestas por día", 
      closable = F, 
      width = 12,
      height = "500px",
      status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      column(width = 11,
      
      plotlyOutput("plot_completadas_dia_OU",
                   width = "90%",
                   height = "700px"),
      plotlyOutput("plot_iniciadas_dia_OU",
                   width = "90%",
                   height = "700px")
      )
    ),
    box(
    title = "3 - Nivel Regional", 
    closable = F, 
    width = 12,
    height = "500px",
    status = "warning", 
    solidHeader = FALSE, 
    collapsible = TRUE,
    column(width = 11,
           DT::datatable(datos_region), 
           plotlyOutput("fig_2_ind_gest_regional_OU",
                        width = "90%",
                        height = "700px")
           )
    ),
    box(
      title = "4 - Nivel Comunal", 
      closable = F, 
      width = 12,
      height = "500px",
      status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      column(width = 11,
             DT::datatable(datos_comuna)
      )
    )
    
    
 
   
    )
}
### tab 2 ####
tabItem_function2 <- function(id,...){
   #tabItem(tabName = "TAB_ind_gest_def", h2("1. Indicadores de gestión - Definiciones:"),br(), htmlOutput("out_ind_gestion") )

  tabItem(tabName = "TAB_ind_result_datos", h2("1. Indicadores de gestión - Resultados:"),
          box(
            title = "1 - Nivel Nacional", 
            closable = F, 
            width = 12,
            height = "500px",
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            column(width = 11,
                   
                  # actionButton(inputId = ns("btn_def_ind_gest"),label = "Definición - indicadores",    style = "color: white;  background-color: SteelBlue;" ),
                   plotlyOutput("fig_3_resultado_op_naciona_OU",
                                width = "100%",
                                height = "900px"),
                  plotlyOutput("fig_4_resultado_acum_nacional_OU ",
                               width = "90%",
                               height = "700px")),
            column(width = 4, 
                   
                   descript_block_function(tasas_nacional), 
                   
            )
          ),
          box(
            title = "2 - Encuestas por día", 
            closable = F, 
            width = 12,
            height = "500px",
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            column(width = 11,
                   
  
            )
          ),
          box(
            title = "3 - Nivel Regional", 
            closable = F, 
            width = 12,
            height = "500px",
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            column(width = 11,
                   DT::datatable(datos_region), 
                   plotlyOutput("fig_2_ind_gest_regional_OU",
                                width = "90%",
                                height = "700px")
            )
          ),
          box(
            title = "3 - Nivel Comunal", 
            closable = F, 
            width = 12,
            height = "500px",
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            column(width = 11,
                   DT::datatable(datos_comuna)
            )
          )
  )
     
   }
#### tab 3 ####
# tabItem_function2 <- function(...){
#   tabItem(tabName = "TAB_ind_result_datos", h2("2 Indicadores de resultado operativo - Resultados:"))  #3
# 
#   
# }
# ### tab 4 ####
# tabItem_function2 <- function(...){
# 
#   tabItem(tabName = "TAB_ind_result_def", h2("2 Indicadores de resultado operativo - Definiciones:"), br(), 
#           htmlOutput("out_ind_result_op")
#   ) 
#   
#   
# }
# ### tab 5 ####
# tabItem_function2 <- function(...){
#   tabItem(tabName = "TAB_rend_diar", h2("3 Rendimiento diario encuestador"), h3("Definiciones:"))
#   
#   
#   
# }
#### tab 6 ####
#tabItem_function2 <- function(...){
#  tabItem(tabName = "TAB_det_esf_op", h2("4 Esfuerzo operativo"),h3("Detalle")),                               #6
#  tabItem(tabName = "TAB_res_esf_op", h2("4 Esfuerzo operativo"),h3("Resumen")),                               #7
#  tabItem(tabName = "TAB_esf_vs_log", h2("4 Esfuerzo operativo"),h3("Esfuerzo v/s Logro")),                               #8
#  tabItem(tabName = "TAB_intent_dia", h2("5 Distribución de intentos a lo largo del día"))               #9
#  
#  
#  
#}
#### tab 7 ####
#tabItem_function2 <- function(...){
#  tabItem(tabName = "TAB_det_esf_op", h2("4 Esfuerzo operativo"),h3("Detalle")),                               #6
#  tabItem(tabName = "TAB_res_esf_op", h2("4 Esfuerzo operativo"),h3("Resumen")),                               #7
#  tabItem(tabName = "TAB_esf_vs_log", h2("4 Esfuerzo operativo"),h3("Esfuerzo v/s Logro")),                               #8
#  tabItem(tabName = "TAB_intent_dia", h2("5 Distribución de intentos a lo largo del día"))               #9
#  
#  
#  
#}
#### tab 6 ####
#tabItem_function2 <- function(...){
#  tabItem(tabName = "TAB_det_esf_op", h2("4 Esfuerzo operativo"),h3("Detalle")),                               #6
#  tabItem(tabName = "TAB_res_esf_op", h2("4 Esfuerzo operativo"),h3("Resumen")),                               #7
#  tabItem(tabName = "TAB_esf_vs_log", h2("4 Esfuerzo operativo"),h3("Esfuerzo v/s Logro")),                               #8
#  tabItem(tabName = "TAB_intent_dia", h2("5 Distribución de intentos a lo largo del día"))               #9
#  
#  
#  
#}





















