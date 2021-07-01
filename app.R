
#  paquetes #### 
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(susor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(rio)
library(tidyverse)
library(plotly)
library(shinyAce)


### cargamos datos, funciones y tablas ###
#source("descarga_procesamiento_datos.R")

ind_gestion.html <-includeHTML("indicadores_gestion.html")
ind_result_op.html <-includeHTML("Indicadores_reuslt_operativo.html")

##### UI ####
# fulll hide sidebar
# https://stackoverflow.com/questions/53507487/collapse-left-sidebar-fully-in-shinydashboardplus

jscode <- HTML("
$(document).on('shiny:connected', function(event) {
  $('.sidebar-toggle').on('click', function() {
    if ($('body')[0].className != 'skin-blue sidebar-mini sidebar-collapse') {
      $('#sidebarCollapsed').css('display', 'none')
    } else {
      $('#sidebarCollapsed').css('display', 'block')
    }
  })
});
")

csscode <- HTML("
.sidebar-mini.sidebar-collapse .content-wrapper {
      margin-left: 0px !important;
}")

ui <- dashboardPage(skin= "black",
    # tags$head(tags$link(rel="shortcut icon", href="apple-icon-57x57.png")),
    dashboardHeader(disable = F,
                    titleWidth = 330,
                  title = tagList(
                      span(class = "logo-lg", "Tablero - COVID"), 
                      img(src = "apple-icon-57x57.png", width = 30))
                  #  enable_rightsidebar = F,
                  #  rightSidebarIcon = "gears"
                  ),
    dashboardSidebar(
         #  tags$head(tags$script(jscode)),
        #   tags$head(tags$style(csscode)),    
        collapsed = F,
          width = 330,
        column(12,
               sidebarMenu(
                 menuItem(h6("1 Indicadores de gestión"), startExpanded = T,
                          menuSubItem(text = h6("-  1.1 Resultados"), tabName = "TAB_ind_gest_datos",icon = ""),                   # 1
                          menuSubItem(text = h6("-  1.2 Definiciones"), tabName = "TAB_ind_gest_def",icon = "")),                  # 2
                                    
                 menuItem(h6("2 Indicadores de resultado operativo") , startExpanded = T,          
                          menuSubItem(text = h6("-  2.1 Resultados"), tabName = "TAB_ind_result_datos",icon = ""),                 # 3
                          menuSubItem(text = h6("-  2.2 Definiciones"),tabName = "TAB_ind_result_def",icon = "")),                 # 4

                 menuItem(h6("3 Rendimiento diario encuestador"),tabName = "TAB_rend_diar",icon = NULL),                        # 5
                 
                 menuItem(h6("4 Esfuerzo operativo"), startExpanded = T, tabName = "TAB_esfu_opera",icon = NULL,               # 6
                          menuSubItem(text = h6("-  4.1 Detalle"), tabName = "TAB_det_esf_op",icon = ""),
                          menuSubItem(text = h6("-  4.2 Resumen"), tabName = "TAB_res_esf_op",icon = ""),                           # 7
                          menuSubItem(text = h6("-  4.3 Esfuerzo versus logro"),  tabName = "TAB_esf_vs_log",icon = "")),          # 8
                          
                  menuItem(h6("5 Distribución de intentos a lo largo del día"), tabName = "TAB_intent_dia",icon = NULL)      # 9
        ) # sidebarmenu
     ) # Column  
 ), # dashboardSidebar
    dashboardBody(
      tabItems(
      tabItem(tabName = "TAB_ind_gest_datos", h2("1. Indicadores de gestión - Resultados:"), ),                #1
      tabItem(tabName = "TAB_ind_gest_def", h2("1. Indicadores de gestión - Definiciones:"),br(), htmlOutput("out_ind_gestion") ),    #2
      tabItem(tabName = "TAB_ind_result_datos", h2("2 Indicadores de resultado operativo - Resultados:")),  #3
      tabItem(tabName = "TAB_ind_result_def", h2("2 Indicadores de resultado operativo - Definiciones:"), br(), htmlOutput("out_ind_result_op")
              ),  #4
      tabItem(tabName = "TAB_rend_diar", h2("3 Rendimiento diario encuestador"), h3("Definiciones:")),                                #5
      tabItem(tabName = "TAB_det_esf_op", h2("4 Esfuerzo operativo"),h3("Detalle")),                               #6
      tabItem(tabName = "TAB_res_esf_op", h2("4 Esfuerzo operativo"),h3("Resumen")),                               #7
      tabItem(tabName = "TAB_esf_vs_log", h2("4 Esfuerzo operativo"),h3("Esfuerzo v/s Logro")),                               #8
      tabItem(tabName = "TAB_intent_dia", h2("5 Distribución de intentos a lo largo del día"))               #9
         ) 
      )
  )

server <- function(input, output, session) {
  output$out_ind_gestion <- renderText({ind_gestion.html})
  output$out_ind_result_op <- renderText({ind_result_op.html})
  
  
        
}

shinyApp(ui, server)
