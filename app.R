
## bibliografia ####
# fulll hide sidebar
# https://stackoverflow.com/questions/53507487/collapse-left-sidebar-fully-in-shinydashboardplus

## speedup shinyapps
# https://appsilon.com/r-shiny-faster-updateinput-css-javascript/

# https://www.r-bloggers.com/2021/06/speeding-up-r-shiny-the-definitive-guide/

# Shiny / dashboard: going further… and deploy
# http://www.gcoqueret.com/files/shiny/S6_deploy.html#8

# favicon generator
# https://favicon.io/favicon-generator/

#  paquetes #### 
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
#library(susor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(rio)
library(tidyverse)
library(plotly)
library(shinyAce)


# tabla <- datos_nacional
# num <- 1
### cargamos datos, funciones y tablas ###
#source("descarga_procesamiento_datos.R")
 source("objetos/objetos.R")
 source("tabs.r")
 source("server_tabs.r")
 source("funciones.r")


ind_gestion.html <-includeHTML("indicadores_gestion.html")
ind_result_op.html <-includeHTML("Indicadores_reuslt_operativo.html")

##### UI ####
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
        #   tags$head(tags$script(jscode)),
        #   tags$head(tags$style(csscode)),    
        collapsed = F,
          width = 430,
        column(12,
               sidebarMenu(
                 tab_function(label = "1 Indicadores de gestión", tabName = "TAB_ind_gest_datos"),
                       #   menuSubItem(text = h6("-  1.1 Resultados"), tabName = "TAB_ind_gest_datos",icon = ""),                   # 1
                          #menuSubItem(text = h6("-  1.2 Definiciones"), tabName = "TAB_ind_gest_def",icon = "")),                  # 2
                                    
                tab_function("2 Indicadores de resultado operativo", tabName = "TAB_ind_result_datos")#,
                       #  menuSubItem(text = h6("-  2.1 Resultados"), tabName = "TAB_ind_result_datos",icon = ""),                 # 3
                       #  menuSubItem(text = h6("-  2.2 Definiciones"),tabName = "TAB_ind_result_def",icon = "")),                 # 4

           #    tab_function("3 Rendimiento diario encuestador",tabName = "TAB_rend_diar",icon = NULL),                        # 5
           #    
           #    tab_function("4 Esfuerzo operativo", startExpanded = T, tabName = "TAB_esfu_opera",icon = NULL,               # 6
           #             menuSubItem(text = h6("-  4.1 Detalle"), tabName = "TAB_det_esf_op",icon = ""),
           #             menuSubItem(text = h6("-  4.2 Resumen"), tabName = "TAB_res_esf_op",icon = ""),                           # 7
           #             menuSubItem(text = h6("-  4.3 Esfuerzo versus logro"),  tabName = "TAB_esf_vs_log",icon = "")),          # 8
           #             
           # tab_function("5 Distribución de intentos a lo largo del día", tabName = "TAB_intent_dia",icon = NULL)      # 9
        ) # sidebarmenu
     ) # Column  
 ), # dashboardSidebar
    dashboardBody(
      tabItems(
        
#### TAB 1 #####        
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
                         
                         actionButton(inputId = "btn_def_ind_gest",label = "Definición - indicadores",    style = "color: white; 
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

        ),
        
# #### TAB 2 #####        
        tabItem(tabName = "TAB_ind_result_datos", h2("2 Indicadores de resultado operativo - Resultados:"),
                box(
                  title = "1 - Nivel Nacional",
                  closable = F,
                  width = 12,
                  height = "500px",
                  status = "warning",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  column(width = 8,
                  plotlyOutput("fig_3_resultado_op_nacional_OU",
                                      width = "100%",
                                       height = "900px")),  
                  column(width = 4, descript_block_function(tasas_nacional), )
                  ),
                box(
                  title = "2 - Acumulados",
                  closable = F,
                  width = 12,
                  height = "500px",
                  status = "warning",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  column(width = 11, #  DT::datatable(datos_region),
                         plotlyOutput("fig_4_resultado_acum_nacional_OU",
                                      width = "90%",
                                      height = "700px")
                  )
                )# ,
                 #  box(
                 #    title = "3 - Nivel Regional",
                 #    closable = F,
                 #    width = 12,
                 #    height = "500px",
                 #    status = "warning",
                 #    solidHeader = FALSE,
                 #    collapsible = TRUE,
                 #    column(width = 11#,       DT::datatable(datos_comuna)
                 #    )
                 #  ),
                 #  box(
                 #    title = "3 - Nivel Comunal",
                 #    closable = F,
                 #    width = 12,
                 #    height = "500px",
                 #    status = "warning",
                 #    solidHeader = FALSE,
                 #    collapsible = TRUE,
                 #    column(width = 11#,  DT::datatable(datos_comuna)
                 #    )
                 # )
          )  
      )  
  )
)


server <- function(input, output, session) {
  
  #### tab 1 ###
  # tabServer("tab_1")

## box 1 ####
    output$fig_1_ind_gest_nacional_OU <- renderPlotly({fig_1_ind_gest_nacional %>% config(displayModeBar = F)})
    output$out_ind_gestion <- renderText({ind_gestion.html})
    output$out_ind_result_op <- renderText({ind_result_op.html})

## box 2 ####
    output$plot_completadas_dia_OU <- renderPlotly({plot_completadas_dia %>% config(displayModeBar = F)})
    output$plot_iniciadas_dia_OU <- renderPlotly({plot_iniciadas_dia %>% config(displayModeBar = F)})

## box 3 ####
    output$fig_2_ind_gest_regional_OU <- renderPlotly({fig_2_ind_gest_regional %>% config(displayModeBar = F)})

  #### tab2 ####
    
## box 1 ####  
    output$fig_3_resultado_op_nacional_OU <- renderPlotly({fig_3_resultado_op_nacional %>% config(displayModeBar = F)})
    output$fig_4_resultado_acum_nacional_OU <- renderPlotly({fig_4_resultado_acum_nacional %>% config(displayModeBar = F)})
    
}

shinyApp(ui, server)
