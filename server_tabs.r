source("funciones.r")
source("objetos/objetos.R")


tabServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #### tab 1 ###
      output$fig_1_ind_gest_nacional_OU <- renderPlotly({ggplotly(fig_1_ind_gest_nacional + theme_elegante())  %>% config(displayModeBar = F)})
      output$out_ind_gestion <- renderText({ind_gestion.html})
      output$out_ind_result_op <- renderText({ind_result_op.html})
      
    }
  )
}
