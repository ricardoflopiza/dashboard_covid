library(shiny)
library(DT)
library(feather)


source("descarga_procesamiento_datos.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    fluidRow(column(6,DT::dataTableOutput('x1')),
             column(6, plotOutput('x2', height = 500)))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$x1 = DT::renderDataTable(cars, server = FALSE)
    
    # highlight selected rows in the scatterplot
    output$x2 = renderPlot({
        s = input$x1_rows_selected
        par(mar = c(4, 4, 1, .1))
        plot(cars)
        if (length(s)) points(cars[s, , drop = FALSE], pch = 19, cex = 2)
    })
    
    # server-side processing
    mtcars2 = mtcars[, 1:8]
    output$x3 = DT::renderDataTable(mtcars2, server = TRUE)
    
    # print the selected indices
    output$x4 = renderPrint({
        s = input$x3_rows_selected
        if (length(s)) {
            cat('These rows were selected:\n\n')
            cat(s, sep = ', ')
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
