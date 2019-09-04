library(shiny)

# Define UI for application 
fluidPage(
    
    # Application title
    titlePanel("Survey Data From One ForestGEO Site (400m by 400m) at Front Royal, Virginia"),
    
    fluidRow(
        column(2,
               numericInput("radius",
                            "Fixed-radius value (m):",
                            min = 1,
                            max = 20,
                            value = 10)
        ),
        column(3,
               selectInput("baf",
                           label = "Basal Area Factor (m2/ha):",
                           choices = c(1, 2, 3, 4, 5, 6),
                           selected = 5)
        ),
        column(3,
               numericInput("xcoord",
                            "X coordinate of point (m):",
                            min = 10,
                            max = 390,
                            value = 200)
        ),
        column(3,
               numericInput("ycoord",
                            "Y coordinate of point (m):",
                            min = 10,
                            max = 390,
                            value = 200)
        ),
        column(1,
               actionButton("run", "Go")
        )
    ),
    
    fluidRow(
        column(4, plotOutput("allPlot")),
        column(4, plotOutput("fixedPlot")),
        column(4, plotOutput("variablePlot"))
    ),
    
    fluidRow(
        column(3, plotOutput("comparePlot")),
        column(3, plotOutput("comparePlot2")),
        column(6, tableOutput("basalAreas"))
    )
)
