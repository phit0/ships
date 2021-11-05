library(shiny)

ui <- fluidPage(
    uiOutput("vessel_type"),
    uiOutput("ship_id"),
    leaflet::leafletOutput("map")

)

server <- function(input, output, session) {

    rv <- reactiveValues()
    observe({ # initialization
        isolate({
            datapath <- system.file("../ships.csv", package = "ships")
            rv$shinyShip <- ships$new(datapath)
        })
        # prepare drop down menu for vessel_type
        output$vessel_type <- renderUI({
            selectInput("vessel_type", "Choose Vessel Type",
                        choices = unique(rv$shinyShip$summary[, vessel_type]),
                        selected = rv$shinyShip$summary[1, vessel_type])
        })
    })

    # dynamic drop down menu for ship_id
    observeEvent(input$vessel_type, {
        isolate({
            if (is.null(input$vessel_type)) {
                rv$vt <- "Cargo"
            } else {
                rv$vt <- input$vessel_type
            }
        })

        output$ship_id <- renderUI({

            selectInput("ship_id", "Choose Ship ID",
                        choices = rv$shinyShip$summary[vessel_type == rv$vt,
                                                       SHIP_ID],
                        selected = rv$shinyShip$summary[vessel_type == rv$vt,
                                                        SHIP_ID][1])
        })

    })

    observeEvent(input$ship_id, {
        output$map <- leaflet::renderLeaflet({
            rv$shinyShip$display_ship(input$vessel_type, input$ship_id)
        })
    })


}

shinyApp(ui, server)
