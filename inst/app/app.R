library(shiny)
library(shiny.semantic)
# library(ships)
devtools::load_all(".")

ui <- semanticPage(
    titlePanel("Ship movements - December 2016", windowTitle = "ships"),
    "Displayed is the longest distance between two recording positions for each vessel",
    sidebar_layout(
        sidebar_panel(
            uiOutput("vessel_type"),
            uiOutput("vessel_name"),
            tableOutput("infoTab")
        ),
        main_panel(
            leaflet::leafletOutput("map", height = 600)
        )
    )
)

server <- function(input, output, session) {

    rv <- reactiveValues()
    observe({ # initialization
        isolate({
            datapath <- system.file("extdata/ships.rds", package = "ships")
            rv$shinyShip <- ships::ships$new(datapath)
        })
        # prepare drop down menu for vessel_type
        output$vessel_type <- renderUI({
            selectInput("vessel_type", "Choose Vessel Type",
                        choices = unique(rv$shinyShip$summary[, vessel_type]),
                        selected = rv$shinyShip$summary[1, vessel_type])
        })
    })

    # dynamic drop down menu for vessel_name
    observeEvent(input$vessel_type, {
        isolate({
            if (is.null(input$vessel_type)) {
                rv$vt <- "Cargo"
            } else {
                rv$vt <- input$vessel_type
            }
        })

        output$vessel_name <- renderUI({

            selectInput("vessel_name", "Choose Vessel",
                        choices = rv$shinyShip$summary[vessel_type == rv$vt,
                                                       vessel_name],
                        selected = rv$shinyShip$summary[vessel_type == rv$vt,
                                                        vessel_name][1])
        })

    })

    observeEvent(req(input$vessel_name, input$vessel_type), {
        isolate({
            sub <- rv$shinyShip$summary[
                which(vessel_name == input$vessel_name,
                      vessel_type == input$vessel_type),
                ]
            rv$info <- data.frame(
                "." = c("ID", "Time", "Destination", "Speed", "Flag"),
                "vessel info" = c(as.character(sub$SHIP_ID),
                           paste0(round(sub$time[[1]] / 3600, 2), " h"),
                           sub$DESTINATION,
                           as.character(sub$SPEED),
                           sub$FLAG)
            )
            colnames(rv$info)[1] <- ""
        })
        output$infoTab = renderTable({
            rv$info
        })


        output$map <- leaflet::renderLeaflet({
            rv$shinyShip$display_ship(input$vessel_type, input$vessel_name)
        })
    })


}

shinyApp(ui, server)
