.datatable.aware=TRUE

#' R6 class for ships data
#'
#' @export
#'
#' @import data.table
#'
ships <- R6::R6Class(
    "ships",
    public = list(
        data = NULL,
        summary = NULL,
        #' @description data import and calculations
        #'
        #' @param path *character* path to *.rds file
        #' @param mem *logical* save memory? Otherwise, the complete data
        #'  is stored
        #'
        #' @return *ships* R6 instance
        #' @export
        #'
        #' @examples \dontrun{test <- ships$new(inst/extdata/ships.rds)}
        initialize = function(path, mem = TRUE) {
            ## Load and clean data ##

            self$data <- readRDS(path)[, .(LAT, LON, SHIPNAME, SHIP_ID,
                                           DATETIME, ship_type, SPEED,
                                           DESTINATION)]

            self$data[, nobs := nrow(.SD), by = .(SHIP_ID)]
            self$data <- self$data[nobs > 1]

            ## Calculate distances ##
            self$data[, loc_diff := haversine(.SD),
                      by = .(SHIP_ID)]

            self$summary <- self$data[, .(vessel_max = max(loc_diff),
                          vessel_type = ship_type[which.max(loc_diff)],
                          vessel_name = SHIPNAME[which.max(loc_diff)],
                          LAT1 = LAT[which.max(loc_diff)],
                          LON1 = LON[which.max(loc_diff)],
                          LAT2 = LAT[which.max(loc_diff) + 1],
                          LON2 = LON[which.max(loc_diff) + 1],
                          time = abs(DATETIME[which.max(loc_diff)+1] -
                              DATETIME[which.max(loc_diff)]),
                          SPEED = SPEED[which.max(loc_diff)],
                          DESTINATION = DESTINATION[which.max(loc_diff)]),
                      by = .(SHIP_ID)]
            if (mem) {
                # remove data to save memory
                self$data <- NULL
                gc()
            }

            ## final checks ##
            # in case there is the same SHIP_ID more than once in the summary
            # because two identical maximum distances were recorded,
            # keep only the first entry.
            self$summary[, .SD[1], by = .(SHIP_ID)]

            },
        #' @description Display Leaflet map
        #'
        #' @param sel_type selected ship type
        #' @param sel_name selected ship name
        #'
        #' @export
        #'
        #' @examples \dontrun{test$display_ship("Cargo", "KAROLI")}
        display_ship = function(sel_type, sel_name) {
            leaflet::leaflet(self$summary[
                which(vessel_type == sel_type & vessel_name == sel_name)
                ]) %>%
                leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
                leaflet::addMarkers(
                    lng = ~LON1,
                    lat = ~LAT1,
                    popup = "Start") %>%
                leaflet::addMarkers(
                    lng = ~LON2,
                    lat = ~LAT2,
                    popup = "End") %>%
                leaflet::addPolylines(
                    lng = self$summary[
                        which(vessel_type == sel_type & vessel_name == sel_name),
                        .(LON1, LON2)
                    ] %>% as.numeric(),
                    lat = self$summary[
                        which(vessel_type == sel_type & vessel_name == sel_name),
                        .(LAT1, LAT2)
                    ] %>% as.numeric(),
                    weight = 2, color = "red",
                    label = ~paste(" Distance: ",
                                   round(vessel_max),
                                   " m"),
                    labelOptions = leaflet::labelOptions(noHide = T)
                )


        }

                     ))

#' Calculate shortest distance between two points on earth
#'
#' @param dt a data.table
#'
#' @import data.table
#'
haversine <- function(dt) {
    return(
        c(geosphere::distHaversine(data.frame(dt[, .(LON, LAT)])), 0)
    )
}
