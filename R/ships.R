# R6 class for ships data

ships <- R6::R6Class(
    "ships",
    public = list(
        data = NULL,
        summary = NULL,
        initialize = function(path) {
            ## Sanity checks ##
            data <- data.table::fread(path)

            data[, nobs := nrow(.SD), by = .(SHIP_ID)]
            data <- data[nobs > 1]

            self$data <- data
            ## Calculate subsequent distances ##
            # Note: the last point for every SHIP_ID conects to
            # a new ship!
            self$data[, loc_diff := haversine(.SD),
                      by = .(SHIP_ID)]
            # self$data[,vessel_obs := 1:.N, by = .(SHIP_ID)]

            self$summary <- self$data[, .(vessel_max = max(loc_diff),
                          LAT1 = LAT[which.max(loc_diff)],
                          LON1 = LON[which.max(loc_diff)],
                          LAT2 = LAT[which.max(loc_diff) + 1],
                          LON2 = LON[which.max(loc_diff) + 1],
                          DATETIME = DATETIME[which.max(loc_diff)]),
                      by = .(SHIP_ID)]


            # self$summary <- rbind(self$summary,self$summary[.N])
            # self$summary[.N, DATETIME := as.POSIXct("2016-12-19 08:40:04")]
            # self$summary[, ndup := nrow(.SD), by = .(SHIP_ID)]

            # in case there is the same SHIP_ID more than once in the summary
            # beacause two identical maximum distances were recorded,
            # keep only the first entry.
            self$summary[, .SD[1], by = .(SHIP_ID)]

            },
        display_ship = function(shipID) {
            leaflet::leaflet() %>%
                leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
                leaflet::addMarkers(lng = self$summary[SHIP_ID == shipID, LON1],
                                    lat = self$summary[SHIP_ID == shipID, LAT1],
                                    popup = "Start") %>%
                leaflet::addMarkers(lng = self$summary[SHIP_ID == shipID, LON2],
                                    lat = self$summary[SHIP_ID == shipID, LAT2],
                                    popup = "End")


        }

                     ))

haversine <- function(dt) {
    return(
        c(geosphere::distHaversine(data.frame(dt[, .(LON, LAT)])), 0)
    )
}
