datapath <- system.file("extdata/ships.rds", package = "ships")
test <- ships$new(datapath, mem = FALSE)

test_that("Last distance entries of vessels are 0", {
    expect_true(
        all(test$data[, .SD[.N], by = .(SHIP_ID)
        ][, .(loc_diff)] == 0)
    )
})

test_that("Summarization preserves the correct information", {

    expect_true(
        all(do.call(paste0, test$summary[, .(SHIP_ID, LON1, LAT1, vessel_max)]) %in%
                    do.call(paste0, test$data[, .(SHIP_ID, LON, LAT, loc_diff)]))
        )
})
