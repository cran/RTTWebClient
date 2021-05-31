reqTime <- as.POSIXct("2030-01-01", tz = "GMT")
reqTimeInMS <- round(as.double(reqTime)) * 1000
ttPublicClient <- InitPublicWebClient(server = "ttlivewebapi.fxopen.com")
test_that("Is Last 10 Ticks right format", {
  # skip_on_cran()
  # if(requireNamespace("lubridate", quietly = TRUE)){
  #   reqTimeInMS <- round(as.double(lubridate::now("UTC")) * 1000)
  # }else{
  #   reqTimeInMS <- 0
  # }
  vcr::use_cassette("Public_client_last10Ticks", {
    ticks <- ttPublicClient$GetTicksRawMethod("EURUSD", reqTimeInMS, count = -10)
  })
  expect_equal(typeof(ticks), "list")
  # expect_true(is.data.table(ticks))
  ticksColNames <- colnames(ticks)
  expect_identical(ticksColNames, c("Timestamp", "BidPrice", "BidVolume", "BidType","AskPrice","AskVolume", "AskType"))

})

test_that("Is Last 10 Bars right format", {
  # skip_on_cran()
  # if(requireNamespace("lubridate", quietly = TRUE)){
  #   reqTimeInMS <- round(as.double(lubridate::now("UTC")) * 1000)
  # }else{
  #   reqTimeInMS <- 0
  # }
  vcr::use_cassette("Public_client_last10M1Bars", {
    bars <- ttPublicClient$GetBarRawMethod("EURUSD", "Bid", "M1", reqTimeInMS, count = -10)
  })
  expect_equal(typeof(bars), "list")
  # expect_true(is.data.table(bars))
  barsColNames <- colnames(bars)
  expect_identical(barsColNames, c("Volume", "Close", "Low", "High", "Open", "Timestamp"))
})

rhost <- InitRTTWebApiHost(server = "ttlivewebapi.fxopen.com")
test_that("Is Last 10 Bars right format", {
  # skip_on_cran()
  # if(requireNamespace("lubridate", quietly = TRUE)){
  #   reqTime <- lubridate::now("UTC")
  # }else{
  #   reqTime <- 0
  # }
  vcr::use_cassette("Rhost_last10M1Bars", {
    bars <- rhost$GetBarsHistory("EURUSD", "Bid", "M1", reqTime, reqTime, count = -10)
  })
  expect_equal(typeof(bars), "list")
  # expect_true(is.data.table(bars))
  barsColNames <- colnames(bars)
  expect_identical(barsColNames, c("Volume", "Close", "Low", "High", "Open", "Timestamp"))
})

test_that("Is Last 10 ticks right format", {
  # skip_on_cran()
  # if(requireNamespace("lubridate", quietly = TRUE)){
  #   reqTime <- lubridate::now("UTC")
  # }else{
  #   reqTime <- 0
  # }
  vcr::use_cassette("RHost_last10Ticks", {
    ticks <- rhost$GetTickHistory("EURUSD", reqTime, reqTime, count = -10)
  })
  expect_equal(typeof(ticks), "list")
  # expect_true(is.data.table(ticks))
  ticksColNames <- colnames(ticks)
  expect_identical(ticksColNames, c("Timestamp", "BidPrice", "BidVolume", "BidType","AskPrice","AskVolume", "AskType"))
})

