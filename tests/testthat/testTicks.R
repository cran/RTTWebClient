ttPublicClient <- InitPublicWebClient(server = "ttlivewebapi.fxopen.com")
test_that("Is Last 10 Ticks right format", {
  if(requireNamespace("lubridate", quietly = TRUE)){
    reqTimeInMS <- round(as.double(lubridate::now("UTC")) * 1000)
  }else{
    reqTimeInMS <- 0
  }
  ticks <- ttPublicClient$GetTicksFromWeb("EURUSD", reqTimeInMS, count = -10)
  expect_equal(typeof(ticks), "list")
  # expect_true(is.data.table(ticks))
  ticksColNames <- colnames(ticks)
  expect_identical(ticksColNames, c("Timestamp", "BidPrice", "BidVolume", "BidType","AskPrice","AskVolume", "AskType"))

})

test_that("Is Last 10 Bars right format", {
  if(requireNamespace("lubridate", quietly = TRUE)){
    reqTimeInMS <- round(as.double(lubridate::now("UTC")) * 1000)
  }else{
    reqTimeInMS <- 0
  }
  bars <- ttPublicClient$GetBarFromWeb("EURUSD", "Bid", "M1", reqTimeInMS, count = -10)
  expect_equal(typeof(bars), "list")
  # expect_true(is.data.table(bars))
  barsColNames <- colnames(bars)
  expect_identical(barsColNames, c("Volume", "Close", "Low", "High", "Open", "Timestamp"))
})

rhost <- InitRTTWebApiHost()
test_that("Is Last 10 Bars right format", {
  if(requireNamespace("lubridate", quietly = TRUE)){
    reqTime <- lubridate::now("UTC")
  }else{
    reqTime <- 0
  }
  bars <- rhost$GetBarsHistory("EURUSD", "Bid", "M1", reqTime, reqTime, count = -10)
  expect_equal(typeof(bars), "list")
  # expect_true(is.data.table(bars))
  barsColNames <- colnames(bars)
  expect_identical(barsColNames, c("Volume", "Close", "Low", "High", "Open", "Timestamp"))
})

test_that("Is Last 10 ticks right format", {
  if(requireNamespace("lubridate", quietly = TRUE)){
    reqTime <- lubridate::now("UTC")
  }else{
    reqTime <- 0
  }
  ticks <- rhost$GetTickHistory("EURUSD", reqTime, reqTime, count = -10)
  expect_equal(typeof(ticks), "list")
  # expect_true(is.data.table(ticks))
  ticksColNames <- colnames(ticks)
  expect_identical(ticksColNames, c("Timestamp", "BidPrice", "BidVolume", "BidType","AskPrice","AskVolume", "AskType"))
})
