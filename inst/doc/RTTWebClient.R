## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
#  library(RTTWebClient)
#  library(lubridate)

## ---- eval=FALSE--------------------------------------------------------------
#  ttWebClient <- InitPublicWebClient(server = "ttlivewebapi.fxopen.com")
#  #or use InitPrivateWebClient(server = "ttlivewebapi.fxopen.com", port = 8443, id = "", key = "", secret = "") to set a private connect. Need set HMAC id, key and secret

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebClient$GetDividendsRawMethod())

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebClient$GetSymbolsInfoRawMethod())

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebClient$GetBarRawMethod("EURUSD", "Bid","M1", round(as.double(now("UTC")) * 1000), count = -10))

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebClient$GetTicksRawMethod("EURUSD", round(as.double(now("UTC")) * 1000), count = -10))

## ---- eval=FALSE--------------------------------------------------------------
#  ttWebApiHost <- InitRTTWebApiHost(server = "ttlivewebapi.fxopen.com")

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetDividends())

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetSymbolsInfo())

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetBarsHistory("EURUSD", "Bid","M1", now("UTC") - days(1), now("UTC")))

## ---- eval=FALSE--------------------------------------------------------------
#  print(ttWebApiHost$GetTickHistory("EURUSD",  now("UTC") - days(1), now("UTC")))

