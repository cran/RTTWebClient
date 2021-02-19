## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(RTTWebClient)
library(lubridate)

## -----------------------------------------------------------------------------
ttWebClient <- InitPublicWebClient(server = "ttlivewebapi.fxopen.com")
#or use InitPrivateWebClient(server = "ttlivewebapi.fxopen.com", port = 8443, id = "", key = "", secret = "") to set a private connect. Need set HMAC id, key and secret

## -----------------------------------------------------------------------------
print(ttWebClient$GetDividendsFromWeb())

## -----------------------------------------------------------------------------
print(ttWebClient$GetSymbolsInfoFromWeb())

## -----------------------------------------------------------------------------
print(ttWebClient$GetBarFromWeb("EURUSD", "Bid","M1", round(as.double(now("UTC")) * 1000), count = -10))

## -----------------------------------------------------------------------------
print(ttWebClient$GetTicksFromWeb("EURUSD", round(as.double(now("UTC")) * 1000), count = -10))

## -----------------------------------------------------------------------------
ttWebApiHost <- InitRTTWebApiHost(server = "ttlivewebapi.fxopen.com")

## -----------------------------------------------------------------------------
print(ttWebApiHost$GetDividends())

## -----------------------------------------------------------------------------
print(ttWebApiHost$GetSymbolsInfo())

## -----------------------------------------------------------------------------
print(ttWebApiHost$GetBarsHistory("EURUSD", "Bid","M1", now("UTC") - days(1), now("UTC")))

## -----------------------------------------------------------------------------
print(ttWebApiHost$GetTickHistory("EURUSD",  now("UTC") - days(1), now("UTC")))

