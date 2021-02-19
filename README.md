# RTTWebClient
This package works with WebAPi of TickTrader Server. 

# Prerequisites
This package use httr, jsonlite, data.table r libraries. Please install them before using.

# How to install RTTWebClient?
```
if(!require(devtools)) {install.packages("devtools"); library(devtools)}
if(require(RTTWebClient)) {detach("package:RTTWebClient", unload=TRUE); remove.packages("RTTWebClient")}
install_github("SoftFx/TTWebClient-R",ref = "dev")	 

```

# Examples
 You can use function ttInitialize to set server name and port:
1) cryptottlivewebapi.fxopen.net - TT Exchange FXOPEN WebAPI
2) ttlivewebapi.fxopen.com - TT Live WebAPI

Port is 8443 as default

To get Quotes, Bars, etc. info you should call function from R list object which was created by ttInitialize with the appropriate parameters. 
See code below


```
library(RTTWebClient)
publicClient <- InitPublicWebClient(server = "ttlivewebapi.fxopen.com")
symbols <- publicClient$GetSymbolsInfoFromWeb()
 
```
