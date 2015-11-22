# This show how to make a backtester for tick data using the framework
# 
# Begin by loading the libraries and source code
# to install rJava run command install.package("rJava")
library("rJava")

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir('com.tactico.backtest/R/')

#Fill in:
username <- ""
password <- ""

#Bollinger Band Backtest is defined in  com.tactico.backtest/R/04BollingerBandBacktest.R
#Read there how to define your own strategy (Specifically the Evaluate method)

#Variables that the BB backtest needs (defined better in the source):
symbol <- "BAC:US"
start.date <- "01/03/2014"
end.date <- "01/03/2015"
granularity <- 15L #Minutes between ticks
period <- 20L#Number of day of history required to makes decisions at every bar
entry.thres <- 2L
exit.thres <- 1L
timeout <- 300L#After granularity*timeout minutes close positions if no signal
entry.mode <- TRUE#defined in source
shares <- 1000L#Shares to trade with


bollinger.band.backtest <- new("BollingerBandBacktest", symbol, start.date, end.date, granularity, period,
                               entry.thres , exit.thres, timeout, entry.mode, shares, username, password)

bollinger.band.backtest <- Start(bollinger.band.backtest)

