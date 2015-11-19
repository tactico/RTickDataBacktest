#' Abstract Backtest
#'
#' Abstract Class used to build any type of backtest
#' The idea is that if a particular strategy requires several symbols then this 
#' backtester will provide synchronous data for each symbol
#' and terminate after any of the feeds end. If the user would like to run a backtest 
#' on many symbols then they should run multiple instances of this backtester.
#' If the user would like to allow missing values, or asynchronous bahaviour, then this 
#' class should be extended or duplicated and modified to preference.
#' 
#' @param start.date start date of the backtest
#' @param end.date end date of the backtest
#' @param history.buffer.size Number of past timestamps to keep in memory
#' @return .Object copy of the created object
Backtest <- setClass(
  "Backtest",

  slots = c(
    NAME = "character",
    start.date = "POSIXlt",
    end.date = "POSIXlt",
    cur.date = "POSIXlt",
    data = "ANY",#dictionary over symbols which point to a timeserires for each symbol (currently only one tickdata per symbol)
    positions = "ANY",
    time.started = "POSIXlt",
    time.finalized = "POSIXlt",
    start.session = "integer",
    end.session = "integer",
    trade.statistics = "ANY",#Use eventually
    equity.time = "ANY",
    equity.value = "ANY",
    instr = "ANY",
    symbols = "ANY",
    history.buffer.size = "integer",
    username = "character",
    password = "character"
  ),

  prototype = list(
    NAME = "Abstract Backtest",
    start.session = c(0L,0L),
    end.session = c(23L, 59L),
    data = list(),
    positions = list(),
    equity.time = list(),
    equity.value = list(),
    instr = list(),
    trade.statistics = list(),#Use eventually
    username = NULL,
    password = NULL
    )
)
setMethod("initialize", "Backtest", function(.Object, start.date, end.date, history.buffer.size=100L,username=NULL,password=NULL){
  .Object@time.started <- as.POSIXlt(Sys.time())
  .Object@start.date <- as.POSIXlt(start.date, format="%m/%d/%Y")
  .Object@end.date <- as.POSIXlt(end.date, format="%m/%d/%Y")
  .Object@history.buffer.size <- history.buffer.size
  if (!is.null(username) && !is.null(password)){
    .Object@username <- username
    .Object@password <- password
  }
  
  
  return(.Object)
})


#' Add TimeSeries Objects
#'
#' Adds any type of TimeSeries object that will be used in the backtest
#' @param Obj Backtest object
#' @param symbols Equity names
#' @param type currently only tickdata supported, but will extend to quote
#' @return Copy of the backtest object
#' @export
setGeneric(name="AddTimeSeries",def=function(Obj, symbols, granularity,type){standardGeneric("AddTimeSeries")})
setMethod(f="AddTimeSeries",
          signature=c("Backtest","character","integer","missing"),
          definition=function(Obj, symbols, granularity,type='tickdata')
          {
            #type does nothing right now
            datafetcher <- DataFetcherObject("tickdata", username, password)
            Obj@symbols <- symbols
            for (sym in symbols){
              data.files <- datafetcher$GetData(strsplit(sym,":")[[1]][1], strsplit(sym,":")[[1]][2], format(Obj@start.date,"%m/%d/%Y"), 
                                                format(Obj@end.date,"%m/%d/%Y"), granularity)
              #TODO
              #if length(Obj@data[[sym]]) == 0 then Obj@data[[sym]] <- list(type = new(...))
              Obj@data[[sym]] <- new("BarTimeSeries", data.files[1], Obj@history.buffer.size)
            }
            #Taking first timeseries object exchange
            Obj <- SetSessionTime(Obj,strsplit(symbols[[1]],":")[[1]][2])
            
            return(Obj)
          })

#' Get TimeSeries Object
#'
#' Gets the TimeSeries object from the backtest based on the equity name
#' @param Obj Backtest object
#' @param symbol Equity name
#' @return Copy of the TimeSeries object
#' @export
setGeneric(name="GetTimeSeries",def=function(Obj, symbol){standardGeneric("GetTimeSeries")})
setMethod(f="GetTimeSeries",
          signature=c("Backtest","character"),
          definition=function(Obj,symbol)
          {
            return(Obj@data[[symbol]])
          })

#' Set TimeSeries Object
#'
#' Set the TimeSeries object from the backtest based on the equity name
#' @param Obj Backtest object
#' @param symbol Equity name
#' @param TSobj
#' @return Copy of the object
#' @export
setGeneric(name="SetTimeSeries",def=function(Obj, symbol,timeseries){standardGeneric("SetTimeSeries")})
setMethod(f="SetTimeSeries",
          signature=c("Backtest","character","TimeSeries"),
          definition=function(Obj,symbol,timeseries)
          {
            Obj@data[[symbol]] <- timeseries
            return (Obj)
          })



#' Add Position Objects
#'
#' Adds a position object that will be used in the backtest
#' @param Obj Backtest object
#' @param symbol Equity name
#' @param pos Position object
#' @return Copy of the backtest object
#' @export
setGeneric(name="AddPositions",def=function(Obj, symbols){standardGeneric("AddPositions")})
setMethod(f="AddPositions",
          signature=c("Backtest","character"),
          definition=function(Obj, symbols)
          {
            equity <- 0
            for (sym in symbols){
              pos <- new("Position",sym)
              Obj@positions[[sym]] <- pos
              equity <- equity + GetRealized(Obj@positions[[sym]]) + GetUnrealized(Obj@positions[[sym]])
            }
            Obj@equity.time <- Obj@start.date
            Obj@equity.value <- equity
            return(Obj)
          })

#' Get Position Object
#'
#' Gets the Position object from the backtest based on the equity name
#' @param Obj Backtest object
#' @param symbol Equity name
#' @return Copy of the Position object
#' @export
setGeneric(name="GetPosition",def=function(Obj, symbol){standardGeneric("GetPosition")})
setMethod(f="GetPosition",
          signature = c("Backtest","character"),
          definition=function(Obj, symbol)
          {
            return(Obj@positions[[symbol]])
          })

#' Set Position Object
#'
#' Sets the Position object from the backtest based on the equity name
#' @param Obj Backtest object
#' @param symbol Equity name
#' @param position object
#' @return Copy of the  object
#' @export
setGeneric(name="SetPosition",def=function(Obj, symbol,pos){standardGeneric("SetPosition")})
setMethod(f="SetPosition",
          signature = c("Backtest","character","Position"),
          definition=function(Obj, symbol,pos)
          {
            Obj@positions[[symbol]] <- pos
            return( Obj)
          })

#' Set Market Session Time
#'
#' Sets the start and end time of the traded market using the Exchange name
#' Markets: Us:US Canada:TSX Australia:AU London:LN Korea:KS HongKong:HK
#'          Japan:JP Brazil:BZ Futures:FUT
#' @param Obj Backtest object
#' @param exchange Exchange name
#' @return Copy the Backtest object
#' @export
setGeneric(name="SetSessionTime",def=function(Obj,exchange){standardGeneric("SetSessionTime")})
setMethod(f="SetSessionTime",
          signature="Backtest",
          definition=function(Obj, exchange)
          {
            if(exchange == "US"){
              Obj@start.session <- c(9L, 30L)
              Obj@end.session  <-  c(16L, 0L)
            }else if(exchange == "TSX"){
              Obj@start.session <- c(9L, 30L)
              Obj@end.session  <-  c(16L, 0L)
            }else if(exchange == "AU"){
              Obj@start.session <- c(10L, 0L)
              Obj@end.session  <-  c(16L, 0L)
            }else if(exchange == "LN"){
              Obj@start.session <- c(8L, 0L)
              Obj@end.session  <-  c(16L, 30L)
            }else if(exchange == "JP"){
            }else if(exchange == "BZ"){
              Obj@start.session <- c(10L, 0L)
              Obj@end.session  <-  c(16L, 55L)
            }else if(exchange == "HK"){
              Obj@start.session <- c(9L, 30L)
              Obj@end.session  <-  c(16L, 0L)
            }else if(exchange == "IT"){
            }else if(exchange == "MM"){
            }else if(exchange == "KS"){
              Obj@start.session <- c(9L, 0L)
              Obj@end.session  <-  c(15L, 0L)
            }else if(exchange == "FUT"){
            }
            return(Obj)
          })

#' Is timestamp within trading hours
#'
#' @param Obj Backtest object
#' @param symbol timestamp
#' @return Bool
#' @export
setGeneric(name="inSession",def=function(Obj, timestamp){standardGeneric("inSession")})
setMethod(f="inSession",
          signature = c("Backtest","POSIXlt"),
          definition=function(Obj, timestamp)
          {
            CT <- as.numeric(format(timestamp, "%H")) + as.numeric(format(timestamp, "%M"))/60
            
            ST <- as.numeric(Obj@start.session[1]) + as.numeric(Obj@start.session[2])/60
            ET <- as.numeric(Obj@end.session[1]) + as.numeric(Obj@end.session[2])/60
            return ((CT>=ST)&&(CT<=ET))
          })

#'Synchronous timestamps among all symbols 
#' Report if there is a large gap
#'
#' @param Obj Backtest object
#' @return Time Stamp (POSIXlt)
#' @export
setGeneric(name="syncTimeStep",def=function(Obj){standardGeneric("syncTimeStep")})
setMethod(f="syncTimeStep",
          signature="Backtest",
          definition=function(Obj)
          {
            if (is.null(nextTimeStamp(Obj@data[[1]]))){
              inSessionBefore <- FALSE
            } else {
            inSessionBefore <- inSession(Obj,nextTimeStamp(Obj@data[[1]]))
            }
            #Peek all timeseries
            i <- 1L
            while (i <= length(Obj@data)){
              Peek(Obj@data[[i]])
              i <- i + 1L
            }
            #Synchronize to ts[1] (arbitrary)
            while (TRUE){
              i <- 2L
              while (i <= length(Obj@data)){
                if (nextTimeStamp(Obj@data[[i]]) > nextTimeStamp(Obj@data[[1]])){
                  Peek(Obj@data[[1]])
                  i <- 2L#back to start
                  next
                } else if (nextTimeStamp(Obj@data[[i]]) < nextTimeStamp(Obj@data[[1]])){
                  Peek(Obj@data[[i]])
                  i <- 2L
                  next
                }
                i <- i + 1L
              }
              break
            }
            inSessionAfter <- inSession(Obj,nextTimeStamp(Obj@data[[1]]))
            if (!inSessionBefore && inSessionAfter){#start trading day
              Obj <- StartSession(Obj)
            } else if (inSessionBefore && !inSessionAfter){#end trading day
              Obj <- EndSession(Obj)
            }
            return (nextTimeStamp(Obj@data[[1]]))
          })

#' Determines if any timeseries at last.item
#'
#' @param Obj Backtest object
#' @return Bool
#' @export
setGeneric(name="anyLastItem",def=function(Obj){standardGeneric("anyLastItem")})
setMethod(f="anyLastItem",
          signature="Backtest",
          definition=function(Obj)
          {
            #Peek all timeseries
            i <- 1L
            while (i <= length(Obj@data)){
              if (LastItem(Obj@data[[i]])){
                return (TRUE)
              }
              i <- i + 1L
            }
            return (FALSE)
          })


#' Start Backtest
#'
#' Starts Any Backtest Object
#' @param Obj Backtest object
#' @return Copy the Backtest object
#' @export
setGeneric(name="Start",def=function(Obj){standardGeneric("Start")})
setMethod(f="Start",
          signature="Backtest",
          definition=function(Obj)
          {
            Obj <- Initialize(Obj)#Will get called on Obj (specific class type)
            
            #Set start.date
            while ((tsi <- syncTimeStep(Obj)) < Obj@start.date){
              next
            }
            Obj@start.date <- tsi
            Obj@cur.date <- Obj@start.date
            while (!anyLastItem(Obj) && Obj@cur.date <= Obj@end.date){#this line enforces synchronous behaviour
              i <- 1L
              while (i <= length(Obj@data)) {
                GoTo(Obj@data[[i]])
                i <- i + 1L
              }
              if (inSession(Obj,Obj@cur.date)){
                Obj <- Evaluate(Obj)#If inside trading day then do strategy.
                Obj <- recordEquity(Obj,Obj@cur.date)
              }
              Obj@cur.date <- syncTimeStep(Obj)
            }
            Obj <- Finalize(Obj)
            Obj@time.finalized <- as.POSIXlt(Sys.time())
            print(paste("Started:",Obj@time.started," Finished:",Obj@time.finalized))
            plotEquity(Obj)
            return(Obj)
          })

#' Recording pnl
#'
#' @param Obj Backtest object
#' @return Object
#' @export
setGeneric(name="recordEquity",def=function(Obj,timestamp){standardGeneric("recordEquity")})
setMethod(f="recordEquity",
          signature=c("Backtest","POSIXlt"),
          definition=function(Obj,timestamp)
          { 
            equity <- 0
            i <- 1L
            while (i <= length(Obj@data)){
              equity <- equity + GetRealized(Obj@positions[[i]]) + GetUnrealized(Obj@positions[[i]])
              i <- i + 1L
            }
            Obj@equity.time <- c(Obj@equity.time, timestamp)
            Obj@equity.value <- c(Obj@equity.value, equity)
            return(Obj) 
          })

#' Plot Equity Curve
#'
#' @param Obj Backtest object
#' @return Object
#' @export
setGeneric(name="plotEquity",def=function(Obj){standardGeneric("plotEquity")})
setMethod(f="plotEquity",
          signature="Backtest",
          definition=function(Obj)
          { 
            
              plot(Obj@equity.time,Obj@equity.value,type='l',main=paste('Equity Curve for: ',Obj@NAME),xlab='Date',ylab='Realized + Unrealized Gains')

          })



#' Initialize Backtest
#'
#' Initializes Any Backtest Object
#' @param Obj Backtest object
#' @return Copy the Backtest object
#' @export
setGeneric(name="Initialize",def=function(Obj){standardGeneric("Initialize")})
setMethod(f="Initialize",
          signature="Backtest",
          definition=function(Obj)
          {
            return(Obj)
          })

#' Evaluate Backtest
#'
#' Evaluates Any Backtest Object
#' @param Obj Backtest object
#' @return Copy the Backtest object
#' @export
setGeneric(name="Evaluate",def=function(Obj){standardGeneric("Evaluate")})

#' Finalize Backtest
#'
#' Finalizes Any Backtest Object
#' @param Obj Backtest object
#' @return Copy the Backtest object
#' @export
setGeneric(name="Finalize",def=function(Obj){standardGeneric("Finalize")})
setMethod(f="Finalize",
          signature="Backtest",
          definition=function(Obj)
          {
            return(Obj)
          })

#' Starts Market Session
#'
#' Simulates the start of the trading day
#' @param Obj Backtest object
#' @return Copy the Backtest object
#' @export
setGeneric(name="StartSession",def=function(Obj){standardGeneric("StartSession")})
setMethod(f="StartSession",
          signature="Backtest",
          definition=function(Obj)
          {
            return(Obj)
          })

#' Ends Market Session
#'
#' Simulates the end of the trading day
#' @param Obj Backtest object
#' @return Copy the Backtest object
#' @export
setGeneric(name="EndSession",def=function(Obj){standardGeneric("EndSession")})
setMethod(f="EndSession",
          signature="Backtest",
          definition=function(Obj)
          {
            return(Obj)
          })
