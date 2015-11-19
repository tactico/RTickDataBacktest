#' BollingerBandBacktest Class
#'
#' Used to Backtest a Bollinger Band strategy with bar data
#' NOTE: Some variables may be legacy.
#' @param symbol Symbol
#' @param start.date Start Date
#' @param end.date End Date
#' @param granularity Granularity (in minutes, example: 15 = using 15 minute bars)
#' @param period Number of Bars used to create the Bollinger Bands
#' @param entry.thres Number of standard deviations used for entry signal
#' @param exit.thres Number of standard deviations used for exit signal
#' @param timeout Number of bars that if exeeded since trade entry, will cause the Trade to be exited
#' @param emode Entry mode (FALSE: enter position on signal, TRUE: enter position on next candle if candle is on uptick)
#' @param shares Quantity to trade
#' @param TickData username
#' @param TickData password
#' @return .Object Copy of the created object
#' @export
Backtest <- setClass(
  "BollingerBandBacktest",

  slots = c(
    NAME = "character",
    period = "integer",
    entry.thres = "numeric",
    exit.thres = "numeric",
    timeout = "integer",
    entry.mode = "logical",
    shares = "integer",
    duration = "integer",
    wait.uptick.short = "logical",
    wait.uptick.long = "logical",
    bar = "BarTimeSeries",
    symbol = "character",
    position = "Position",
    username = "character",
    password = "character"
  ),

  prototype = list(
    NAME = "BollingerBandBacktest"
  ),

  contains = "Backtest"
)
setMethod("initialize", "BollingerBandBacktest", function(.Object, symbol, start.date, end.date, granularity,
                                                          period, entry.thres , exit.thres, timeout, emode, shares, username, password){
  #Initialize the base backtest class
  #All BTs need this line and these variables fed in
  .Object <- callNextMethod(.Object, start.date, end.date, period,username,password)
  
  #Set timeseries and positions in backtester
  #A BTs require these lines, symbol can be a list of symbols eg. (BAC:US,MHD:US)
  .Object <- AddTimeSeries(.Object, symbol, granularity)
  .Object <- AddPositions(.Object, symbol)

  #This specific strategy's variables
  .Object@symbol <- symbol#We do BB test on one symbol
  .Object@period <- period#Period we require for BB calc (this sets static queue size)
  .Object@entry.thres <- entry.thres #BB param
  .Object@exit.thres <- exit.thres #BB param
  .Object@timeout <- timeout #BB param
  .Object@entry.mode <- emode #BB param
  .Object@shares <- shares #Number of shares to trade

  #Next thing user should call is Start() (Source in Backtest.R)
  #This will:
  #0. Initialize the BT
  #1. Check if timestamp is within market hour and that there is new data
  #2. If so then advance timestamp of all symbols (synchronously) and call Evaluate
  #3. Record the equity and go to 1.
  #4. Call Finalize and plot equity curve
  
  #Evaluate, which by default in the generic method does nothing.
  #Evaluate must be defined here in this class.
  return(.Object)
})



#' Initialize BollingerBandBacktest
#' @param Obj BollingerBandBacktest object
#' @return Copy the BollingerBandBacktest object
setMethod(f="Initialize",
          signature="BollingerBandBacktest",
          definition=function(Obj){
            Obj@duration <- 0L
            Obj@wait.uptick.short = FALSE
            Obj@wait.uptick.long = FALSE
            return (Obj)
          })

#' Evaluate BollingerBandBacktest
#' This method gets called at every in-market tick and the user
#' is able to pull all desired data to make a choice
#' Which he does in terms of placing trades.
#' 
#' @param Obj BollingerBandBacktest object
#' @return Copy the BollingerBandBacktest object
setMethod(f="Evaluate",
          signature="BollingerBandBacktest",
          definition=function(Obj){

            Obj@bar <- GetTimeSeries(Obj,Obj@symbol)# BarTS for our symbol
            Obj@position <- GetPosition(Obj,Obj@symbol)# Position object for our symbol
            
            #Have we waited long enough to fill queues? Queue are of length given when we initialized parent Backtest object
            if (!isFilled(Obj@bar)){
              return (Obj)
            }

            if(Volume(Obj@bar) <= 0  || Close(Obj@bar) <= 0){#Ticks with zero volume or price? Skip
              print(paste("Skipped:",ToString(Obj@bar)))
              return(Obj)
            }
            
            #We can get lists of historical ticks ordered from oldest to newest data
            #They are of length given when we initialized parent Backtest object
            #If we haven't advanced far enough to fill the queue then list contains NAs e.g. (na,na,1,2,3)
            closes <- Closes(Obj@bar) 
            #Timestamps, Opens, Highs, Lows, and Volumes can be loaded similarly.
            #timestamps <- Timestamps(Obj@bar) #Maybe want try a time weighting?
            
            #We can also get the most recent Close, Open, High, Low, etc.
            #'Close(bar)' returns the same things as 'tail(Closes(bar),n=1)'
            #i.e. the last item in the queue is the newest
            
            #typical <- Open(Obj@bar) #Information we should have in reality but performs worse than below 
            typical <- (High(Obj@bar) + Open(Obj@bar) + Low(Obj@bar))/3 #Typical price within bar for current data point
            
            maind <- mean(c(closes[1:Obj@period-1],typical))#We choose to replace the last point with typical
            varstdev <- sd(c(closes[1:Obj@period-1],typical))
            
            #Calculate BBs
            upper.band.entry <- maind + (Obj@entry.thres * varstdev)
            upper.band.exit <- maind + (Obj@exit.thres * varstdev)
            lower.band.entry <- maind - (Obj@entry.thres * varstdev)
            lower.band.exit <- maind - (Obj@exit.thres * varstdev)
            
            #We update the unrealized gains of our position before doing any action
            Obj@position <- CalcPl(Obj@position, Close(Obj@bar))
            
            cur.time.stamp <- Timestamp(Obj@bar)#This is the timestamp of the last tick
            
            #Standard BB now.
            #If we have any shares 'GetPositionSize(Obj@position) > 0' and price above exit, then place a Trade
            if(GetPositionSize(Obj@position) > 0 && Close(Obj@bar) >= lower.band.exit){
              #Note: At the moment placing a trade guarantees it goes through at the specified price which may not reflect real life
              #Here we chose whatever the close is.
              #but with a bid-ask spread it might be lower for sells (and higher for buys) of longs.
              #Feel free to do implement that in your strategy :)
              Obj@position <- Trade(Obj@position, cur.time.stamp, -1L * GetPositionSize(Obj@position), Close(Obj@bar))
              Obj@duration <- 0L#If this parameter gets too large we exit our position, see below.
              print(paste("(Exit long)  ", cur.time.stamp,"   lower.band.exit: " , lower.band.exit , "price: " , Close(Obj@bar),
                          ", pnl: ", GetRealized(Obj@position) + GetUnrealized(Obj@position)))
            }
            
            if(GetPositionSize(Obj@position) < 0 && Close(Obj@bar) <= upper.band.exit){
              Obj@position <- Trade(Obj@position, cur.time.stamp, -1L * GetPositionSize(Obj@position), Close(Obj@bar))
              Obj@duration <- 0L
              print(paste("(Exit short)  ", cur.time.stamp,"   upper.band.exit: " , upper.band.exit , "price: " , Close(Obj@bar),", pnl: ",
                          GetRealized(Obj@position) + GetUnrealized(Obj@position)))
            }
            #As a test to yourself, see if you can understand what wait.uptick.short does.
            if(GetPositionSize(Obj@position) == 0 && Close(Obj@bar) >= upper.band.entry){
              if(Obj@entry.mode && !Obj@wait.uptick.short){
                Obj@wait.uptick.short <- TRUE
              }else{
                Obj@position <- Trade(Obj@position, cur.time.stamp, -1L * Obj@shares, Close(Obj@bar))
                Obj@wait.uptick.short <- FALSE
                print(paste("(Enter short)  ", cur.time.stamp,"   upper.band.entry: " , upper.band.entry , "price: " , Close(Obj@bar),", pnl: ",
                            GetRealized(Obj@position) + GetUnrealized(Obj@position)))
              }
            }

            if(GetPositionSize(Obj@position) == 0 && Close(Obj@bar) <= lower.band.entry){
              if(Obj@entry.mode&& !Obj@wait.uptick.long){
                Obj@wait.uptick.long <- TRUE
              }else{
                Obj@position <- Trade(Obj@position, cur.time.stamp, Obj@shares, Close(Obj@bar))
                Obj@wait.uptick.long <- FALSE
                print(paste("(Enter long)  ", cur.time.stamp,"   lower.band.entry: " , lower.band.entry , "price: " , Close(Obj@bar),
                            ", pnl: ", GetRealized(Obj@position) + GetUnrealized(Obj@position)))
              }
            }
            
            if(GetPositionSize(Obj@position) != 0){
              Obj@duration <- Obj@duration + 1L
            }
            #Exit after a number of ticks
            if(Obj@duration > Obj@timeout){
              Obj@position <- Trade(Obj@position, cur.time.stamp, -1L * GetPositionSize(Obj@position), Close(Obj@bar))
              Obj@duration <- 0L
            }
            #Tell base backtester of your positions and timeseries if you've made adjustments to them (best practice to always do this)
            Obj<-SetPosition(Obj,Obj@symbol,Obj@position)
            Obj<-SetTimeSeries(Obj,Obj@symbol,Obj@bar)
            return (Obj)
          })


#' Finalize BollingerBandBacktest
#' @param Obj BollingerBandBacktest object
#' @return Copy the BollingerBandBacktest object
setMethod(f="Finalize",
          signature="BollingerBandBacktest",
          definition=function(Obj){
            #Obj@position <- Trade(Obj@position, Timestamp(Obj@bar), -1L * GetPositionSize(Obj@position), Close(Obj@bar))
            print( paste("date: ", Obj@equity.time[[1]][length(Obj@equity.time)]," PnL: ", Obj@equity.value[[1]][length(Obj@equity.value)]))
            print( paste("pnl: ", GetRealized(Obj@position) + GetUnrealized(Obj@position)))
            return (Obj)
          })
            
#' Starts Market Session
#'
#' Simulates the start of the trading day
#' @param Obj BollingerBandBacktest object
#' @return Copy the BollingerBandBacktest object
#' @export
setGeneric(name="StartSession",def=function(Obj){standardGeneric("StartSession")})
setMethod(f="StartSession",
          signature="BollingerBandBacktest",
          definition=function(Obj)
          {
            print('Start of day!')
            return(Obj)
          })

#' Ends Market Session
#'
#' Simulates the end of the trading day
#' @param Obj BollingerBandBacktest object
#' @return Copy the BollingerBandBacktest object
#' @export
setGeneric(name="EndSession",def=function(Obj){standardGeneric("EndSession")})
setMethod(f="EndSession",
          signature="BollingerBandBacktest",
          definition=function(Obj)
          {
            print('End of day!')
            return(Obj)
          })            

