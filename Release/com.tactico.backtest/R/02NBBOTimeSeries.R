#' NBBOTimeSeries Class
#'
#' Used to build NBBO data out of a Quote data csv file
#' @param filepath Filepath of csv file containing the Quote data
#' @return .Object Copy of the created object
#' @export
NBBOTimeSeries <- setClass(
  "NBBOTimeSeries",

  slots = c(
    #nbbo.timestamp = "POSIXlt",
    nbbo.bid.price = "Queue",
    nbbo.ask.price = "Queue",
    nbbo.bid.size = "Queue",
    nbbo.ask.size = "Queue",
    last.processed.timestamp = "character"
  ),

  prototype = list(
   # nbbo.timestamp = NULL,
    nbbo.bid.price = NULL,
    nbbo.ask.price = NULL,
    nbbo.bid.size = NULL,
    nbbo.ask.size = NULL,
    last.processed.timestamp = ''
  ),

  contains = "QuoteTimeSeries"
)
setMethod("initialize", "NBBOTimeSeries", function(.Object, filepath,BufferSize=1024L){
  .Object <- callNextMethod(.Object, filepath,BufferSize=1024L)
 # .Object@last.processed.timestamp <- Sys.time()
  .Object@env$bbos <- list()
  .Object@nbbo.bid.price <- Queue(.Object@BUFFER.SIZE)
  .Object@nbbo.ask.price <- Queue(.Object@BUFFER.SIZE)
  .Object@nbbo.bid.size <- Queue(.Object@BUFFER.SIZE)
  .Object@nbbo.ask.size <- Queue(.Object@BUFFER.SIZE)

  return(.Object)
})

#' Goes to Timestamp
#' @param Obj NBBOTimeSeries object
#' @param timestamp Timestamp to go to
#' @return Copy of the NBBOTimeSeries object
#' @export
setMethod(f="GoTo",
          signature="NBBOTimeSeries",
          definition=function(Obj)
          {
            #enqueue(Obj@timestamps, Obj@env$linesFromBuffer[Obj@env$lineIndex,1])
            enqueue(Obj@bid.size, as.integer(Obj@env$linesFromBuffer[Obj@env$lineIndex,2]))
            enqueue(Obj@bid.price, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,3]))
            enqueue(Obj@ask.size, as.integer(Obj@env$linesFromBuffer[Obj@env$lineIndex,4]))
            enqueue(Obj@ask.price, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,5]))
            enqueue(Obj@exchange, Obj@env$linesFromBuffer[Obj@env$lineIndex,6])
            enqueue(Obj@bid.exchange, Obj@env$linesFromBuffer[Obj@env$lineIndex,5])
            enqueue(Obj@ask.exchange, Obj@env$linesFromBuffer[Obj@env$lineIndex,6])
            #QuoteTimeSeries.GoTo(Obj)
            SetData(Obj, Obj@env$linesFromBuffer[Obj@env$lineIndex,1], QuoteExchange(Obj), QuoteBidPrice(Obj), QuoteAskPrice(Obj), QuoteBidSize(Obj), QuoteAskSize(Obj))
    
          })

#' Updates the NBBO
#' @param timestamp Timestamp to go to
#' @return Copy of the NBBOTimeSeries object
setGeneric(name="UpdateNbbo",
           def=function(Obj){
             standardGeneric("UpdateNbbo")
           })
setMethod(f="UpdateNbbo",
          signature="NBBOTimeSeries",
          definition=function(Obj)
          {
            bid.size <- 0L
            ask.size <- 0L
            bid.price <- 0.0
            ask.price <- 99999.9

            for(i in 1:length(Obj@env$bbos))
            {
              bbo <- Obj@env$bbos[[i]]

              if(bbo@bid.price > bid.price){
                bid.price <- bbo@bid.price
                bid.size <- bbo@bid.size
              }else if(bbo@bid.price == bid.price){
                bid.size <- bid.size + bbo@bid.size
              }

              if(bbo@ask.price > 0){
                if(bbo@ask.price < ask.price){
                  ask.price <- bbo@ask.price
                  ask.size <- bbo@ask.size
                }else if(bbo@ask.price == ask.price){
                  ask.size <- ask.size + bbo@ask.size
                }
              }
            }
            enqueue(Obj@timestamps, Obj@last.processed.timestamp)
            enqueue(Obj@nbbo.bid.size, as.integer(bid.size))
            enqueue(Obj@nbbo.bid.price, as.numeric(bid.price))
            enqueue(Obj@nbbo.ask.size, as.integer(ask.size))
            enqueue(Obj@nbbo.ask.price, as.numeric(ask.price))

          })

#' Add Quote Data
#'
#' Add Quote data (a single exchange's data information) to NBBO object
#' @param Obj NBBOTimeSeries object
#' @param timestamp Timestamp
#' @param exchange Exchange
#' @param bid.price Bid Price
#' @param ask.price Ask Price
#' @param bid.size Bid Size
#' @param ask.size Ask Size
#' @return Copy of the NBBOTimeSeries object
#' @export
setGeneric(name="SetData",
           def=function(Obj, timestamp, exchange, bid.price, ask.price, bid.size, ask.size){
             standardGeneric("SetData")
           })
setMethod(f="SetData",
          signature="NBBOTimeSeries",
          definition=function(Obj, timestamp, exchange, bid.price, ask.price, bid.size, ask.size)
          {
            #if(as.numeric(strftime(timestamp, format = "%y")) != as.numeric(strftime(Obj@last.processed.timestamp, format = "%y")) &&
             #  as.numeric(strftime(timestamp, format = "%j")) != as.numeric(strftime(Obj@last.processed.timestamp, format = "%j")) ){
              #Obj@env$bbos <- list()
            #}

            Obj@last.processed.timestamp <- timestamp
            if(is.null(Obj@env$bbos[[exchange]])){
              Obj@env$bbos[[exchange]] <- new("BBO", exchange)
            }

            bbo <- Obj@env$bbos[[exchange]]
            bp <- readFrontOfQueue(Obj@nbbo.bid.price) 
            ap <- readFrontOfQueue(Obj@nbbo.ask.price) 
            if (is.null(bp) || is.null(ap)){
              bp <- 0.
              ap <- 99999.
            }
            
            if(bbo@bid.price == bp || bbo@ask.price == ap ||
               bid.price >= bp || ask.price <= ap){
              Obj@env$bbos[[exchange]] <- SetBBO(bbo, bid.price, ask.price, bid.size, ask.size)
              Obj <- UpdateNbbo(Obj)#Only update if triggered
            }else{
              Obj@env$bbos[[exchange]] <- SetBBO(bbo, bid.price, ask.price, bid.size, ask.size)#
            }
          
          })

#' Get Bid Price
#' @param Obj NBBOTimeSeries object
#' @return Bid Price
#' @export
setGeneric(name="BidPrice", def=function(Obj){standardGeneric("BidPrice")})
setMethod(f="BidPrice", signature="NBBOTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@nbbo.bid.price))})

setGeneric(name="BidPrices", def=function(Obj){standardGeneric("BidPrices")})
setMethod(f="BidPrices", signature="NBBOTimeSeries", definition=function(Obj){return (readFullQueue(Obj@nbbo.bid.price))})

#' Get Ask Price
#' @param Obj NBBOTimeSeries object
#' @return Ask Price
#' @export
setGeneric(name="AskPrice", def=function(Obj){standardGeneric("AskPrice")})
setMethod(f="AskPrice", signature="NBBOTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@nbbo.ask.price))})

setGeneric(name="AskPrices", def=function(Obj){standardGeneric("AskPrices")})
setMethod(f="AskPrices", signature="NBBOTimeSeries", definition=function(Obj){return (readFullQueue(Obj@nbbo.ask.price))})

#' Get Bid Size
#' @param Obj NBBOTimeSeries object
#' @return Bid Size
#' @export
setGeneric(name="BidSize", def=function(Obj){standardGeneric("BidSize")})
setMethod(f="BidSize", signature="NBBOTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@nbbo.bid.size))})

setGeneric(name="BidSizes", def=function(Obj){standardGeneric("BidSizes")})
setMethod(f="BidSizes", signature="NBBOTimeSeries", definition=function(Obj){return (readFullQueue(Obj@nbbo.bid.size))})

#' Get Ask Size
#' @param Obj NBBOTimeSeries object
#' @return Ask Size
#' @export
setGeneric(name="AskSize", def=function(Obj){standardGeneric("AskSize")})
setMethod(f="AskSize", signature="NBBOTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@nbbo.ask.size))})

setGeneric(name="AskSizes", def=function(Obj){standardGeneric("AskSizes")})
setMethod(f="AskSizes", signature="NBBOTimeSeries", definition=function(Obj){return (readFullQueue(Obj@nbbo.ask.size))})


#' To String
#'
#' Represents The NBBO data in a String
#' @param Obj NBBOTimeSeries object
#' @return NBBO data point in string representation
#' @export
setMethod(f="ToString",
          signature="NBBOTimeSeries",
          definition=function(Obj)
          {
            return(paste("timeStamp: ", Timestamp(Obj),",  bidSize: " , BidSize(Obj) ,
                         ",  bidPrice: " , BidPrice(Obj) , ",  askPrice: " , AskPrice(Obj) ,
                         ",  askSize: " , AskSize(Obj) ))
          })



