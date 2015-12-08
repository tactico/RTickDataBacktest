#' QuoteTimeSeries Class
#'
#' Used to get Quote data out of a csv file
#' @param filepath Filepath of csv file containing the data
#' @return .Object Copy of the created object
#' @export
QuoteTimeSeries <- setClass(
  "QuoteTimeSeries",

  slots = c(
    bid.price = "Queue",
    ask.price = "Queue",
    bid.size = "Queue",
    ask.size = "Queue",
    exchange = "Queue",
    bid.exchange = "Queue",
    ask.exchange = "Queue",
    market.maker.id = "Queue",
    quote.condition = "Queue",
    nbboluld = "Queue",
    fileBuffer = "fileBuffer"
  ),

  prototype = list(
    bid.price = NULL,
    ask.price = NULL,
    bid.size = NULL,
    ask.size = NULL,
    exchange = NULL,
    bid.exchange = NULL,
    ask.exchange = NULL,
    market.maker.id = NULL,
    quote.condition = NULL,
    nbboluld = NULL,
    fileBuffer = NULL
  ),

  contains = "TimeSeries"
)
setMethod("initialize", "QuoteTimeSeries", function(.Object, filepath,BufferSize=1024L){
  .Object <- callNextMethod(.Object, filepath, BufferSize)
  
  .Object@bid.price <- Queue(.Object@BUFFER.SIZE)
  .Object@ask.price <- Queue(.Object@BUFFER.SIZE)
  .Object@bid.size <- Queue(.Object@BUFFER.SIZE)
  .Object@ask.size <- Queue(.Object@BUFFER.SIZE)
  .Object@exchange <- Queue(.Object@BUFFER.SIZE)
  .Object@bid.exchange <- Queue(.Object@BUFFER.SIZE)
  .Object@ask.exchange <- Queue(.Object@BUFFER.SIZE)
  .Object@market.maker.id <- Queue(.Object@BUFFER.SIZE)
  .Object@quote.condition <- Queue(.Object@BUFFER.SIZE)
  .Object@nbboluld <- Queue(.Object@BUFFER.SIZE)
  
  .Object@fileBuffer <- fileBuffer(filepath,4096L)
  loadNextBuffer(.Object@fileBuffer)
  
  # Dynamic variables
  .Object@env$linesFromBuffer <- matrix(unlist(strsplit(readBuffer(.Object@fileBuffer), ',')), ncol = 8, byrow = TRUE)
  .Object@env$lineIndex <- (.Object@fileBuffer)@env$startIndex - 1L
  
  if(nchar(.Object@env$linesFromBuffer[2,1]) == 10){
    .Object@format <- '%m/%d/%Y'
  }else{
    .Object@format <- '%m/%d/%Y %H:%M:%S'
  }

  return(.Object)
})


#' Load more data
#'
#' Loads more data, when current data set has already been looped trough
#' @param Obj QuoteTimeSeries object
#' @return Copy of the QuoteTimeSeries object
#' @export
setMethod(f="Load",
          signature="QuoteTimeSeries",
          definition=function(Obj)
          {
            Obj@env$lineIndex <- Obj@env$lineIndex + 1L
            if (Obj@env$lineIndex > (Obj@fileBuffer)@lines.per.buffer){#load next chunk of file
              loadNextBuffer(Obj@fileBuffer)
              Obj@env$linesFromBuffer <- matrix(unlist(strsplit(readBuffer(Obj@fileBuffer), ',')), ncol = 8, byrow = TRUE)
              Obj@env$lineIndex <- (Obj@fileBuffer)@env$startIndex
              if (Obj@env$lineIndex > (Obj@fileBuffer)@lines.per.buffer){#could not load new data
                Obj@env$last.item <- as.POSIXlt(Obj@env$linesFromBuffer[(Obj@fileBuffer)@lines.per.buffer,1], format= Obj@format)
                Obj@env$lineIndex <- (Obj@fileBuffer)@env$startIndex - 1L
                #print (paste("Last item:",Obj@env$last.item))
              }
            }
            Obj@env$nextTimeStamp <- as.POSIXlt(Obj@env$linesFromBuffer[Obj@env$lineIndex,1], format= Obj@format)
          })

#' Add current timestamp to queue 
#' 
#' Presumably user has called nextTimeStamp() and agreed this is the right timestamp to go to
#' @param Obj BarTimeSeries object
#' @return NULL
#' @export
setMethod(f="GoTo",
          signature="QuoteTimeSeries",
          definition=function(Obj)
          {
            enqueue(Obj@timestamps, Obj@env$linesFromBuffer[Obj@env$lineIndex,1])
            enqueue(Obj@bid.size, as.integer(Obj@env$linesFromBuffer[Obj@env$lineIndex,2]))
            enqueue(Obj@bid.price, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,3]))
            enqueue(Obj@ask.size, as.integer(Obj@env$linesFromBuffer[Obj@env$lineIndex,4]))
            enqueue(Obj@ask.price, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,5]))
            enqueue(Obj@exchange, Obj@env$linesFromBuffer[Obj@env$lineIndex,6])
            enqueue(Obj@bid.exchange, Obj@env$linesFromBuffer[Obj@env$lineIndex,5])
            enqueue(Obj@ask.exchange, Obj@env$linesFromBuffer[Obj@env$lineIndex,6])
          })

#' Get Quote Bid Size
#' @param Obj QuoteTimeSeries object
#' @return Quote Bid Size
#' @export
setGeneric(name="QuoteBidSize", def=function(Obj){standardGeneric("QuoteBidSize")})
setMethod(f="QuoteBidSize", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@bid.size))})

setGeneric(name="QuoteBidSizes", def=function(Obj){standardGeneric("QuoteBidSizes")})
setMethod(f="QuoteBidSizes", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@bid.size))})


#' Get Quote Bid Price
#' @param Obj QuoteTimeSeries object
#' @return Quote Bid Price
#' @export
setGeneric(name="QuoteBidPrice", def=function(Obj){standardGeneric("QuoteBidPrice")})
setMethod(f="QuoteBidPrice", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@bid.price))})

setGeneric(name="QuoteBidPrices", def=function(Obj){standardGeneric("QuoteBidPrices")})
setMethod(f="QuoteBidPrices", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@bid.price))})

#' Get Quote Ask Size
#' @param Obj QuoteTimeSeries object
#' @return Quote Ask Size
#' @export
setGeneric(name="QuoteAskSize", def=function(Obj){standardGeneric("QuoteAskSize")})
setMethod(f="QuoteAskSize", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@ask.size))})

setGeneric(name="QuoteAskSizes", def=function(Obj){standardGeneric("QuoteAskSizes")})
setMethod(f="QuoteAskSizes", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@ask.size))})

#' Get Quote Ask Price
#' @param Obj QuoteTimeSeries object
#' @return Quote Ask Price
#' @export
setGeneric(name="QuoteAskPrice", def=function(Obj){standardGeneric("QuoteAskPrice")})
setMethod(f="QuoteAskPrice", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@ask.price))})

setGeneric(name="QuoteAskPrices", def=function(Obj){standardGeneric("QuoteAskPrices")})
setMethod(f="QuoteAskPrices", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@ask.price))})

#' Get Quote Exchange
#' @param Obj QuoteTimeSeries object
#' @return Quote Exchange
#' @export
setGeneric(name="QuoteExchange", def=function(Obj){standardGeneric("QuoteExchange")})
setMethod(f="QuoteExchange", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@exchange))})

setGeneric(name="QuoteExchanges", def=function(Obj){standardGeneric("QuoteExchanges")})
setMethod(f="QuoteExchanges", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@exchange))})

#' Get Quote Bid Exchange
#' @param Obj QuoteTimeSeries object
#' @return Quote Bid Exchange
#' @export
setGeneric(name="QuoteBidExchange", def=function(Obj){standardGeneric("QuoteBidExchange")})
setMethod(f="QuoteBidExchange", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@bid.exchange))})

setGeneric(name="QuoteBidExchanges", def=function(Obj){standardGeneric("QuoteBidExchanges")})
setMethod(f="QuoteBidExchanges", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@bid.exchange))})

#' Get Quote Ask Exchange
#' @param Obj QuoteTimeSeries object
#' @return Quote Ask Exchange
#' @export
setGeneric(name="QuoteAskExchange", def=function(Obj){standardGeneric("QuoteAskExchange")})
setMethod(f="QuoteAskExchange", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@ask.exchange))})

setGeneric(name="QuoteAskExchanges", def=function(Obj){standardGeneric("QuoteAskExchanges")})
setMethod(f="QuoteAskExchanges", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@ask.exchange))})

#' Get Quote Market Maker ID
#' @param Obj QuoteTimeSeries object
#' @return Quote Market Maker ID
#' @export
setGeneric(name="QuoteMarketMakerID", def=function(Obj){standardGeneric("QuoteMarketMakerID")})
setMethod(f="QuoteMarketMakerID", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@market.maker.id))})

setGeneric(name="QuoteMarketMakerIDs", def=function(Obj){standardGeneric("QuoteMarketMakerIDs")})
setMethod(f="QuoteMarketMakerIDs", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@market.maker.id))})

#' Get Quote Condition
#' @param Obj QuoteTimeSeries object
#' @return Quote Condition
#' @export
setGeneric(name="QuoteQuoteCondition", def=function(Obj){standardGeneric("QuoteQuoteCondition")})
setMethod(f="QuoteQuoteCondition", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@quote.condition))})

setGeneric(name="QuoteQuoteConditions", def=function(Obj){standardGeneric("QuoteQuoteConditions")})
setMethod(f="QuoteQuoteConditions", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@quote.condition))})

#' Get Quote Nbbo Luld
#' @param Obj QuoteTimeSeries object
#' @return Quote Nbbo Luld
#' @export
setGeneric(name="QuoteNbboLuld", def=function(Obj){standardGeneric("QuoteNbboLuld")})
setMethod(f="QuoteNbboLuld", signature="QuoteTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@nbboluld))})

setGeneric(name="QuoteNbboLulds", def=function(Obj){standardGeneric("QuoteNbboLulds")})
setMethod(f="QuoteNbboLulds", signature="QuoteTimeSeries", definition=function(Obj){return (readFullQueue(Obj@nbboluld))})



