#' QuoteTimeSeries Class
#'
#' Used to get Quote data out of a csv file
#' @param filepath Filepath of csv file containing the data
#' @return .Object Copy of the created object
#' @export
QuoteTimeSeries <- setClass(
  "QuoteTimeSeries",

  slots = c(
    BUFFER.SIZE = "integer",
    SUB.BUFFER.SIZE = "integer",
    my.bid.price = "numeric",
    my.ask.price = "numeric",
    my.bid.size = "integer",
    my.ask.size = "integer",
    my.exchange = "character",
    my.bid.exchange = "character",
    my.ask.exchange = "character",
    my.market.maker.id = "character",
    my.quote.condition = "character",
    my.nbboluld = "character"
  ),

  prototype = list(
    BUFFER.SIZE = 1024L,
    SUB.BUFFER.SIZE = 100L,
    my.bid.price = c(),
    my.ask.price = c(),
    my.bid.size = c(),
    my.ask.size = c(),
    my.exchange = c(),
    my.bid.exchange = c(),
    my.ask.exchange = c(),
    my.market.maker.id = c(),
    my.quote.condition = c(),
    my.nbboluld = c()
  ),

  contains = "TimeSeries"
)
setMethod("initialize", "QuoteTimeSeries", function(.Object, filepath){
  .Object <- callNextMethod(.Object, filepath)

  lines1 <- readLines(.Object@my.line.number.reader, n=(.Object@BUFFER.SIZE + 1))
  lines <- strsplit(lines1, ',')
  lines<- do.call(rbind,lines)

  if(nchar(lines[2,1]) == 10){
    .Object@my.format <- '%m/%d/%Y'
  }else{
    .Object@my.format <- '%m/%d/%Y %H:%M:%S'
  }

  .Object@my.timestamp <- as.POSIXlt(lines[2:nrow(lines),1], format= .Object@my.format)
  .Object@my.bid.size <- as.integer(lines[2:nrow(lines),2])
  .Object@my.bid.price <- as.numeric(lines[2:nrow(lines),3])
  .Object@my.ask.size <- as.integer(lines[2:nrow(lines),4])
  .Object@my.ask.price <- as.numeric(lines[2:nrow(lines),5])
  .Object@my.exchange <- lines[2:nrow(lines),6]
  .Object@my.bid.exchange <- lines[2:nrow(lines),7]
  .Object@my.ask.exchange <- lines[2:nrow(lines),8]

  if(length(lines1) < .Object@BUFFER.SIZE){
    close(.Object@my.line.number.reader)
    .Object@my.last.item <- .Object@my.timestamp[length(.Object@my.timestamp)]
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
            #we move the last x data items to the top of the array as we load more data in
            Obj@my.timestamp <- Obj@my.timestamp[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]
            Obj@my.bid.size <- Obj@my.bid.size[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]
            Obj@my.bid.price <- Obj@my.bid.price[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]
            Obj@my.ask.size <- Obj@my.ask.size[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]
            Obj@my.ask.price <- Obj@my.ask.price[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]
            Obj@my.exchange <- Obj@my.exchange[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]
            Obj@my.bid.exchange <- Obj@my.bid.exchange[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]
            Obj@my.ask.exchange <- Obj@my.ask.exchange[(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE+1):Obj@BUFFER.SIZE]

            #get more data
            lines1 <- readLines(Obj@my.line.number.reader, n=(Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE))
            lines <- strsplit(lines1, ',')
            lines<- do.call(rbind,lines)

            #add new data to previous x data items
            Obj@my.timestamp <- c(Obj@my.timestamp, as.POSIXlt(lines[1:nrow(lines),1], format= Obj@my.format))
            Obj@my.bid.size <- c(Obj@my.bid.size, as.integer(lines[1:nrow(lines),2]))
            Obj@my.bid.price <- c(Obj@my.bid.price, as.numeric(lines[1:nrow(lines),3]))
            Obj@my.ask.size <- c(Obj@my.ask.size, as.integer(lines[1:nrow(lines),4]))
            Obj@my.ask.price <- c(Obj@my.ask.price, as.numeric(lines[1:nrow(lines),5]))
            Obj@my.exchange <- c(Obj@my.exchange, lines[1:nrow(lines),6])
            Obj@my.bid.exchange <- c(Obj@my.bid.exchange, lines[1:nrow(lines),7])
            Obj@my.ask.exchange <- c(Obj@my.ask.exchange, lines[1:nrow(lines),8])

            if(length(lines1) < (Obj@BUFFER.SIZE-Obj@SUB.BUFFER.SIZE)){
              close(Obj@my.line.number.reader)
              Obj@my.last.item <- Obj@my.timestamp[length(Obj@my.timestamp)]

            }

            Obj@my.index <- Obj@SUB.BUFFER.SIZE
            return(Obj)
          })

#' Get Quote Bid Size
#' @param Obj QuoteTimeSeries object
#' @return Quote Bid Size
#' @export
setGeneric(name="QuoteBidSize", def=function(Obj){standardGeneric("QuoteBidSize")})
setMethod(f="QuoteBidSize", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.bid.size[Obj@my.index])})

#' Get Quote Bid Price
#' @param Obj QuoteTimeSeries object
#' @return Quote Bid Price
#' @export
setGeneric(name="QuoteBidPrice", def=function(Obj){standardGeneric("QuoteBidPrice")})
setMethod(f="QuoteBidPrice", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.bid.price[Obj@my.index])})

#' Get Quote Ask Size
#' @param Obj QuoteTimeSeries object
#' @return Quote Ask Size
#' @export
setGeneric(name="QuoteAskSize", def=function(Obj){standardGeneric("QuoteAskSize")})
setMethod(f="QuoteAskSize", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.ask.size[Obj@my.index])})

#' Get Quote Ask Price
#' @param Obj QuoteTimeSeries object
#' @return Quote Ask Price
#' @export
setGeneric(name="QuoteAskPrice", def=function(Obj){standardGeneric("QuoteAskPrice")})
setMethod(f="QuoteAskPrice", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.ask.price[Obj@my.index])})

#' Get Quote Exchange
#' @param Obj QuoteTimeSeries object
#' @return Quote Exchange
#' @export
setGeneric(name="QuoteExchange", def=function(Obj){standardGeneric("QuoteExchange")})
setMethod(f="QuoteExchange", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.exchange[Obj@my.index])})

#' Get Quote Bid Exchange
#' @param Obj QuoteTimeSeries object
#' @return Quote Bid Exchange
#' @export
setGeneric(name="QuoteBidExchange", def=function(Obj){standardGeneric("QuoteBidExchange")})
setMethod(f="QuoteBidExchange", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.bid.exchange[Obj@my.index])})

#' Get Quote Ask Exchange
#' @param Obj QuoteTimeSeries object
#' @return Quote Ask Exchange
#' @export
setGeneric(name="QuoteAskExchange", def=function(Obj){standardGeneric("QuoteAskExchange")})
setMethod(f="QuoteAskExchange", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.ask.exchange[Obj@my.index])})

#' Get Quote Market Maker ID
#' @param Obj QuoteTimeSeries object
#' @return Quote Market Maker ID
#' @export
setGeneric(name="QuoteMarketMakerID", def=function(Obj){standardGeneric("QuoteMarketMakerID")})
setMethod(f="QuoteMarketMakerID", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.market.maker.id[Obj@my.index])})

#' Get Quote Condition
#' @param Obj QuoteTimeSeries object
#' @return Quote Condition
#' @export
setGeneric(name="QuoteQuoteCondition", def=function(Obj){standardGeneric("QuoteQuoteCondition")})
setMethod(f="QuoteQuoteCondition", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.quote.condition[Obj@my.index])})

#' Get Quote Nbbo Luld
#' @param Obj QuoteTimeSeries object
#' @return Quote Nbbo Luld
#' @export
setGeneric(name="QuoteNbboLuld", def=function(Obj){standardGeneric("QuoteNbboLuld")})
setMethod(f="QuoteNbboLuld", signature="QuoteTimeSeries", definition=function(Obj){return (Obj@my.nbboluld[Obj@my.index])})



