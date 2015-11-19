#' BarTimeSeries Class
#'
#' Used to build Bars out of a csv file
#' @param filepath Filepath of csv file containing the data
#' @param BufferSize Number of timestamps to keep in queue
#' @return .Object Copy of the created object
#' @export
BarTimeSeries <- setClass(
  "BarTimeSeries",

  slots = c(
    open = "Queue",
    high = "Queue",
    low = "Queue",
    close = "Queue",
    volume = "Queue",
    fileBuffer = "fileBuffer"
  ),

  prototype = list(#queues, and data (better be enough RAM)
    open = NULL,
    high = NULL,
    low = NULL,
    close = NULL,
    volume = NULL,
    fileBuffer = NULL
  ),

  validity = function(object){return(TRUE)},

  contains = "TimeSeries")

setMethod("initialize", "BarTimeSeries", function(.Object, filepath,BufferSize=1024L){
  .Object <- callNextMethod(.Object, filepath,BufferSize)#call TimeSeries initializer
  
  .Object@open <- Queue(.Object@BUFFER.SIZE)
  .Object@high <- Queue(.Object@BUFFER.SIZE)
  .Object@low <- Queue(.Object@BUFFER.SIZE)
  .Object@close <- Queue(.Object@BUFFER.SIZE)
  .Object@volume <- Queue(.Object@BUFFER.SIZE)
  
  .Object@fileBuffer <- fileBuffer(filepath,4096L)
  loadNextBuffer(.Object@fileBuffer)
  
  # Dynamic variables
  .Object@env$linesFromBuffer <- matrix(unlist(strsplit(readBuffer(.Object@fileBuffer), ',')), ncol = 6, byrow = TRUE)
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
#' @param Obj BarTimeSeries object
#' @return Copy of the BarTimeSeries object
#' @export
setMethod(f="Load",
          signature="BarTimeSeries",
          definition=function(Obj)
          {
            Obj@env$lineIndex <- Obj@env$lineIndex + 1L
            if (Obj@env$lineIndex > (Obj@fileBuffer)@lines.per.buffer){#load next chunk of file
              loadNextBuffer(Obj@fileBuffer)
              Obj@env$linesFromBuffer <- matrix(unlist(strsplit(readBuffer(Obj@fileBuffer), ',')), ncol = 6, byrow = TRUE)
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
          signature="BarTimeSeries",
          definition=function(Obj)
          {
            enqueue(Obj@timestamps, Obj@env$linesFromBuffer[Obj@env$lineIndex,1])
            enqueue(Obj@open, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,2]))
            enqueue(Obj@high, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,3]))
            enqueue(Obj@low, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,4]))
            enqueue(Obj@close, as.numeric(Obj@env$linesFromBuffer[Obj@env$lineIndex,5]))
            enqueue(Obj@volume, as.integer(Obj@env$linesFromBuffer[Obj@env$lineIndex,6]))
          })

#' Filled
#' @return bool, if filled or not
setGeneric(name="isFilled",def=function(Obj){standardGeneric("isFilled")})       
setMethod(f="isFilled", signature="BarTimeSeries", definition=function(Obj){
  return (!(is.na(readFullQueue(Obj@close)[1]) || is.null(readFullQueue(Obj@close)[1])))
})


#' To String
#'
#' Represents The Bar data in a String
#' @param Obj BarTimeSeries object
#' @return Bar data point in string representation
#' @export
setMethod(f="ToString",
          signature="BarTimeSeries",
          definition=function(Obj)
          {
            return(paste("TimeStamp: ", Timestamp(Obj),",  Open: " , Open(Obj) ,
                         ",  High: " , High(Obj) , ",  Low: " , Low(Obj) ,
                         ",  Close: " , Close(Obj), ",  Volume: " , Volume(Obj) ))
          })


#' Get Open Price
#' @param Obj BarTimeSeries object
#' @return Open Price
#' @export
setGeneric(name="Open", def=function(Obj){standardGeneric("Open")})
setMethod(f="Open", signature="BarTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@open))})

#' Get Open Prices
#' @param Obj BarTimeSeries object
#' @return Open Prices
#' @export
setGeneric(name="Opens", def=function(Obj){standardGeneric("Opens")})
setMethod(f="Opens", signature="BarTimeSeries", definition=function(Obj){return (readFullQueue(Obj@open))})

#' Get High Price
#' @param Obj BarTimeSeries object
#' @return High Price
#' @export
setGeneric(name="High", def=function(Obj){standardGeneric("High")})
setMethod(f="High", signature="BarTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@high))})

#' Get High Prices
#' @param Obj BarTimeSeries object
#' @return High Prices
#' @export
setGeneric(name="Highs", def=function(Obj){standardGeneric("Highs")})
setMethod(f="Highs", signature="BarTimeSeries", definition=function(Obj){return (readFullQueue(Obj@high))})

#' Get Low Price
#' @param Obj BarTimeSeries object
#' @return Low Price
#' @export
setGeneric(name="Low", def=function(Obj){standardGeneric("Low")})
setMethod(f="Low", signature="BarTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@low))})

#' Get Low Prices
#' @param Obj BarTimeSeries object
#' @return Low Prices
#' @export
setGeneric(name="Lows", def=function(Obj){standardGeneric("Lows")})
setMethod(f="Lows", signature="BarTimeSeries", definition=function(Obj){return (readFullQueue(Obj@low))})

#' Get Close Price
#' @param Obj BarTimeSeries object
#' @return Close Price
#' @export
setGeneric(name="Close", def=function(Obj){standardGeneric("Close")})
setMethod(f="Close", signature="BarTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@close))})

#' Get Close Prices
#' @param Obj BarTimeSeries object
#' @return Close Prices
#' @export
setGeneric(name="Closes", def=function(Obj){standardGeneric("Closes")})
setMethod(f="Closes", signature="BarTimeSeries", definition=function(Obj){return (readFullQueue(Obj@close))})

#' Get Volume
#' @param Obj BarTimeSeries object
#' @return Volume
#' @export
setGeneric(name="Volume", def=function(Obj){standardGeneric("Volume")})
setMethod(f="Volume", signature="BarTimeSeries", definition=function(Obj){return (readFrontOfQueue(Obj@volume))})

#' Get Volumes
#' @param Obj BarTimeSeries object
#' @return Volumes
#' @export
setGeneric(name="Volumes", def=function(Obj){standardGeneric("Volumes")})
setMethod(f="Volumes", signature="BarTimeSeries", definition=function(Obj){return (readFullQueue(Obj@volume))})
