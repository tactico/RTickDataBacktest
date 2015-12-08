#' Queue class
#'
#' Class serving perpose of a ring buffer of given type
#' Dynamic data stored in environment so in-place calls to class functions are used.
#' @param size Size of the ring buffer
#' @return .Object copy of the created object
Queue <- setClass(
  "Queue",
  
  slots = c(
    env = "ANY"
  ),
  
  prototype = list(
    env = NULL
  ),
  
  validity = function(object){
    return(TRUE)
  })

setMethod("initialize", "Queue", function(.Object, size){
  .Object@env <- new.env()
  .Object@env$count <- 0L
  .Object@env$size <- as.integer(size)
  .Object@env$front <- .Object@env$size-1L
  .Object@env$array <- c()
  return(.Object)
})

#' Read newest item in ringbuffer
#' @return most recently added item to ringbuffer
setGeneric(name="readFrontOfQueue",def=function(Obj){standardGeneric("readFrontOfQueue")})       
setMethod(f="readFrontOfQueue", signature="Queue", definition=function(Obj){
  return(Obj@env$array[Obj@env$front+1L])
})



#' Read in chronologal order the ringbuffer
#' @return ringbuffer including unfilled items
setGeneric(name="readFullQueue",def=function(Obj){standardGeneric("readFullQueue")})
setMethod("readFullQueue", "Queue", definition=function(Obj){
  next_ <- (Obj@env$count%%Obj@env$size)+1L
  if (next_ == 1L){
    return (Obj@env$array)
  }
  else {
    return (c(Obj@env$array[next_:Obj@env$size],Obj@env$array[1L:(Obj@env$front+1L)]))
  }
})

#' Add an item to the front of the queue
#'
#' Inplace class method, does not return a copy of the class
#' @param new.element Item to add
#' @return NULL
setGeneric(name="enqueue",def=function(Obj,...){standardGeneric("enqueue")})
setMethod("enqueue", "Queue", definition=function(Obj,new.element){
  Obj@env$front <- (Obj@env$count) %% Obj@env$size
  Obj@env$count <- Obj@env$count + 1L
  Obj@env$array[Obj@env$front+1L] <- new.element
})

#' Add in order a list of items to the front of the queue
#'
#' repeated calls to enqueue
#' @param new.elements Items to add
#' @return NULL
setGeneric(name="enqueueMany",def=function(Obj,...){standardGeneric("enqueueMany")})
setMethod("enqueueMany", "Queue", definition=function(Obj,new.elements){
  i <- 1L
  while (i <= length(new.elements)){
    enqueue(Obj,new.elements[i])
    i <- i + 1L
  }
})

#' FileBuffer class
#'
#' Class which handles reading parts of data file (so that an entire file need not be read in at once)
#' Stores dynamic variables in a class environment to allow inplace method calling.
#' @param filename Name of file (assumed to have header of one line)
#' @param linesPerBuffer Number of lines to store in buffer at any given time. 
#' @return .Object copy of the created object
fileBuffer <- setClass(
  "fileBuffer",
  
  slots = c(
    env = "ANY",
    filename = "character",
    lines.per.buffer = "integer",
    fullLines = "Queue"
  ),
  
  prototype = list(
    env = NULL,
    filename = "",
    lines.per.buffer = 1024L,
    fullLines = NULL
  ),
  
  validity = function(object){
    return(TRUE)
  })

setMethod("initialize", "fileBuffer", function(.Object, filename,linesPerBuffer){
  .Object@env <- new.env()
  .Object@env$pos <- 0L
  .Object@env$eof <- FALSE
  .Object@env$startIndex = 1L
  .Object@filename <- as.character(filename)#connection name
  .Object@fullLines <- Queue(as.integer(linesPerBuffer))
  .Object@lines.per.buffer <- as.integer(linesPerBuffer)
  .Object@env$con <- file(filename,"rb")
  return(.Object)
})

setGeneric(name="loadNextBuffer",def=function(Obj){standardGeneric("loadNextBuffer")})       
setMethod(f="loadNextBuffer", signature="fileBuffer", definition=function(Obj){
  #read lines without loading all file into memory

  con <-  Obj@env$con    #file(Obj@filename,"rb")#open binary
  seek(con,as.integer(Obj@env$pos))#To last position of read line (0 indexed)
  if(Obj@env$pos == 0L){
    lines <- readLines(con,n=Obj@lines.per.buffer+1)
    lines <- lines[2:length(lines)]
    enqueueMany(Obj@fullLines,lines)
  } else{
      enqueueMany(Obj@fullLines,lines <- readLines(con,n=Obj@lines.per.buffer))
  }
  Obj@env$pos <- as.integer(seek(con,where=0L,origin='current'))#new position (possibly EOF)
  if (length(lines) < Obj@lines.per.buffer){#finished file
    Obj@env$eof <- TRUE
    close(con)
  }
  Obj@env$startIndex <- Obj@lines.per.buffer - length(lines) + 1L
  #close(con)
})

setGeneric(name="isEof",def=function(Obj){standardGeneric("isEof")})       
setMethod(f="isEof", signature="fileBuffer", definition=function(Obj){
  #read lines without loading all file into memory
  return (Obj@env$eof)
})

setGeneric(name="readBuffer",def=function(Obj){standardGeneric("readBuffer")})       
setMethod(f="readBuffer", signature="fileBuffer", definition=function(Obj){

  return(readFullQueue(Obj@fullLines))
})


#' Abstract TimeSeries
#'
#' Abstract Class used to build any type of TimeSeries
#' @param filepath Filepath of csv file containing the data
#' @return .Object copy of the created object
TimeSeries <- setClass(
  "TimeSeries",

  slots = c(
    BUFFER.SIZE = "integer",
    timestamps = "Queue",
    format = "character",
    filepath = "character",
    env = "ANY"
  ),

  prototype = list(
    timestamps = NULL,
    format = '',
    filepath = '',
    env = NULL
  ),

  validity = function(object){
    return(TRUE)
  })
setMethod("initialize", "TimeSeries", function(.Object, filepath,BufferSize=1024L){
  if(is.null(filepath)){
    return(.Object)
  }
  .Object@BUFFER.SIZE <- BufferSize
  .Object@timestamps <- Queue(.Object@BUFFER.SIZE)
  .Object@env <- new.env()
  .Object@env$last.item <- NULL
  .Object@env$nextTimeStamp <- NULL
  return(.Object)
})

#' In-place call
#' Determine the next time stamp available 
#' User should then call nextTimeStamp to get this value
#'
#' Gets the next timestamp in the TimeSeries object
#' @param Obj TimeSeries object
#' @return NULL
#' @export
setGeneric(name="Peek",def=function(Obj){standardGeneric("Peek")})
setMethod(f="Peek",
          signature="TimeSeries",
          definition=function(Obj)
          {
            #Load must be defined in child class, e.g. BarTimeSeries
            Load(Obj)
          })

#' The next time stamp available 
#' User should check this before using GoTo to synchronize multiple data feeds
#'
#' Gets the next timestamp in the TimeSeries object
#' @param Obj TimeSeries object
#' @return POSIXlt
#' @export
setGeneric(name="nextTimeStamp",def=function(Obj){standardGeneric("nextTimeStamp")})
setMethod(f="nextTimeStamp",
          signature="TimeSeries",
          definition=function(Obj)
          {
            #User should check if this is good enough, then call goto to go to it.
            return (Obj@env$nextTimeStamp)
          })

#' Increments place in data file/feed and sets env$nextTimeStamp variable to next available in Queue
#' User then should call nextTimeStamp
#' 
#' @param Obj TimeSeries object
#' @return NULL
setGeneric(name="Load",def=function(Obj){standardGeneric("Load")})

#' Enqueues data corresponding to output of nextTimeStamp in queue
#' @param Obj TimeSeries object
#' @return NULL
#' @export
setGeneric(name="GoTo",def=function(Obj){standardGeneric("GoTo")})

#' is LastItem
#'
#' Checks if the data file/feed is at EOF
#' @param Obj TimeSeries object
#' @return TRUE/FALSE
#' @export
setGeneric(name="LastItem",def=function(Obj){standardGeneric("LastItem")})
setMethod(f="LastItem",
          signature="TimeSeries",
          definition=function(Obj)
          {
            return (!is.null(Obj@env$last.item))#This variable is NULL until Load function sets to a POSIXlt
          })

#' Get Timestamp
#'
#' Gets the current Timestamp that the TimeSeries object has loaded into the queue.
#' Differs from nextTimeStamp because user may not want to enqueue every timestamp
#' 
#' @param Obj TimeSeries object
#' @return Timestamp
#' @export
setGeneric(name="Timestamp",def=function(Obj){standardGeneric("Timestamp")})
setMethod(f="Timestamp",
          signature="TimeSeries",
          definition=function(Obj)
          {
            if (Obj@format == ''){return (NULL)} 
            return (as.POSIXlt(readFrontOfQueue(Obj@timestamps),format=Obj@format))
          })

#' Get Timestamps
#'
#' Gets the Timestamps that the TimeSeries object has loaded into the queue.
#' 
#' @param Obj TimeSeries object
#' @return Timestamps
#' @export
setGeneric(name="Timestamps",def=function(Obj){standardGeneric("Timestamps")})
setMethod(f="Timestamps",
          signature="TimeSeries",
          definition=function(Obj)
          {
            if (Obj@format == ''){return (NULL)} 
            return (as.POSIXlt(readFullQueue(Obj@timestamps),format=Obj@format))
          })

#' To String
#'
#'  Represents The timeseries data in a String
#' @param Obj TimeSeries object
#' @return data point in string representation
setGeneric(name="ToString",def=function(Obj){standardGeneric("ToString")})

#' Is Data Point Calculated
#'
#' Checks if TimeSeries object is from CSV file or if a UserDefined TimeSeries (Calculated)
#' @param Obj TimeSeries object
#' @return TRUE/FALSE
#' @export
setGeneric(name="IsCalculated", def=function(Obj){standardGeneric("IsCalculated")})
setMethod(f="IsCalculated", signature="TimeSeries", definition=function(Obj){return (FALSE)})